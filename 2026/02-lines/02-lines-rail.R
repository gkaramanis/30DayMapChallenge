library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")
dir.create(dir_data, showWarnings = FALSE)

# Evaluate expr once and cache the result as rds in data/
cached <- function(name, expr) {
  path <- file.path(dir_data, paste0(name, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  saveRDS(expr, path)
  expr
}

# Län bounding box (xmin, ymin, xmax, ymax) in WGS84
bb_lan <- c(16.70, 59.30, 18.95, 60.75)

lan <- cached("boundary-uppsala_lan", {
  osmdata::opq(bb_lan, timeout = 300) |>
    osmdata::add_osm_feature("boundary", "administrative") |>
    osmdata::add_osm_feature("admin_level", "4") |>
    osmdata::add_osm_feature("name", "Uppsala län") |>
    osmdata::osmdata_sf() |>
    pluck("osm_multipolygons") |>
    select(name) |>
    st_make_valid()
}) |>
  st_transform(crs_se)

# Running track, without yards, sidings and spurs
rail <- cached("osm-rail-lan", {
  osmdata::opq(bb_lan, timeout = 600) |>
    osmdata::add_osm_feature("railway", "rail") |>
    osmdata::osmdata_sf()
})$osm_lines |>
  filter(!service %in% c("yard", "siding", "spur") | is.na(service)) |>
  select(osm_id) |>
  st_transform(crs_se)

# National feed (all operators), key from https://developer.trafiklab.se
se_zip <- file.path(dir_data, "sweden.zip")
if (!file.exists(se_zip)) {
  key <- Sys.getenv("TRAFIKLAB_GTFS_SWEDEN_KEY")
  download.file(
    paste0("https://opendata.samtrafiken.se/gtfs-sweden/sweden.zip?key=", key),
    se_zip, mode = "wb", headers = c("Accept-Encoding" = "gzip")
  )
}

# Passenger trains per day and operator between consecutive stations, both
# directions, for every train that calls at Uppsala C (stations 5 and 71218)
on <- "20261103"
sections <- cached(paste0("rail-sections-operator-uppsala-", on), {
  dir <- file.path(tempdir(), "gtfs-se")
  files <- c(
    "stop_times.txt", "trips.txt", "routes.txt", "stops.txt", "agency.txt",
    "calendar.txt", "calendar_dates.txt"
  )
  unzip(se_zip, files = files, exdir = dir)
  on.exit(unlink(dir, recursive = TRUE))

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  csv <- \(f) sprintf(
    "read_csv('%s', all_varchar = true, quote = '\"', header = true)",
    file.path(dir, f)
  )
  sql <- glue::glue("
    WITH svc AS (
      (SELECT service_id FROM {csv('calendar.txt')}
        WHERE tuesday = '1' AND start_date <= '{on}' AND end_date >= '{on}'
       UNION
       SELECT service_id FROM {csv('calendar_dates.txt')}
        WHERE date = '{on}' AND exception_type = '1')
      EXCEPT
      SELECT service_id FROM {csv('calendar_dates.txt')}
       WHERE date = '{on}' AND exception_type = '2'
    ),
    stations AS (
      SELECT stop_id, coalesce(nullif(parent_station, ''), stop_id) AS station
      FROM {csv('stops.txt')}
    ),
    trains AS (
      SELECT t.trip_id, a.agency_name FROM {csv('trips.txt')} t
      JOIN {csv('routes.txt')} r USING (route_id)
      JOIN {csv('agency.txt')} a USING (agency_id)
      WHERE (r.route_type = '2' OR r.route_type::INT BETWEEN 100 AND 117)
        AND t.service_id IN (SELECT service_id FROM svc)
    ),
    st AS (
      SELECT s.trip_id, trains.agency_name, s.stop_sequence::INT AS seq, x.station
      FROM {csv('stop_times.txt')} s
      JOIN trains USING (trip_id) JOIN stations x USING (stop_id)
    ),
    upp AS (SELECT DISTINCT trip_id FROM st WHERE station IN ('5', '71218')),
    legs AS (
      SELECT agency_name, station AS a,
        lead(station) OVER (PARTITION BY trip_id ORDER BY seq) AS b
      FROM st WHERE trip_id IN (SELECT trip_id FROM upp)
    )
    SELECT agency_name, least(a, b) AS a, greatest(a, b) AS b, count(*) AS trains
    FROM legs WHERE b IS NOT NULL AND a <> b
    GROUP BY ALL
  ")
  legs <- DBI::dbGetQuery(con, sql)

  stops <- read_csv(
    file.path(dir, "stops.txt"),
    col_types = cols(.default = "c"), col_select = c(stop_id, stop_name, stop_lat, stop_lon)
  ) |>
    filter(stop_id %in% c(legs$a, legs$b)) |>
    mutate(across(c(stop_lat, stop_lon), as.numeric))

  legs |>
    left_join(stops, by = join_by(a == stop_id)) |>
    left_join(stops, by = join_by(b == stop_id), suffix = c("_a", "_b")) |>
    as_tibble()
})

# Keep sections with both ends inside the OSM track extract, and drop those
# ending south-west of Uppsala, i.e. Mälarbanan from Stockholm to Västerås
in_extract <- \(lon, lat) between(lon, 16.55, 18.95) & between(lat, 59.28, 60.75)
on_malarbanan <- \(lon, lat) lon < 17.5 & lat < 59.8
operators <- c("SJ", "Mälartåg", "SL Pendeltåg")
sections <- sections |>
  filter(
    in_extract(stop_lon_a, stop_lat_a), in_extract(stop_lon_b, stop_lat_b),
    !on_malarbanan(stop_lon_a, stop_lat_a), !on_malarbanan(stop_lon_b, stop_lat_b)
  ) |>
  mutate(operator = case_when(
    str_detect(agency_name, "^SJ") ~ "SJ",
    str_detect(agency_name, "Mälartåg") ~ "Mälartåg",
    # The feed names SL Pendeltåg's Uppsala trains only "185"
    agency_name %in% c("185", "SL Pendeltåg") ~ "SL Pendeltåg",
    .unmatched = "error"
  ))

# Stockholm C is the southern end; Pendeltåg trains continue past it and
# stop at Stockholm City next door, which is drawn as Stockholm C
stockholm_c <- c(lon = 18.0580, lat = 59.3304)
sections <- sections |>
  filter(stop_lat_a >= 59.325, stop_lat_b >= 59.325) |>
  mutate(
    stop_lon_a =if_else(stop_name_a == "Stockholm City", stockholm_c[["lon"]], stop_lon_a),
    stop_lat_a = if_else(stop_name_a == "Stockholm City", stockholm_c[["lat"]], stop_lat_a),
    stop_lon_b = if_else(stop_name_b == "Stockholm City", stockholm_c[["lon"]], stop_lon_b),
    stop_lat_b = if_else(stop_name_b == "Stockholm City", stockholm_c[["lat"]], stop_lat_b)
  )

net <- rail |>
  st_cast("LINESTRING") |>
  sfnetworks::as_sfnetwork(directed = FALSE) |>
  tidygraph::convert(sfnetworks::to_spatial_subdivision) |>
  tidygraph::activate("edges") |>
  mutate(length = as.numeric(sfnetworks::edge_length()))

nodes <- st_as_sf(net, "nodes")
edges <- st_as_sf(net, "edges")
weights <- edges$length
nearest_node <- \(lon, lat) {
  st_as_sf(tibble(lon, lat), coords = c("lon", "lat"), crs = 4326) |>
    st_transform(crs_se) |>
    st_nearest_feature(nodes)
}
sections <- sections |>
  mutate(
    from = nearest_node(stop_lon_a, stop_lat_a),
    to = nearest_node(stop_lon_b, stop_lat_b)
  )
station_nodes <- unique(c(sections$from, sections$to))

# Split each section into the station-to-station segments it passes, so an
# express train shares one line with the stopping trains between the same
# stations
segment_trains <- pmap(sections, \(from, to, operator, trains, ...) {
  path <- igraph::shortest_paths(net, from, to, weights = weights)$vpath[[1]]
  stops <- intersect(as.integer(path), station_nodes)
  if (length(stops) < 2) return(NULL)
  tibble(
    a = pmin(head(stops, -1), tail(stops, -1)),
    b = pmax(head(stops, -1), tail(stops, -1)),
    operator, trains
  )
}) |>
  list_rbind() |>
  count(a, b, operator, wt = trains, name = "trains")

# One simplified line per segment. Lines run north on the main line (towards
# Uppsala from Stockholm, away from it towards Gävle) and west towards Sala,
# so "left" is the same side all the way along.
upp_node <- nearest_node(17.6477, 59.8586)
dist_upp <- igraph::distances(net, v = upp_node, weights = weights)[1, ]
upp_y <- st_coordinates(nodes[upp_node, ])[, "Y"]

segment_line <- \(a, b) {
  if (dist_upp[a] > dist_upp[b]) {
    tmp <- a
    a <- b
    b <- tmp
  }
  path <- igraph::shortest_paths(net, a, b, weights = weights, output = "epath")$epath[[1]]
  m <- st_coordinates(nodes[a, ])[, c("X", "Y"), drop = FALSE]
  for (e in as.integer(path)) {
    xy <- st_coordinates(st_geometry(edges)[e])[, c("X", "Y")]
    if (sum((xy[1, ] - m[nrow(m), ])^2) > sum((xy[nrow(xy), ] - m[nrow(m), ])^2)) {
      xy <- xy[nrow(xy):1, ]
    }
    m <- rbind(m, xy[-1, , drop = FALSE])
  }
  if (mean(m[, 2]) < upp_y - 2000) m <- m[nrow(m):1, ]
  m <- st_coordinates(st_simplify(st_sfc(st_linestring(m)), dTolerance = 150))[, c("X", "Y")]
  despike(m[c(TRUE, rowSums(diff(m)^2) > 0), , drop = FALSE])
}

# Drop vertices where the line doubles back, e.g. a detour onto the spur of
# a station platform, which an offset would turn into a loop
despike <- \(m) {
  repeat {
    if (nrow(m) < 3) return(m)
    d <- diff(m)
    d <- d / sqrt(rowSums(d^2))
    turn <- rowSums(d[-nrow(d), , drop = FALSE] * d[-1, , drop = FALSE])
    if (all(turn > -0.3)) return(m)
    m <- m[-(which.min(turn) + 1), , drop = FALSE]
  }
}

unit <- \(v) v / sqrt(sum(v^2))
segments <- distinct(segment_trains, a, b)
segments$m <- map2(segments$a, segments$b, segment_line)
segments <- segments |>
  mutate(
    start = map2_int(m, seq_along(m), \(m, i) {
      ends <- c(a[i], b[i])
      ends[which.min(map_dbl(ends, \(v) sum((st_coordinates(nodes[v, ])[1, 1:2] - m[1, ])^2)))]
    }),
    end = if_else(start == a, b, a),
    t_start = map(m, \(m) unit(m[2, ] - m[1, ])),
    t_end = map(m, \(m) unit(m[nrow(m), ] - m[nrow(m) - 1, ]))
  )

# At each station a segment shares its end direction with the segment that
# continues it most straight, so neighbouring strands meet without a kink
joined <- \(t, others) {
  if (length(others) == 0) return(t)
  dots <- map_dbl(others, \(o) sum(o * t))
  if (max(dots) < 0.3) return(t)
  unit(t + others[[which.max(dots)]])
}
segments <- segments |>
  mutate(
    t_end_j = pmap(list(end, t_end), \(v, t) joined(t, segments$t_start[segments$start == v])),
    t_start_j = pmap(list(start, t_start), \(v, t) joined(t, segments$t_end[segments$end == v]))
  )

# Shift a line sideways by d metres to the left, with given end directions
left <- \(t) c(-t[2], t[1])
offset_line <- \(m, d, t_start, t_end) {
  seg <- diff(m)
  n_seg <- cbind(-seg[, 2], seg[, 1]) / sqrt(rowSums(seg^2))
  n_vtx <- rbind(left(t_start), n_seg[-nrow(n_seg), , drop = FALSE] + n_seg[-1, , drop = FALSE], left(t_end))
  len <- sqrt(rowSums(n_vtx^2))
  # A vertex where the line turns back keeps its incoming normal
  back <- len < 1e-9
  n_vtx[back, ] <- rbind(n_seg[1, ], n_seg)[back, ]
  len[back] <- 1
  st_linestring(m + n_vtx / len * d)
}

spacing <- 1300
slot <- c("SJ" = 1, "Mälartåg" = 0, "SL Pendeltåg" = -1)

strands <- segment_trains |>
  left_join(select(segments, a, b, m, t_start_j, t_end_j), by = join_by(a, b)) |>
  mutate(geometry = st_sfc(
    pmap(list(m, operator, t_start_j, t_end_j), \(m, op, ts, te) {
      offset_line(m, slot[[op]] * spacing, ts, te)
    }),
    crs = crs_se
  )) |>
  select(operator, trains, geometry) |>
  st_as_sf() |>
  mutate(operator = factor(operator, levels = operators)) |>
  arrange(trains)

segments <- segments |>
  mutate(geometry = st_sfc(map(m, st_linestring), crs = crs_se)) |>
  select(a, b, geometry) |>
  st_as_sf()

# Each town is a bar across all three strands, at right angles to the track
# over the last kilometre into the station
towns <- tribble(
  ~name,       ~lon,    ~lat,
  "Uppsala",   17.6477, 59.8586,
  "Stockholm", 18.0580, 59.3304,
  "Gävle",     17.1469, 60.6756,
  "Sala",      16.6066, 59.9199,
  "Tierp",     17.5150, 60.3422,
  "Arlanda",   17.9293, 59.6497
)

bar_half <- 2 * spacing + 400
segment_ends <- unique(c(segments$a, segments$b))
town_bars <- pmap(towns, \(name, lon, lat) {
  pt <- st_as_sf(tibble(lon, lat), coords = c("lon", "lat"), crs = 4326) |>
    st_transform(crs_se)
  node <- segment_ends[st_nearest_feature(pt, nodes[segment_ends, ])]
  centre <- st_coordinates(nodes[node, ])[1, c("X", "Y")]
  seg <- segments |> filter(a == node | b == node) |> slice(1)
  m <- st_coordinates(seg)[, c("X", "Y")]
  if (sum((m[1, ] - centre)^2) > sum((m[nrow(m), ] - centre)^2)) m <- m[nrow(m):1, ]
  along <- cumsum(c(0, sqrt(rowSums(diff(m)^2))))
  ahead <- m[max(2, which.min(abs(along - 1000))), ]
  tangent <- ahead - centre
  normal <- c(-tangent[2], tangent[1]) / sqrt(sum(tangent^2))
  ends <- rbind(centre - normal * bar_half, centre + normal * bar_half)
  east <- ends[which.max(ends[, 1]), ]
  tibble(
    name,
    bar = st_sfc(st_linestring(ends), crs = crs_se),
    label = st_sfc(st_point(east + (east - centre) / bar_half * 700), crs = crs_se)
  )
}) |>
  list_rbind()
bars <- st_as_sf(select(town_bars, name, geometry = bar))
labels <- st_as_sf(select(town_bars, name, geometry = label))

view <- st_bbox(c(xmin = 16.55, ymin = 59.30, xmax = 18.40, ymax = 60.72), crs = 4326)

gg_record(
  here::here("30daymap-temp"),
  width = map_box$width, height = map_box$height, units = "in", dpi = 320
)

p <- ggplot() +
  geom_sf(data = lan, fill = clr_land, color = clr_grid, linewidth = 0.4) +
  geom_sf(data = rail, color = clr_grid, linewidth = 0.3) +
  geom_sf(
    data = strands, aes(linewidth = trains, color = operator),
    lineend = "round", linejoin = "round"
  ) +
  geom_sf(data = bars, color = clr_text, linewidth = 1.1, lineend = "round") +
  shadowtext::geom_shadowtext(
    data = labels, aes(label = name, geometry = geometry),
    stat = "sf_coordinates", family = f1, fontface = "bold", size = 5,
    hjust = 0, color = clr_text, bg.color = clr_bg
  ) +
  scale_color_manual(
    values = c("SJ" = clr_text2, "Mälartåg" = clr_accent2, "SL Pendeltåg" = clr_accent),
    drop = FALSE
  ) +
  scale_linewidth(range = c(0.3, 1.6), breaks = c(10, 40, 80)) +
  coord_map_box(view) +
  labs(color = NULL, linewidth = "Trains per day") +
  guides(color = guide_legend(override.aes = list(linewidth = 1.6), order = 1)) +
  theme_map()

save_map(
  p, here::here("2026/02-lines/02-lines-rail.png"),
  title = "Trains through Uppsala",
  subtitle = "Passenger trains per day that call at Uppsala C, \\ on Tuesday 3 November 2026",
  caption = caption_src("GTFS Sweden 3 via Trafiklab, OpenStreetMap")
)
