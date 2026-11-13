library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")
dir.create(dir_data, showWarnings = FALSE)

bb_city <- c(17.55, 59.80, 17.76, 59.90)

# Evaluate expr once and cache the result as rds in data/
cached <- function(name, expr) {
  path <- file.path(dir_data, paste0(name, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  saveRDS(expr, path)
  expr
}

# features: named list of key = values; each key is queried separately and
# the results combined. NA means any value.
osm_get <- function(name, bb, features) {
  cached(paste0("osm-", name), {
    res <- imap(features, function(value, key) {
      q <- osmdata::opq(bb, timeout = 600)
      q <- if (all(is.na(value))) osmdata::add_osm_feature(q, key) else osmdata::add_osm_feature(q, key, value)
      osmdata::osmdata_sf(q)
    })
    do.call(c, unname(res))
  })
}

osm_water <- function(bb = bb_city, name = "water-city") {
  w <- osm_get(name, bb, list(natural = "water", waterway = "river"))
  bind_rows(w$osm_polygons |> select(osm_id), w$osm_multipolygons |> select(osm_id), w$osm_lines |> filter(waterway == "river") |> st_transform(crs_se) |> st_buffer(8) |> st_transform(4326) |> select(osm_id)) |>
    st_transform(crs_se)
}

# Roads from the OSM extract that day 3's r5r setup downloads, wider than bb_city
road_types <- c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential", "living_street", "service", "motorway_link", "trunk_link", "primary_link", "secondary_link")

osm_roads_wide <- function() {
  cached("osm-roads-city-wide", st_read(file.path(dir_data, "r5", "uppsala.osm.pbf"), layer = "lines", query = sprintf("SELECT highway FROM lines WHERE highway IN (%s)", paste0("'", road_types, "'", collapse = ", ")), wkt_filter = st_as_text(st_as_sfc(st_bbox(c(xmin = 17.42, ymin = 59.79, xmax = 17.87, ymax = 59.93), crs = 4326))), quiet = TRUE)) |>
    st_transform(crs_se)
}

# Regional UL feed. Key from https://developer.trafiklab.se, stored in
# ~/.Renviron as TRAFIKLAB_GTFS_REGIONAL_KEY
ul_gtfs <- function() {
  path <- file.path(dir_data, "ul.zip")
  if (!file.exists(path)) {
    download.file(paste0("https://opendata.samtrafiken.se/gtfs/ul/ul.zip?key=", Sys.getenv("TRAFIKLAB_GTFS_REGIONAL_KEY")), path, mode = "wb", headers = c("Accept-Encoding" = "gzip"))
  }
  tidytransit::read_gtfs(path)
}

# service_ids running on a date, from calendar and calendar_dates
services_on <- function(gtfs, on) {
  day <- c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")[lubridate::wday(on, week_start = 1)]
  ids <- gtfs$calendar |> filter(start_date <= on, end_date >= on, .data[[day]] == 1) |> pull(service_id)
  cd <- gtfs$calendar_dates |> filter(date == on)
  union(setdiff(ids, cd$service_id[cd$exception_type == 2]), cd$service_id[cd$exception_type == 1])
}

# Position of every bus every step_s seconds: distance along the shape is
# linear in time between stop times (so a bus waits at a stop while
# arrival < departure), then mapped to x, y along the shape
bus_positions <- function(gtfs, on, step_s = 5) {
  trips <- gtfs$trips |>
    filter(service_id %in% services_on(gtfs, on)) |>
    left_join(gtfs$routes |> select(route_id, route_short_name, route_type), by = "route_id") |>
    filter(route_type == 700)

  shapes <- gtfs$shapes |>
    filter(shape_id %in% trips$shape_id) |>
    st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326) |>
    st_transform(crs_se)
  shape_xy <- tibble(shape_id = shapes$shape_id, d = shapes$shape_dist_traveled, x = st_coordinates(shapes)[, 1], y = st_coordinates(shapes)[, 2]) |>
    arrange(shape_id, d) |>
    nest(.by = shape_id, .key = "pts")

  timing <- gtfs$stop_times |>
    filter(trip_id %in% trips$trip_id, !is.na(shape_dist_traveled)) |>
    transmute(trip_id, d = shape_dist_traveled, arr = as.numeric(arrival_time), dep = as.numeric(departure_time)) |>
    pivot_longer(c(arr, dep), values_to = "t") |>
    arrange(trip_id, t, d) |>
    select(trip_id, t, d) |>
    nest(.by = trip_id, .key = "timing")

  trips |>
    select(trip_id, line = route_short_name, shape_id) |>
    inner_join(timing, by = "trip_id") |>
    inner_join(shape_xy, by = "shape_id") |>
    reframe(
      {
        tm <- timing[[1]]
        pts <- pts[[1]]
        t <- seq(min(tm$t), max(tm$t), by = step_s)
        d <- approx(tm$t, tm$d, t, ties = "ordered")$y
        tibble(t = t, x = approx(pts$d, pts$x, d, rule = 2, ties = "ordered")$y, y = approx(pts$d, pts$y, d, rule = 2, ties = "ordered")$y)
      },
      .by = c(trip_id, line)
    )
}


gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

on <- as.Date("2026-11-13")
cell_m <- 10
window_s <- 15

positions <- cached("ul-bus-positions-20261113", bus_positions(ul_gtfs(), on))

city <- st_bbox(st_transform(st_as_sfc(st_bbox(c(xmin = bb_city[1], ymin = bb_city[2], xmax = bb_city[3], ymax = bb_city[4]), crs = 4326)), crs_se))

# Meeting cells closer than this merge into one place
merge_m <- 100

# Positions every second, so a moving bus can't skip a small cell
positions_1s <- positions |>
  filter(between(x, city[["xmin"]] - 500, city[["xmax"]] + 500), between(y, city[["ymin"]] - 500, city[["ymax"]] + 500)) |>
  reframe(
    {
      t_1s <- seq(min(t), max(t))
      x_1s <- approx(t, x, t_1s)$y
      y_1s <- approx(t, y, t_1s)$y
      tibble(t = t_1s, x = x_1s, y = y_1s)
    },
    .by = c(trip_id, line)
  )

# Time each bus enters and leaves each cell
visits <- positions_1s |>
  filter(between(x, city[["xmin"]], city[["xmax"]]), between(y, city[["ymin"]], city[["ymax"]])) |>
  mutate(cx = floor(x / cell_m), cy = floor(y / cell_m)) |>
  arrange(trip_id, t) |>
  mutate(run = consecutive_id(cx, cy), .by = trip_id) |>
  summarise(t_in = min(t), t_out = max(t), .by = c(trip_id, line, run, cx, cy)) |>
  select(-run)

# Two buses on different lines in the same cell within window_s of each
# other. Each pair counts once, at the cell where they first meet.
first_meetings <- visits |>
  inner_join(visits |> transmute(cx, cy, trip_b = trip_id, line_b = line, t_in_b = t_in, lo = t_in - window_s, hi = t_out + window_s), by = join_by(cx, cy, t_in <= hi, t_out >= lo)) |>
  filter(line != line_b, trip_id < trip_b) |>
  slice_min(pmax(t_in, t_in_b), n = 1, with_ties = FALSE, by = c(trip_id, trip_b)) |>
  mutate(x = (cx + 0.5) * cell_m, y = (cy + 0.5) * cell_m)

# One place per cluster of meeting cells: cells within merge_m join up
meeting_pts <- first_meetings |>
  distinct(x, y) |>
  st_as_sf(coords = c("x", "y"), crs = crs_se, remove = FALSE)

cluster_polys <- meeting_pts |>
  st_buffer(merge_m / 2) |>
  st_union() |>
  st_cast("POLYGON")

clusters <- st_sf(place = seq_along(cluster_polys), geometry = cluster_polys)

places <- first_meetings |>
  left_join(meeting_pts |> st_join(clusters) |> st_drop_geometry(), by = join_by(x, y)) |>
  summarise(meetings = n(), x = mean(x), y = mean(y), .by = place) |>
  st_as_sf(coords = c("x", "y"), crs = crs_se) |>
  arrange(meetings)

busiest <- slice_max(places, meetings, n = 1) |>
  mutate(label = "Kungsgatan ×\nVaksalagatan")

view <- map_box_view(c(648300, 6637800), 11500)

roads <- osm_roads_wide() |> st_crop(view)
water <- osm_water() |> st_crop(view)

p <- ggplot() +
  geom_sf(data = water, fill = clr_water, color = NA) +
  geom_sf(data = roads, color = clr_road, linewidth = 0.2) +
  geom_sf(data = places, aes(size = meetings), shape = 21, fill = clr_accent2, color = clr_bg, stroke = 0.3, alpha = 0.85) +
  ggrepel::geom_text_repel(data = busiest, aes(label = label, geometry = geometry), stat = "sf_coordinates", family = f1, size = 3.8, color = clr_text, bg.color = clr_bg, bg.r = 0.12, nudge_x = -1600, nudge_y = 600, segment.color = clr_text2, segment.size = 0.3, seed = 2) +
  scale_size_area(max_size = 14, labels = scales::label_number(big.mark = " ")) +
  guides(size = guide_legend(title = "Meetings a day", override.aes = list(fill = clr_accent2, color = clr_bg))) +
  coord_map_box(view) +
  theme_map()

save_map(p, here::here("2026/13-interactions/13-interactions-bus-meetings.png"),
  title = "Uppsala's busiest bus \\ crossing is on Kungsgatan",
  subtitle = paste0("Buses on different lines within ", cell_m, " m \\ and ", window_s, " seconds of each other, ", str_squish(format(on, "%A %e %B %Y"))),
  caption = caption_src("GTFS Regional via Trafiklab, OpenStreetMap")
)
