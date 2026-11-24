library(tidyverse)
library(sf)
library(duckdb)
library(sfnetworks)
library(tidygraph)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")
dir.create(dir_data, showWarnings = FALSE, recursive = TRUE)

# GTFS Sverige 2 from Trafiklab (needs an API key), 5 November 2026
gtfs_zip <- file.path(dir_data, "gtfs-sverige-2", "sweden-20261105.zip")

# Trafikverket's Järnvägsnät med grundegenskaper 3.0 from Lastkajen, in SWEREF 99 TM
rail_gpkg <- file.path(dir_data, "trv-jarnvagsnat", "Järnvägsnät_grundegenskaper3_0_GeoPackage.gpkg")

bb_lan <- c(16.70, 59.30, 18.95, 60.75)
uppsala_id <- "740000005"
stockholm_id <- "740000001"
pendel <- "Storstockholms Lokaltrafik"
max_minutes <- 60

# Evaluate expr once and cache the result as rds in data/
cached <- function(name, expr) {
  path <- file.path(dir_data, paste0(name, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  out <- expr
  saveRDS(out, path)
  out
}

osm_boundary <- function(name, admin_level, bb) {
  cached(paste0("boundary-", janitor::make_clean_names(name)), {
    osmdata::opq(bb, timeout = 300) |> osmdata::add_osm_feature("boundary", "administrative") |> osmdata::add_osm_feature("admin_level", admin_level) |> osmdata::add_osm_feature("name", name) |> osmdata::osmdata_sf() |> pluck("osm_multipolygons") |> select(name) |> st_make_valid()
  })
}

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

# Every stop within the hour on rail trips that call at Uppsala C, timed from the Uppsala departure, with the operator
legs_raw <- cached("rail-from-uppsala-20261105", {
  tmp <- tempfile()
  dir.create(tmp)
  unzip(gtfs_zip, files = c("stop_times.txt", "trips.txt", "routes.txt", "agency.txt"), exdir = tmp)
  con <- dbConnect(duckdb())
  q <- sprintf("
    WITH rail AS (
      SELECT t.trip_id, a.agency_name AS agency FROM read_csv_auto('%1$s/trips.txt', all_varchar = true) t
      JOIN read_csv_auto('%1$s/routes.txt', all_varchar = true) r USING (route_id)
      JOIN read_csv_auto('%1$s/agency.txt', all_varchar = true) a USING (agency_id)
      WHERE TRY_CAST(r.route_type AS INTEGER) BETWEEN 100 AND 199
    ),
    st AS (
      SELECT trip_id, stop_id, CAST(stop_sequence AS INTEGER) AS seq,
        TRY_CAST(split_part(arrival_time, ':', 1) AS INTEGER) * 60 + TRY_CAST(split_part(arrival_time, ':', 2) AS INTEGER) AS arr,
        TRY_CAST(split_part(departure_time, ':', 1) AS INTEGER) * 60 + TRY_CAST(split_part(departure_time, ':', 2) AS INTEGER) AS dep
      FROM read_csv_auto('%1$s/stop_times.txt', all_varchar = true)
      WHERE trip_id IN (SELECT trip_id FROM rail)
    ),
    up AS (SELECT trip_id, seq AS up_seq, dep AS up_dep FROM st WHERE stop_id = '%2$s')
    SELECT trip_id, agency, stop_id, seq, CASE WHEN seq = up_seq THEN 0 ELSE arr - up_dep END AS minutes
    FROM st JOIN up USING (trip_id) JOIN rail USING (trip_id)
    WHERE seq >= up_seq AND (seq = up_seq OR arr - up_dep <= %3$d)", tmp, uppsala_id, max_minutes)
  out <- dbGetQuery(con, q)
  dbDisconnect(con, shutdown = TRUE)
  unlink(tmp, recursive = TRUE)
  out
})

stops_raw <- cached("gtfs-stops-20261105", read_csv(unz(gtfs_zip, "stops.txt"), col_types = cols(.default = "c")))
stops <- stops_raw |> transmute(stop_id, label = stop_name |> str_replace(" Centralstation$", " C") |> str_remove(" station$"), lon = as.numeric(stop_lon), lat = as.numeric(stop_lat))

# Regular service only: stations with at least 20 trains in the feed, which drops Lennakatten and one-off trains
legs <- legs_raw |> as_tibble() |> arrange(trip_id, seq) |> filter(n_distinct(trip_id) >= 20, .by = stop_id)

# Median time on a single train, no changes
reach <- legs |> summarise(minutes = median(minutes), pendel_only = all(agency == pendel), .by = stop_id)

# End stations: the last stop within the hour on some train, and never a stop that a train passes on its way further out
last_stops <- legs |> slice_max(seq, by = trip_id) |> distinct(stop_id)
passed <- legs |> filter(seq < max(seq), .by = trip_id) |> distinct(stop_id)
ends <- anti_join(last_stops, passed, by = join_by(stop_id))

nodes <- reach |> inner_join(stops, by = join_by(stop_id)) |> mutate(end = stop_id %in% ends$stop_id) |> st_as_sf(coords = c("lon", "lat"), crs = 4326) |> st_transform(crs_se)

# Consecutive stops on the same train, one row per operator and station pair; every train travels away from Uppsala
pairs <- legs |> mutate(next_id = lead(stop_id), .by = trip_id) |> filter(!is.na(next_id)) |> distinct(agency, from = stop_id, to = next_id)

# Open main and diverging main tracks; diverging tracks at stations link the parallel main tracks
rail <- read_sf(rail_gpkg, wkt_filter = st_as_text(st_as_sfc(st_bbox(st_buffer(nodes, 10000))))) |> filter(SpTyp %in% c("nhsp", "ahsp"), Status == "Öppen") |> st_zm() |> select()

# Round to the meter so segment ends that miss by centimeters join
rail_rounded <- st_set_geometry(rail, st_sfc(map(st_geometry(rail), \(l) st_linestring(round(unclass(l)[, 1:2]))), crs = st_crs(rail)))
track <- as_sfnetwork(rail_rounded, directed = FALSE) |> convert(to_spatial_subdivision) |> activate("edges") |> mutate(weight = as.numeric(edge_length()))

# Each operator gets a fixed lane, in meters to the right of the direction of travel
lane <- c("Mälartåg" = -750, "SJ" = -250, "Snälltåget" = 250, "Storstockholms Lokaltrafik" = 750)

# Track coordinates from one station to the next, without repeated points
station_node <- setNames(st_nearest_feature(nodes, st_as_sf(track, "nodes")), nodes$stop_id)
track_xy <- st_coordinates(st_as_sf(track, "nodes"))[, 1:2]
route_xy <- function(a, b) {
  xy <- track_xy[as.integer(igraph::shortest_paths(track, station_node[[a]], station_node[[b]], weights = igraph::E(track)$weight, output = "vpath")$vpath[[1]]), , drop = FALSE]
  xy[c(TRUE, rowSums(abs(diff(xy))) > 0), , drop = FALSE]
}

# Move each vertex sideways, along the direction from five points back to five points ahead, so switches at the ends don't bend it
offset_xy <- function(xy, d) {
  i <- seq_len(nrow(xy))
  v <- xy[pmin(i + 5, nrow(xy)), , drop = FALSE] - xy[pmax(i - 5, 1), , drop = FALSE]
  xy + d * cbind(v[, 2], -v[, 1]) / sqrt(rowSums(v^2))
}

# A pair is redundant when the same operator's shorter hops cover the same track (within 1 % of its length)
is_redundant <- function(from, to, km) {
  g <- igraph::graph_from_data_frame(tibble(from, to, weight = km))
  map_lgl(seq_along(from), \(i) igraph::distances(igraph::delete_edges(g, i), from[i], to[i], mode = "out")[1, 1] <= km[i] * 1.01)
}

pairs_kept <- pairs |> mutate(km = map2_dbl(from, to, \(a, b) igraph::distances(track, station_node[[a]], station_node[[b]], weights = igraph::E(track)$weight)[1, 1] / 1000)) |> filter(!is_redundant(from, to, km), .by = agency)

operator_lines <- pairs_kept |> mutate(geometry = st_sfc(pmap(list(from, to, agency), \(a, b, op) st_linestring(offset_xy(route_xy(a, b), lane[[op]]))), crs = crs_se)) |> st_as_sf()

# Each station once per operator that stops there, where that operator's lines start or end
line_ends <- st_drop_geometry(operator_lines) |> mutate(first = map(st_geometry(operator_lines), \(l) head(unclass(l), 1)), last = map(st_geometry(operator_lines), \(l) tail(unclass(l), 1)))
lane_stations <- bind_rows(transmute(line_ends, agency, stop_id = from, xy = first), transmute(line_ends, agency, stop_id = to, xy = last)) |> summarise(x = mean(map_dbl(xy, 1)), y = mean(map_dbl(xy, 2)), .by = c(agency, stop_id)) |> st_as_sf(coords = c("x", "y"), crs = crs_se)

# OSM county clipped to GADM land, since the OSM boundary runs out to sea
land <- cached("gadm41-swe-1", read_sf("/vsizip//vsicurl/https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_SWE_1.json.zip")) |> st_transform(crs_se) |> st_make_valid() |> st_union()
county <- osm_boundary("Uppsala län", "4", bb_lan) |> st_transform(crs_se) |> st_intersection(land)

p <- ggplot() +
  geom_sf(data = county, fill = NA, color = clr_grid, linewidth = 0.6) +
  geom_sf(data = filter(operator_lines, agency != pendel), color = clr_text2, linewidth = 0.5, alpha = 0.7) +
  geom_sf(data = filter(operator_lines, agency == pendel), color = clr_accent, linewidth = 0.5, alpha = 0.7) +
  geom_sf(data = filter(lane_stations, agency != pendel), color = clr_text, size = 2, alpha = 0.7) +
  geom_sf(data = filter(lane_stations, agency == pendel), color = clr_accent, size = 2, alpha = 0.7) +
  ggrepel::geom_marquee_repel(data = filter(nodes, end | stop_id %in% c(uppsala_id, stockholm_id) | abs(minutes - 30) <= 2), aes(label = if_else(stop_id == uppsala_id, label, paste0("**", label, "** ", minutes, " min")), color = if_else(pendel_only, clr_accent, clr_text), geometry = geometry), stat = "sf_coordinates", family = f1, size = 4, bg.color = clr_bg, bg.r = 0.15, seed = 1) +
  scale_color_identity() +
  coord_map_box(st_buffer(nodes, 8000)) +
  theme_map()

save_map(
  p, here::here("2026/24-network/24-network-rail-times.png"),
  title = "Within an hour by train \\ from Uppsala C",
  subtitle = "Median minutes without changing trains. \\ Each operator is a line, commuter train in amber",
  caption = caption_src("Trafiklab, Trafikverket, OpenStreetMap, GADM")
)
