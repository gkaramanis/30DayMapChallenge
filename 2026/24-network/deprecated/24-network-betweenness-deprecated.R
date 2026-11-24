library(tidyverse)
library(sf)
library(igraph)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")
dir.create(dir_data, showWarnings = FALSE, recursive = TRUE)

bb_kommun <- c(17.15, 59.60, 18.15, 60.20)

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
    osmdata::opq(bb, timeout = 300) |>
      osmdata::add_osm_feature("boundary", "administrative") |>
      osmdata::add_osm_feature("admin_level", admin_level) |>
      osmdata::add_osm_feature("name", name) |>
      osmdata::osmdata_sf() |>
      pluck("osm_multipolygons") |>
      select(name) |>
      st_make_valid()
  })
}

uppsala_kommun <- function() osm_boundary("Uppsala kommun", "7", bb_kommun)

# Keys from https://developer.trafiklab.se, stored in ~/.Renviron:
# TRAFIKLAB_GTFS_REGIONAL_KEY, TRAFIKLAB_GTFS_SWEDEN_KEY
gtfs_download <- function(url, file) {
  path <- file.path(dir_data, file)
  if (!file.exists(path)) {
    download.file(url, path, mode = "wb", headers = c("Accept-Encoding" = "gzip"))
  }
  path
}

ul_gtfs_path <- function() {
  key <- Sys.getenv("TRAFIKLAB_GTFS_REGIONAL_KEY")
  stopifnot("Set TRAFIKLAB_GTFS_REGIONAL_KEY" = key != "" || file.exists(file.path(dir_data, "ul.zip")))
  gtfs_download(paste0("https://opendata.samtrafiken.se/gtfs/ul/ul.zip?key=", key), "ul.zip")
}

ul_gtfs <- function() tidytransit::read_gtfs(ul_gtfs_path())

# service_ids running on a date, from calendar and calendar_dates
services_on <- function(gtfs, on) {
  day <- c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")[lubridate::wday(on, week_start = 1)]
  ids <- character()
  if (!is.null(gtfs$calendar) && nrow(gtfs$calendar) > 0) {
    ids <- gtfs$calendar |>
      filter(start_date <= on, end_date >= on, .data[[day]] == 1) |>
      pull(service_id)
  }
  if (!is.null(gtfs$calendar_dates)) {
    cd <- gtfs$calendar_dates |> filter(date == on)
    ids <- union(setdiff(ids, cd$service_id[cd$exception_type == 2]), cd$service_id[cd$exception_type == 1])
  }
  ids
}

# stop_times for trips on a date, with route info and departure in seconds
stop_times_on <- function(gtfs, on) {
  trips <- gtfs$trips |>
    filter(service_id %in% services_on(gtfs, on)) |>
    left_join(gtfs$routes |> select(route_id, route_short_name, route_type, any_of("agency_id")), by = "route_id")
  gtfs$stop_times |>
    inner_join(trips, by = "trip_id") |>
    mutate(dep_s = as.numeric(departure_time), arr_s = as.numeric(arrival_time))
}

stops_sf <- function(gtfs) {
  stops <- gtfs$stops
  if (!"parent_station" %in% names(stops)) stops$parent_station <- NA_character_
  stops |>
    filter(!is.na(stop_lat)) |>
    st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326, remove = FALSE)
}

# Bus route types: 3 and extended 700-799
is_bus <- function(route_type) route_type == 3 | between(route_type, 700, 799)


gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

gtfs <- ul_gtfs()
on <- as.Date("2026-11-24")
stopifnot("No service on this date" = length(services_on(gtfs, on)) > 0)

stops <- stops_sf(gtfs) |>
  mutate(node = coalesce(na_if(parent_station, ""), stop_id))

kommun <- uppsala_kommun()
in_kommun <- stops |> st_filter(kommun) |> pull(node) |> unique()

# Edges between consecutive stops of each trip, weighted by mean travel time
edges <- stop_times_on(gtfs, on) |>
  filter(is_bus(route_type)) |>
  left_join(st_drop_geometry(stops) |> select(stop_id, node), by = "stop_id") |>
  arrange(trip_id, stop_sequence) |>
  group_by(trip_id) |>
  mutate(to = lead(node), minutes = (lead(arr_s) - dep_s) / 60) |>
  ungroup() |>
  filter(!is.na(to), node != to, node %in% in_kommun, to %in% in_kommun) |>
  group_by(from = node, to) |>
  summarise(trips = n(), minutes = pmax(mean(minutes, na.rm = TRUE), 0.5), .groups = "drop")

g <- graph_from_data_frame(edges, directed = TRUE)
btw <- betweenness(g, weights = E(g)$minutes, normalized = TRUE)

nodes <- stops |>
  group_by(node) |>
  summarise(name = first(stop_name)) |>
  st_centroid() |>
  filter(node %in% names(btw)) |>
  mutate(betweenness = btw[node]) |>
  st_transform(crs_se) |>
  arrange(betweenness)

xy <- nodes |>
  mutate(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]) |>
  st_drop_geometry() |>
  select(node, x, y)

segments <- edges |>
  left_join(xy, by = c("from" = "node")) |>
  left_join(xy |> rename(xend = x, yend = y), by = c("to" = "node"))

p <- ggplot() +
  geom_segment(data = segments, aes(x, y, xend = xend, yend = yend, linewidth = trips), color = clr_text2, alpha = 0.3) +
  geom_sf(data = nodes, aes(size = betweenness, color = betweenness), alpha = 0.85) +
  ggrepel::geom_text_repel(data = nodes |> slice_max(betweenness, n = 8), aes(label = str_remove(name, " \\(Uppsala\\)"), geometry = geometry), stat = "sf_coordinates", family = f1, size = 5, color = clr_text, seed = 4, bg.color = clr_bg, bg.r = 0.15, segment.color = clr_text2) +
  scale_linewidth(range = c(0.05, 1.2), guide = "none") +
  scale_size_area(max_size = 8, breaks = c(0.05, 0.15, 0.3)) +
  scale_color_steps(low = clr_accent2, high = clr_accent, n.breaks = 6, guide = "none") +
  coord_map_box(st_bbox(nodes)) +
  labs(size = "Betweenness") +
  guides(size = guide_legend(override.aes = list(color = clr_accent2))) +
  theme_map()

save_map(p, here::here("2026/24-network/24-network-betweenness.png"), title = "The stops everything \\ passes through", subtitle = "UL bus network in Uppsala Municipality as a graph, stops sized \\ by betweenness on travel-time-weighted shortest paths", caption = caption_src("UL GTFS via Trafiklab"))
