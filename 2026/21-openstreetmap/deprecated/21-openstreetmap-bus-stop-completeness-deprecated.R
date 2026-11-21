library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")
dir.create(dir_data, showWarnings = FALSE, recursive = TRUE)

bb_lan <- c(16.70, 59.30, 18.95, 60.75)

# Evaluate expr once and cache the result as rds in data/
cached <- function(name, expr) {
  path <- file.path(dir_data, paste0(name, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  out <- expr
  saveRDS(out, path)
  out
}

# OSM tag column, or NA when no feature carries the tag
tag <- function(df, key) {
  if (key %in% names(df)) df[[key]] else rep(NA_character_, nrow(df))
}

# features: named list of key = values, e.g. list(amenity = "cafe", shop = "bakery").
# Each key is queried separately and the results combined (OR). NA means any value.
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

uppsala_lan <- function() osm_boundary("Uppsala län", "4", bb_lan)

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
on <- as.Date("2026-11-20") # Saturday 21 has a thin timetable, so the Friday before
stopifnot("No service on this date" = length(services_on(gtfs, on)) > 0)

# Bus stops in service according to UL
served <- stop_times_on(gtfs, on) |>
  filter(is_bus(route_type)) |>
  distinct(stop_id)

lan <- uppsala_lan()

gtfs_stops <- stops_sf(gtfs) |>
  semi_join(served, by = "stop_id") |>
  st_filter(lan) |>
  st_transform(crs_se)

osm <- osm_get("bus-stops-lan", bb_lan, list(highway = "bus_stop", public_transport = "platform"))
osm_stops <- bind_rows(osm$osm_points |> select(osm_id, any_of(c("highway", "public_transport", "bus"))), osm$osm_polygons |> select(osm_id, any_of(c("highway", "public_transport", "bus"))) |> st_centroid(), osm$osm_lines |> select(osm_id, any_of(c("highway", "public_transport", "bus"))) |> st_centroid())
osm_stops <- osm_stops[tag(osm_stops, "highway") %in% "bus_stop" | tag(osm_stops, "bus") %in% "yes", ] |>
  st_transform(crs_se)

# A GTFS stop counts as mapped if an OSM stop lies within 50 m
nearest <- st_nearest_feature(gtfs_stops, osm_stops)
gtfs_stops <- gtfs_stops |>
  mutate(dist_m = as.numeric(st_distance(gtfs_stops, osm_stops[nearest, ], by_element = TRUE)), in_osm = if_else(dist_m <= 50, "In OSM", "Missing from OSM")) |>
  arrange(in_osm)

share <- mean(gtfs_stops$in_osm == "In OSM")
lan <- st_transform(lan, crs_se)

p <- ggplot() +
  geom_sf(data = lan, fill = clr_land, color = clr_grid, linewidth = 0.3) +
  geom_sf(data = gtfs_stops, aes(color = in_osm), size = 1, stroke = 0) +
  scale_color_manual(values = c("In OSM" = clr_accent2, "Missing from OSM" = clr_accent)) +
  coord_map_box(st_bbox(lan)) +
  labs(color = NULL) +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  theme_map()

save_map(p, here::here("2026/21-openstreetmap/21-openstreetmap-bus-stop-completeness.png"), title = "Is every bus stop on the map?", subtitle = paste0(scales::percent(share, accuracy = 0.1), " of UL's ", nrow(gtfs_stops), " served bus stops \\ have an OpenStreetMap stop within 50 m"), caption = caption_src("UL GTFS via Trafiklab, OpenStreetMap"))

# Missing stops, for fixing in OSM
gtfs_stops |>
  filter(in_osm == "Missing from OSM") |>
  st_transform(4326) |>
  write_sf(file.path(dir_data, "bus-stops-missing-in-osm.geojson"), delete_dsn = TRUE)
