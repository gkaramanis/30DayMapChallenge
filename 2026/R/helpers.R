# Shared helpers for the 2026 Uppsala transport maps.
# Paths assume this folder sits at 2026/ in the 30daymapchallenge project.

library(tidyverse)
library(sf)

dir_2026 <- here::here("2026")
dir_data <- file.path(dir_2026, "data")
dir.create(dir_data, showWarnings = FALSE, recursive = TRUE)

crs_se <- 3006 # SWEREF 99 TM

# Style shared with the 2026 #30DayChartChallenge (30daychartchallenge/2026)

f1 <- "Metropolis"
f2 <- "Karst"

clr_bg <- "#FAFAF6"
clr_text <- "#1A1A18"
clr_text2 <- "#6B6B65"
clr_accent <- "#E8A020"
clr_accent2 <- "#3D4F9F"
clr_grid <- "#E0E0D8"

# Base-map layers, tints of the palette above
clr_land <- "#F0EFE8"
clr_water <- "#D9DDEC"
clr_road <- clr_grid

# Portrait canvas of the chart challenge: 8 x 11.08 in at 320 dpi (2560 x 3545 px)
map_width <- 8
map_height <- 11.08

caption_src <- function(src) {
  paste0("Source: ", src, " · Graphic: Georgios Karamanis")
}

theme_map <- function(base_size = 24, bg = clr_bg, text = clr_text) {
  theme_void(base_family = f1, base_size = base_size) +
    theme(
      plot.background = element_rect(fill = bg, color = NA),
      text = element_text(color = text),
      legend.position = "top",
      legend.title = element_text(size = 14, hjust = 0.5),
      legend.title.position = "top",
      legend.text = element_text(size = 13),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_text(family = f2, face = "bold", size = 36, hjust = 0.5, margin = margin(t = 20, b = 6)),
      plot.subtitle = element_text(size = 22, hjust = 0.5, lineheight = 1.1, margin = margin(t = 6, b = 30)),
      plot.caption = element_text(size = 12, hjust = 0.5, margin = margin(t = 30, b = 10)),
      plot.margin = margin(t = 20, l = 20, r = 20, b = 18)
    )
}

# Bounding boxes (xmin, ymin, xmax, ymax) in WGS84
bb_city <- c(17.55, 59.80, 17.76, 59.90)
bb_centre <- c(17.620, 59.850, 17.660, 59.866)
bb_kommun <- c(17.15, 59.60, 18.15, 60.20)
bb_lan <- c(16.70, 59.30, 18.95, 60.75)

# Bbox vector as a polygon, in SWEREF 99 TM
bb_sf <- function(bb) {
  st_bbox(setNames(bb, c("xmin", "ymin", "xmax", "ymax")), crs = 4326) |>
    st_as_sfc() |>
    st_transform(crs_se)
}

# Evaluate expr once and cache the result as rds in data/
cached <- function(name, expr) {
  path <- file.path(dir_data, paste0(name, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  out <- expr
  saveRDS(out, path)
  out
}

# OSM ---------------------------------------------------------------------

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

uppsala_kommun <- function() osm_boundary("Uppsala kommun", "7", bb_kommun)
uppsala_lan <- function() osm_boundary("Uppsala län", "4", bb_lan)

# Water and roads for base maps
osm_water <- function(bb = bb_city, name = "water-city") {
  w <- osm_get(name, bb, list(natural = "water", waterway = "river"))
  bind_rows(
    w$osm_polygons |> select(osm_id),
    w$osm_multipolygons |> select(osm_id),
    w$osm_lines |> filter(waterway == "river") |> st_transform(crs_se) |> st_buffer(8) |> st_transform(4326) |> select(osm_id)
  ) |>
    st_transform(crs_se)
}

osm_roads <- function(bb = bb_city, name = "roads-city") {
  osm_get(name, bb, list(highway = c(
    "motorway", "trunk", "primary", "secondary", "tertiary",
    "unclassified", "residential", "living_street", "service",
    "motorway_link", "trunk_link", "primary_link", "secondary_link"
  )))$osm_lines |>
    select(osm_id, any_of(c("name", "highway", "maxspeed", "surface", "lit"))) |>
    st_transform(crs_se)
}

osm_cycleways <- function(bb = bb_city, name = "cycleways-city") {
  osm_get(name, bb, list(highway = "cycleway"))$osm_lines |>
    select(osm_id, any_of(c("name", "highway", "surface", "lit"))) |>
    st_transform(crs_se)
}

# GTFS --------------------------------------------------------------------

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

sweden_gtfs <- function() {
  key <- Sys.getenv("TRAFIKLAB_GTFS_SWEDEN_KEY")
  stopifnot("Set TRAFIKLAB_GTFS_SWEDEN_KEY" = key != "" || file.exists(file.path(dir_data, "sweden.zip")))
  path <- gtfs_download(paste0("https://opendata.samtrafiken.se/gtfs-sweden/sweden.zip?key=", key), "sweden.zip")
  tidytransit::read_gtfs(path)
}

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
    ids <- union(
      setdiff(ids, cd$service_id[cd$exception_type == 2]),
      cd$service_id[cd$exception_type == 1]
    )
  }
  ids
}

# First Tuesday from today that has service
typical_weekday <- function(gtfs) {
  dates <- seq(Sys.Date(), Sys.Date() + 27, by = "day")
  dates <- dates[lubridate::wday(dates, week_start = 1) == 2]
  for (d in as.list(dates)) {
    if (length(services_on(gtfs, d)) > 0) return(d)
  }
  stop("No Tuesday with service in the next four weeks; download a fresh feed")
}

# stop_times for trips on a date, with route info and departure in seconds
stop_times_on <- function(gtfs, on = typical_weekday(gtfs)) {
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
is_rail <- function(route_type) route_type == 2 | between(route_type, 100, 199)
