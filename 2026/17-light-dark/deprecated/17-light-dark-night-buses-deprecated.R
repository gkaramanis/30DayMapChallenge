library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")
dir.create(dir_data, showWarnings = FALSE, recursive = TRUE)

# Bounding boxes (xmin, ymin, xmax, ymax) in WGS84
bb_city <- c(17.55, 59.80, 17.76, 59.90)

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

osm_roads <- function(bb = bb_city, name = "roads-city") {
  osm_get(name, bb, list(highway = c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential", "living_street", "service", "motorway_link", "trunk_link", "primary_link", "secondary_link")))$osm_lines |>
    select(osm_id, any_of(c("name", "highway", "maxspeed", "surface", "lit"))) |>
    st_transform(crs_se)
}

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

# stop_times for trips on a date, with route info and departure in seconds
stop_times_on <- function(gtfs, on) {
  feed <- tidytransit::filter_feed_by_date(gtfs, on)
  trips <- feed$trips |>
    left_join(
      feed$routes |>
        select(route_id, route_short_name, route_type, any_of("agency_id")),
      by = join_by(route_id)
    )
  feed$stop_times |>
    inner_join(trips, by = join_by(trip_id)) |>
    mutate(dep_s = as.numeric(departure_time), arr_s = as.numeric(arrival_time))
}

stops_sf <- function(gtfs) {
  stops <- gtfs$stops
  if (!"parent_station" %in% names(stops)) stops$parent_station <- NA_character_
  stops |>
    filter(!is.na(stop_lat)) |>
    st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326, remove = FALSE)
}


gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

gtfs <- ul_gtfs()

on <- as.Date("2026-11-17")

night <- stop_times_on(gtfs, on) |>
  filter(dep_s >= 23 * 3600 | dep_s < 5 * 3600)

night_trips <- night |>
  distinct(trip_id) |>
  left_join(gtfs$trips |> select(trip_id, shape_id), by = "trip_id") |>
  count(shape_id, name = "trips")

shapes <- tidytransit::shapes_as_sf(gtfs$shapes) |>
  inner_join(night_trips, by = "shape_id") |>
  st_transform(crs_se)

night_stops <- stops_sf(gtfs) |>
  semi_join(night, by = "stop_id") |>
  st_transform(crs_se)

city <- bb_sf(bb_city)
roads <- osm_roads() |> st_filter(city)

# Glow from stacked lines, wide and faint to narrow and bright
glow <- map(c(4, 2.5, 1.2, 0.4), function(w) {
  geom_sf(data = shapes, color = clr_accent, linewidth = w, alpha = 0.6 / w)
})

p <- ggplot() +
  geom_sf(data = roads, color = colorspace::darken(clr_text2, 0.55), linewidth = 0.2) +
  glow +
  geom_sf(data = night_stops, color = clr_bg, size = 0.5) +
  coord_map_box(city) +
  theme_map(bg = clr_text, text = clr_bg) +
  theme(plot.title = element_text(color = clr_accent))

save_map(p, here::here("2026/17-light-dark/17-light-dark-night-buses.png"), title = "Uppsala after 23:00", subtitle = paste0("Bus routes and stops served\\ between 23:00 and 05:00,\\ ", "night of Tuesday ", str_squish(format(on, "%e %B %Y"))), caption = caption_src("UL GTFS via Trafiklab, OpenStreetMap"))
