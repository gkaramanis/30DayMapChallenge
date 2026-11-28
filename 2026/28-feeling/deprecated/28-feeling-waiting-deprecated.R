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


gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

gtfs <- ul_gtfs()
on <- as.Date("2026-11-27") # the subtitle says weekday, and Saturday 28 is not one
stopifnot("No service on this date" = length(services_on(gtfs, on)) > 0)

# Expected wait for someone who turns up at random is half the mean headway, 07-19
waits <- stop_times_on(gtfs, on) |>
  filter(between(dep_s, 7 * 3600, 19 * 3600)) |>
  arrange(stop_id, dep_s) |>
  group_by(stop_id) |>
  summarise(departures = n(), headway_min = if (n() > 1) mean(diff(dep_s)) / 60 else NA_real_) |>
  filter(departures >= 2) |>
  mutate(wait_min = headway_min / 2)

city <- bb_sf(bb_city)

stops <- stops_sf(gtfs) |>
  inner_join(waits, by = "stop_id") |>
  st_transform(crs_se) |>
  st_filter(city) |>
  arrange(desc(wait_min))

roads <- osm_roads() |> st_filter(city)

feelings <- c("Relaxed" = clr_accent2, "Fine" = colorspace::lighten(clr_accent2, 0.45), "Restless" = clr_text2, "Impatient" = colorspace::lighten(clr_accent, 0.2), "Fuming" = clr_accent)

stops <- stops |>
  mutate(feeling = cut(wait_min, c(0, 3, 7.5, 15, 30, Inf), labels = names(feelings)))

# Full height of the city box, width in the map box's shape
city_box <- st_bbox(city)
view <- map_box_view(c(mean(city_box[c("xmin", "xmax")]), mean(city_box[c("ymin", "ymax")])), (city_box[["ymax"]] - city_box[["ymin"]]) * map_box$width / map_box$height)

p <- ggplot() +
  geom_sf(data = roads, color = clr_road, linewidth = 0.2) +
  geom_sf(data = stops, aes(color = feeling), size = 1.8) +
  scale_color_manual(values = feelings, labels = paste0(names(feelings), " (", c("under 3", "3-7.5", "7.5-15", "15-30", "over 30"), " min)")) +
  coord_map_box(view) +
  labs(color = NULL) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 4))) +
  theme_map() +
  theme(panel.background = element_rect(fill = clr_land, color = NA))

save_map(p, here::here("2026/28-feeling/28-feeling-waiting.png"), title = "How waiting feels", subtitle = "Average wait at each bus stop for someone \\ arriving at random, 07-19 on a weekday", caption = caption_src("UL GTFS via Trafiklab, OpenStreetMap"))
