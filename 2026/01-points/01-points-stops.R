library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")
dir.create(dir_data, showWarnings = FALSE, recursive = TRUE)

# Bounding boxes (xmin, ymin, xmax, ymax) in WGS84
bb_city <- c(17.55, 59.80, 17.76, 59.90)
bb_kommun <- c(17.15, 59.60, 18.15, 60.20)

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

osm_roads <- function(bb = bb_city, name = "roads-city") {
  osm_get(name, bb, list(highway = c(
    "motorway", "trunk", "primary", "secondary", "tertiary",
    "unclassified", "residential", "living_street", "service",
    "motorway_link", "trunk_link", "primary_link", "secondary_link"
  )))$osm_lines |>
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
    ids <- union(
      setdiff(ids, cd$service_id[cd$exception_type == 2]),
      cd$service_id[cd$exception_type == 1]
    )
  }
  ids
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


gg_record(here::here("30daymap-temp"), width = frame_box$width, height = frame_box$height, units = "in", dpi = 320)

# lan <- uppsala_lan() |> st_transform(crs_se)
kommun <- uppsala_kommun() |>
  st_transform(crs_se)

roads <- osm_roads() |>
  mutate(major = highway %in% c("motorway", "trunk", "primary", "secondary"))

gtfs <- ul_gtfs()
on <- as.Date("2026-11-01")

deps <- stop_times_on(gtfs, on) |>
  count(stop_id, name = "departures")

# Group platforms into their parent station where there is one
stops <- stops_sf(gtfs) |>
  left_join(deps, by = "stop_id") |>
  mutate(group_id = coalesce(na_if(parent_station, ""), stop_id)) |>
  group_by(group_id) |>
  summarise(departures = sum(departures, na.rm = TRUE)) |>
  filter(departures > 0) |>
  st_centroid() |>
  st_transform(crs_se) |>
  arrange(departures)

bb <- st_bbox(bb_sf(bb_city))

p <- ggplot() +
  geom_sf(data = kommun, fill = clr_land, color = clr_grid, linewidth = 0.4) +
  geom_sf(data = roads, aes(linewidth = major), color = clr_road) +
  geom_sf(data = stops, aes(size = departures), color = clr_accent2, alpha = 0.5, stroke = 0) +
  geom_sf(data = slice_max(stops, departures, n = 1), aes(size = departures), color = clr_accent, stroke = 0, alpha = 0.7) +
  shadowtext::geom_shadowtext(data = slice_max(stops, departures, n = 1), aes(label = "Uppsala C", geometry = geometry), family = f1, fontface = "bold", size = 6, color = clr_accent, hjust = 0, nudge_x = 500, stat = "sf_coordinates", bg.color = clr_land) +
  scale_linewidth_manual(values = c(`FALSE` = 0.5, `TRUE` = 1), guide = "none") +
  scale_size_area(max_size = 14, labels = scales::label_number(big.mark = " ")) +
  coord_sf(xlim = bb[c("xmin", "xmax")], ylim = bb[c("ymin", "ymax")], crs = crs_se, expand = FALSE) +
  labs(
    title = "Every bus stop in Uppsala",
    subtitle = paste0("UL stops sized by planned departures,\n", str_squish(format(on, "%A %e %B %Y"))),
    caption = caption_src("UL GTFS via Trafiklab")
  ) +
  guides(size = guide_legend(title = NULL, override.aes = list(color = clr_accent2, alpha = 0.8))) +
  theme_map() +
  theme(
    legend.text = element_text(margin = margin(l = 7, r = 20))
  )

frame_map(p, here::here("2026/01-points/01-points-stops.png"))
