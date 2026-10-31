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

# First Tuesday from today that has service
typical_weekday <- function(gtfs) {
  dates <- seq(Sys.Date(), Sys.Date() + 27, by = "day")
  dates <- dates[lubridate::wday(dates, week_start = 1) == 2]
  for (d in as.list(dates)) {
    if (length(services_on(gtfs, d)) > 0) return(d)
  }
  stop("No Tuesday with service in the next four weeks; download a fresh feed")
}


gg_record(here::here("30daymap-temp"), width = map_width, height = map_height, units = "in", dpi = 320)

gtfs <- ul_gtfs()
on <- typical_weekday(gtfs)

trips_per_shape <- gtfs$trips |>
  filter(service_id %in% services_on(gtfs, on), !is.na(shape_id)) |>
  count(shape_id, name = "trips")

shapes <- tidytransit::shapes_as_sf(gtfs$shapes) |>
  inner_join(trips_per_shape, by = "shape_id") |>
  st_transform(crs_se) |>
  arrange(trips)

city <- bb_sf(bb_city)
centre <- st_coordinates(st_centroid(city))
half_width <- 5300
half_height <- half_width / 1.1

water <- osm_water()

ggplot() +
  geom_sf(data = water, fill = clr_water, color = NA) +
  geom_sf(data = shapes, aes(linewidth = trips, color = trips), alpha = 0.7) +
  scale_linewidth(range = c(0.15, 2.2), guide = "none") +
  scale_color_gradient(low = clr_accent, high = clr_accent2, trans = "log10", breaks = c(10, 100, 1000)) +
  coord_sf(xlim = centre[1] + c(-1, 1) * half_width, ylim = centre[2] + c(-1, 1) * half_height, expand = FALSE) +
  labs(
    title = "Uppsala's bus network",
    subtitle = paste0("Line width shows trips per day\non ", str_squish(format(on, "%e %B %Y"))),
    caption = caption_src("UL GTFS via Trafiklab"),
    color = "Trips per day"
  ) +
  theme_map() +
  theme(legend.key.width = unit(2, "cm"), legend.key.height = unit(0.35, "cm"))

record_polaroid()
