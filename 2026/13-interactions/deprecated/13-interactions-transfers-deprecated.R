library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")
dir.create(dir_data, showWarnings = FALSE)

bb_centre <- c(17.620, 59.850, 17.660, 59.866)

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

# Keys from https://developer.trafiklab.se, stored in ~/.Renviron:
# TRAFIKLAB_GTFS_SWEDEN_KEY
gtfs_download <- function(url, file) {
  path <- file.path(dir_data, file)
  if (!file.exists(path)) {
    download.file(url, path, mode = "wb", headers = c("Accept-Encoding" = "gzip"))
  }
  path
}

sweden_gtfs <- function() {
  key <- Sys.getenv("TRAFIKLAB_GTFS_SWEDEN_KEY")
  stopifnot("Set TRAFIKLAB_GTFS_SWEDEN_KEY" = key != "" || file.exists(file.path(dir_data, "sweden.zip")))
  path <- gtfs_download(paste0("https://opendata.samtrafiken.se/gtfs-sweden/sweden.zip?key=", key), "sweden.zip")
  tidytransit::read_gtfs(path)
}

# service_ids running on a date, from calendar and calendar_dates
services_on <- function(gtfs, on) {
  day <- c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")[lubridate::wday(on, week_start = 1)]
  ids <- gtfs$calendar |> filter(start_date <= on, end_date >= on, .data[[day]] == 1) |> pull(service_id)
  cd <- gtfs$calendar_dates |> filter(date == on)
  union(setdiff(ids, cd$service_id[cd$exception_type == 2]), cd$service_id[cd$exception_type == 1])
}

# Stops within radius metres of centre and their stop_times on a date, with
# each trip's first and last stop_sequence to tell arrivals from departures.
# Reading the national feed needs several GB of RAM.
gtfs_near <- function(centre, radius, on) {
  gtfs <- sweden_gtfs()
  stops <- gtfs$stops |>
    filter(!is.na(stop_lat), location_type == 0) |>
    st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326, remove = FALSE) |>
    st_filter(st_buffer(centre, radius))
  trips <- gtfs$trips |>
    filter(service_id %in% services_on(gtfs, on)) |>
    left_join(gtfs$routes |> select(route_id, route_short_name, route_type), by = "route_id")
  st <- gtfs$stop_times |> filter(stop_id %in% stops$stop_id) |> inner_join(trips, by = "trip_id")
  ends <- gtfs$stop_times |> filter(trip_id %in% st$trip_id) |> summarise(first_seq = min(stop_sequence), last_seq = max(stop_sequence), .by = trip_id)
  list(stops = stops, stop_times = left_join(st, ends, by = "trip_id"))
}

is_rail <- function(route_type) route_type == 2 | between(route_type, 100, 199)

# Quadratic Bézier from a to b that bows to the left of travel, so the two
# directions between the same pair of points separate
arc <- function(a, b, bend = 0.18, n = 40) {
  v <- b - a
  ctrl <- (a + b) / 2 + bend * c(-v[2], v[1])
  t <- seq(0, 1, length.out = n)
  st_linestring((1 - t)^2 %o% a + 2 * (1 - t) * t %o% ctrl + t^2 %o% b)
}


gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

on <- as.Date("2026-11-13")
centre <- st_sfc(st_point(c(17.6454, 59.8586)), crs = 4326)

near <- cached("gtfs-uppsala-c-20261113", gtfs_near(centre, 600, on))

events <- near$stop_times |>
  mutate(mode = if_else(is_rail(route_type), "rail", "bus"), arr_s = as.numeric(arrival_time), dep_s = as.numeric(departure_time), arrives = stop_sequence != first_seq, departs = stop_sequence != last_seq)

stops <- near$stops |>
  st_transform(crs_se) |>
  semi_join(events, by = "stop_id")

# Every train uses one platform-less stop at Uppsala C
station <- stops |>
  semi_join(filter(events, mode == "rail"), by = "stop_id") |>
  summarise(geometry = st_centroid(st_union(geometry)))

# Platform-to-street time plus straight-line distance at walking pace
bus_stops <- stops |>
  semi_join(filter(events, mode == "bus"), by = "stop_id") |>
  mutate(walk_s = 120 + as.numeric(st_distance(geometry, station$geometry)) / 1.2, name = str_remove(stop_name, " \\(Uppsala\\)$"))

window_s <- 600

trains_in <- events |> filter(mode == "rail", arrives) |> distinct(trip_id, t = arr_s)
trains_out <- events |> filter(mode == "rail", departs) |> distinct(trip_id, t = dep_s)
buses_in <- events |> filter(mode == "bus", arrives) |> select(stop_id, bus_trip = trip_id, t = arr_s)
buses_out <- events |> filter(mode == "bus", departs) |> select(stop_id, bus_trip = trip_id, t = dep_s)

walk <- st_drop_geometry(bus_stops) |> select(stop_id, walk_s)

# Bus departures that leave within the window after a train arrives, each bus counted once
train_to_bus <- buses_out |>
  left_join(walk, by = join_by(stop_id)) |>
  cross_join(trains_in |> select(t_train = t)) |>
  filter(t >= t_train + walk_s, t <= t_train + walk_s + window_s) |>
  distinct(stop_id, bus_trip) |>
  count(stop_id, name = "connections") |>
  mutate(direction = "Train to bus")

# Bus arrivals that reach a train departing within the window
bus_to_train <- buses_in |>
  left_join(walk, by = join_by(stop_id)) |>
  cross_join(trains_out |> select(t_train = t)) |>
  filter(t_train >= t + walk_s, t_train <= t + walk_s + window_s) |>
  distinct(stop_id, bus_trip) |>
  count(stop_id, name = "connections") |>
  mutate(direction = "Bus to train")

station_xy <- st_coordinates(station)[1, ]

arcs <- bind_rows(train_to_bus, bus_to_train) |>
  left_join(bus_stops |> select(stop_id), by = join_by(stop_id)) |>
  mutate(geometry = map2(direction, geometry, \(d, g) if (d == "Train to bus") arc(station_xy, st_coordinates(g)[1, ]) else arc(st_coordinates(g)[1, ], station_xy)) |> st_sfc(crs = crs_se)) |>
  st_as_sf() |>
  arrange(connections)

stops_bb <- st_bbox(bus_stops)
view <- map_box_view(c(mean(stops_bb[c("xmin", "xmax")]), mean(stops_bb[c("ymin", "ymax")])), 1100)

buildings <- osm_get("buildings-centre", bb_centre, list(building = NA))$osm_polygons |>
  st_transform(crs_se) |>
  st_crop(view)
roads <- osm_get("roads-centre", bb_centre, list(highway = c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential", "living_street", "service", "motorway_link", "trunk_link", "primary_link", "secondary_link")))$osm_lines |>
  st_transform(crs_se) |>
  st_crop(view)
tracks <- osm_get("rail-lan", c(16.70, 59.30, 18.95, 60.75), list(railway = "rail"))$osm_lines |>
  st_transform(crs_se) |>
  st_crop(view)

stop_labels <- bus_stops |>
  summarise(geometry = st_centroid(st_union(geometry)), .by = name) |>
  mutate(name = if_else(name == "Uppsala Centralstation", "Bus terminal", name))

met <- sum(train_to_bus$connections)
departures <- nrow(buses_out)

p <- ggplot() +
  geom_sf(data = buildings, fill = clr_land, color = NA) +
  geom_sf(data = roads, color = clr_road, linewidth = 0.3) +
  geom_sf(data = tracks, color = clr_text2, linewidth = 0.3) +
  geom_sf(data = arcs, aes(linewidth = connections, color = direction), lineend = "round", alpha = 0.85) +
  geom_sf(data = bus_stops, size = 1.6, shape = 21, fill = clr_bg, color = clr_text) +
  geom_sf(data = station, size = 4, shape = 21, fill = clr_text, color = clr_bg, stroke = 1) +
  ggrepel::geom_text_repel(data = stop_labels, aes(label = name, geometry = geometry), stat = "sf_coordinates", family = f1, size = 3.6, color = clr_text, bg.color = clr_bg, bg.r = 0.12, min.segment.length = Inf, seed = 2) +
  ggrepel::geom_text_repel(data = station, aes(geometry = geometry), label = "Trains", stat = "sf_coordinates", family = f1, fontface = "bold", size = 4, color = clr_text, bg.color = clr_bg, bg.r = 0.12, nudge_x = 60, min.segment.length = Inf, seed = 2) +
  scale_linewidth(range = c(0.2, 4), labels = scales::label_number(big.mark = " ")) +
  scale_color_manual(values = c("Train to bus" = clr_accent, "Bus to train" = clr_accent2)) +
  guides(color = guide_legend(title = NULL, order = 1, override.aes = list(linewidth = 2)), linewidth = guide_legend(title = "Buses a day", order = 2, override.aes = list(color = clr_text2))) +
  coord_map_box(view) +
  theme_map() +
  theme(legend.box = "horizontal", legend.text = element_text(size = 12))

save_map(p, here::here("2026/13-interactions/13-interactions-transfers.png"),
  title = paste0(scales::percent(met / departures), " of buses near \\ Uppsala C meet a train"),
  subtitle = paste0("Buses within 10 minutes of a train, walk included, \\ ", str_squish(format(on, "%A %e %B %Y"))),
  caption = caption_src("GTFS Sweden 3 via Trafiklab, OpenStreetMap")
)
