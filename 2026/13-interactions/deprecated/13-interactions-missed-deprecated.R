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

# Regional UL feed, for route shapes; it shares trip_ids with the national
# feed. Key from https://developer.trafiklab.se, stored in ~/.Renviron as
# TRAFIKLAB_GTFS_REGIONAL_KEY
ul_gtfs <- function() {
  path <- file.path(dir_data, "ul.zip")
  if (!file.exists(path)) {
    download.file(paste0("https://opendata.samtrafiken.se/gtfs/ul/ul.zip?key=", Sys.getenv("TRAFIKLAB_GTFS_REGIONAL_KEY")), path, mode = "wb", headers = c("Accept-Encoding" = "gzip"))
  }
  tidytransit::read_gtfs(path)
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

miss_s <- 120

trains_in <- events |> filter(mode == "rail", arrives) |> distinct(trip_id, t = arr_s)
trains_out <- events |> filter(mode == "rail", departs) |> distinct(trip_id, t = dep_s)
buses_in <- events |> filter(mode == "bus", arrives) |> select(stop_id, bus_trip = trip_id, t = arr_s)
buses_out <- events |> filter(mode == "bus", departs) |> select(stop_id, bus_trip = trip_id, line = route_short_name, t = dep_s)

walk <- st_drop_geometry(bus_stops) |> select(stop_id, walk_s)

long_wait_min <- 30

# Wait from each departure to the next on the same line from the same
# platform; NA for the last bus of the day
next_bus <- buses_out |>
  arrange(stop_id, line, t) |>
  mutate(wait_min = (lead(t) - t) / 60, .by = c(stop_id, line)) |>
  select(stop_id, bus_trip, wait_min)

# Buses that leave less than miss_s before passengers off a train can reach
# the stop, each bus counted once
missed_buses <- buses_out |>
  left_join(walk, by = join_by(stop_id)) |>
  cross_join(trains_in |> select(t_train = t)) |>
  filter(t < t_train + walk_s, t >= t_train + walk_s - miss_s) |>
  distinct(stop_id, bus_trip, line, t) |>
  left_join(next_bus, by = join_by(stop_id, bus_trip)) |>
  mutate(cost = if_else(is.na(wait_min) | wait_min >= long_wait_min, "long", "short")) |>
  # A bus missed at two stops near the station counts once, at the first
  slice_min(t, n = 1, with_ties = FALSE, by = bus_trip)

ul <- ul_gtfs()

ul_trips <- ul$trips |>
  left_join(ul$routes |> select(route_id, line = route_short_name), by = join_by(route_id)) |>
  select(ul_trip = trip_id, line, shape_id)

ul_stops <- ul$stops |>
  filter(!is.na(stop_lat)) |>
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) |>
  st_transform(crs_se) |>
  st_filter(st_buffer(station, 800)) |>
  select(ul_stop = stop_id)

# The two feeds were published weeks apart and their trip_ids differ for
# some lines, so match on line, departure time and a UL stop within 100 m
stop_match <- st_join(bus_stops |> select(stop_id), ul_stops, join = st_is_within_distance, dist = 100) |>
  st_drop_geometry()

# Where on its shape each missed UL bus leaves Uppsala C
missed_starts <- missed_buses |>
  inner_join(stop_match, by = join_by(stop_id), relationship = "many-to-many") |>
  inner_join(ul$stop_times |> transmute(ul_trip = trip_id, ul_stop = stop_id, t = as.numeric(departure_time), d0 = shape_dist_traveled), by = join_by(ul_stop, t), relationship = "many-to-many") |>
  inner_join(ul_trips, by = join_by(ul_trip, line)) |>
  distinct(bus_trip, .keep_all = TRUE)

shape_pts <- ul$shapes |>
  filter(shape_id %in% missed_starts$shape_id) |>
  st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326) |>
  st_transform(crs_se) |>
  mutate(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]) |>
  st_drop_geometry() |>
  arrange(shape_id, shape_dist_traveled) |>
  split(~shape_id)

# The route each missed bus takes from its stop onward
route_from <- function(shape, d0) {
  pts <- shape_pts[[shape]] |> filter(shape_dist_traveled >= d0)
  st_linestring(cbind(pts$x, pts$y))
}

routes <- bind_rows(
  missed_starts |> count(shape_id, d0, name = "buses") |> mutate(direction = "Any missed bus"),
  missed_starts |> filter(cost == "long") |> count(shape_id, d0, name = "buses") |> mutate(direction = "Next bus 30+ min later")
) |>
  mutate(geometry = map2(shape_id, d0, route_from) |> st_sfc(crs = crs_se)) |>
  st_as_sf()

station_xy <- st_coordinates(station)[1, ]

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

# Cut routes into pieces of about 10 m. Pieces whose ends snap to the same
# 10 m grid points are one stretch of street, so buses there add up instead
# of drawing over each other; each stretch is drawn with its first piece's
# own coordinates.
snap_m <- 10

pieces <- routes |>
  st_crop(view) |>
  st_collection_extract("LINESTRING") |>
  st_cast("LINESTRING", warn = FALSE) |>
  st_segmentize(snap_m) |>
  mutate(xy = map(geometry, \(g) st_coordinates(g)[, 1:2])) |>
  st_drop_geometry() |>
  reframe(
    {
      m <- xy[[1]]
      tibble(x1 = head(m[, 1], -1), y1 = head(m[, 2], -1), x2 = m[-1, 1], y2 = m[-1, 2])
    },
    .by = c(shape_id, d0, direction, buses)
  ) |>
  mutate(sx1 = round(x1 / snap_m), sy1 = round(y1 / snap_m), sx2 = round(x2 / snap_m), sy2 = round(y2 / snap_m)) |>
  filter(sx1 != sx2 | sy1 != sy2) |>
  # Same stretch whichever way the bus drives
  mutate(key = if_else(sx1 < sx2 | (sx1 == sx2 & sy1 < sy2), paste(sx1, sy1, sx2, sy2), paste(sx2, sy2, sx1, sy1))) |>
  distinct(shape_id, d0, direction, key, .keep_all = TRUE) |>
  summarise(buses = sum(buses), x1 = first(x1), y1 = first(y1), x2 = first(x2), y2 = first(y2), .by = c(direction, key))

routes_view <- pieces |>
  mutate(geometry = pmap(list(x1, y1, x2, y2), \(x1, y1, x2, y2) st_linestring(rbind(c(x1, y1), c(x2, y2)))) |> st_sfc(crs = crs_se)) |>
  st_as_sf() |>
  arrange(direction == "Next bus 30+ min later", buses)

stop_labels <- bus_stops |>
  summarise(geometry = st_centroid(st_union(geometry)), .by = name) |>
  mutate(name = if_else(name == "Uppsala Centralstation", "Bus terminal", name))

costly <- sum(missed_buses$cost == "long")

p <- ggplot() +
  geom_sf(data = buildings, fill = clr_land, color = NA) +
  geom_sf(data = roads, color = clr_road, linewidth = 0.3) +
  geom_sf(data = tracks, color = clr_text2, linewidth = 0.3) +
  geom_sf(data = routes_view, aes(linewidth = buses, color = direction), lineend = "round", key_glyph = "path") +
  geom_sf(data = bus_stops, size = 1.6, shape = 21, fill = clr_bg, color = clr_text) +
  geom_sf(data = station, size = 4, shape = 21, fill = clr_text, color = clr_bg, stroke = 1) +
  ggrepel::geom_text_repel(data = stop_labels, aes(label = name, geometry = geometry), stat = "sf_coordinates", family = f1, size = 3.6, color = clr_text, bg.color = clr_bg, bg.r = 0.12, min.segment.length = Inf, seed = 2) +
  ggrepel::geom_text_repel(data = station, aes(geometry = geometry), label = "Trains", stat = "sf_coordinates", family = f1, fontface = "bold", size = 4, color = clr_text, bg.color = clr_bg, bg.r = 0.12, nudge_x = 60, min.segment.length = Inf, seed = 2) +
  scale_linewidth(range = c(0.3, 7), labels = scales::label_number(big.mark = " ")) +
  scale_color_manual(values = c("Any missed bus" = "#B9B8AF", "Next bus 30+ min later" = clr_accent)) +
  guides(color = guide_legend(title = NULL, order = 1, override.aes = list(linewidth = 2)), linewidth = guide_legend(title = "Buses a day", order = 2, override.aes = list(color = clr_text2))) +
  coord_map_box(view) +
  theme_map() +
  theme(legend.box = "horizontal", legend.text = element_text(size = 12))

save_map(p, here::here("2026/13-interactions/13-interactions-missed.png"),
  title = paste0(costly, " missed buses a day \\ cost a half-hour wait"),
  subtitle = paste0("Buses that leave Uppsala C under ", miss_s / 60, " minutes \\ before train passengers reach them, ", str_squish(format(on, "%e %b %Y"))),
  caption = caption_src("GTFS Sweden 3 via Trafiklab, OpenStreetMap")
)
