library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")

ul_zip <- file.path(dir_data, "ul.zip")
gtfs <- tidytransit::read_gtfs(ul_zip)

on <- as.Date("2026-11-04")
feed <- tidytransit::filter_feed_by_date(gtfs, on)

uc_parents <- gtfs$stops |>
  filter(str_detect(stop_name, "^Uppsala C(entralstation|\\b)")) |>
  pull(stop_id)

uc_all <- gtfs$stops |>
  filter(stop_id %in% uc_parents | parent_station %in% uc_parents) |>
  pull(stop_id)

window_start <- 7 * 3600 + 30 * 60
window_end   <- 8 * 3600

uc_stop_times <- feed$stop_times |>
  filter(stop_id %in% uc_all, as.numeric(arrival_time) >= window_start, as.numeric(arrival_time) <= window_end)

arrivals <- uc_stop_times |> distinct(trip_id)

uc_dist <- uc_stop_times |>
  summarise(dist_uc = min(shape_dist_traveled), .by = trip_id)

trip_info <- feed$trips |>
  filter(trip_id %in% arrivals$trip_id) |>
  select(trip_id, shape_id, route_id)

cat(nrow(arrivals), "trips arrive at Uppsala C between 07:30–08:00\n")

trip_stops <- feed$stop_times |>
  filter(trip_id %in% arrivals$trip_id, !is.na(shape_dist_traveled)) |>
  transmute(trip_id, dist = shape_dist_traveled, time = as.numeric(arrival_time))

stops_by_trip <- split(trip_stops, trip_stops$trip_id)

# Time a trip passes a point, interpolated between its stops
interp_time <- function(id, d) {
  s <- stops_by_trip[[id]]
  approx(s$dist, s$time, xout = d, rule = 2, ties = mean)$y
}

shape_lines <- gtfs$shapes |>
  filter(shape_id %in% trip_info$shape_id) |>
  arrange(shape_id, shape_pt_sequence) |>
  st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326) |>
  st_transform(crs_se) |>
  group_by(shape_id) |>
  summarise(len_gtfs = max(shape_dist_traveled), geometry = st_combine(geometry), .groups = "drop") |>
  st_cast("LINESTRING")

# Cut each shape into ~100 m chunks; dist_mid is in the GTFS distance units
chunk_len <- 100

make_chunks <- function(geom, shape_id, len_gtfs) {
  len <- as.numeric(st_length(geom))
  breaks <- unique(c(seq(0, len, by = chunk_len), len))
  if (length(breaks) < 2) return(NULL)
  coords <- st_line_sample(st_sfc(geom, crs = crs_se), sample = breaks / len) |>
    st_coordinates()
  segs <- map(seq_len(nrow(coords) - 1), \(i) {
    st_linestring(coords[i:(i + 1), 1:2])
  })
  mids <- (head(breaks, -1) + tail(breaks, -1)) / 2
  st_sf(shape_id = shape_id, chunk = seq_along(segs), dist_mid = mids / len * len_gtfs, geometry = st_sfc(segs, crs = crs_se))
}

chunks <- pmap(list(shape_lines$geometry, shape_lines$shape_id, shape_lines$len_gtfs), make_chunks) |>
  bind_rows()

# One row per trip and chunk, up to the trip's arrival at Uppsala C
chunk_times <- chunks |>
  st_drop_geometry() |>
  inner_join(select(trip_info, trip_id, shape_id), by = join_by(shape_id), relationship = "many-to-many") |>
  inner_join(uc_dist, by = join_by(trip_id)) |>
  filter(dist_mid <= dist_uc) |>
  mutate(time = interp_time(first(trip_id), dist_mid), .by = trip_id)

trip_chunks_all <- chunks |>
  inner_join(chunk_times, by = join_by(shape_id, chunk))

cat(nrow(trip_chunks_all), "trip chunks total\n")

bb_city <- c(xmin = 17.45, ymin = 59.75, xmax = 17.85, ymax = 59.95)
bb_view <- st_bbox(bb_city, crs = 4326)

# Time range inside the view; earlier chunks outside it take the earliest time
time_range <- trip_chunks_all |>
  st_filter(st_transform(st_as_sfc(bb_view), crs_se)) |>
  pull(time) |>
  range()

trip_chunks <- trip_chunks_all |>
  mutate(time = pmax(time, time_range[1]))

# Ordered points per trip: each chunk's start, plus the last end
trip_points <- trip_chunks |>
  mutate(x0 = map_dbl(geometry, \(g) g[1, 1]), y0 = map_dbl(geometry, \(g) g[1, 2]), x1 = map_dbl(geometry, \(g) g[2, 1]), y1 = map_dbl(geometry, \(g) g[2, 2])) |>
  st_drop_geometry() |>
  arrange(trip_id, chunk) |>
  reframe(x = c(x0, last(x1)), y = c(y0, last(y1)), time = c(time, last(time)), .by = trip_id) |>
  mutate(i = row_number(), piece = consecutive_id(time %/% (5 * 60)), .by = trip_id)

# Each 5-minute piece also takes the first point of the next piece, so pieces
# meet without a gap and every piece draws as one polyline with one alpha
piece_joins <- trip_points |>
  filter(piece != lag(piece, default = first(piece)), .by = trip_id) |>
  mutate(piece = piece - 1)

trip_paths <- bind_rows(trip_points, piece_joins) |>
  arrange(trip_id, piece, i) |>
  mutate(piece_time = mean(time), .by = c(trip_id, piece)) |>
  arrange(piece_time, trip_id, piece, i)

# Kommun boundary
kommun <- readRDS(file.path(dir_data, "boundary-uppsala_kommun.rds")) |>
  st_transform(crs_se)

# Evaluate expr once and cache the result as rds in data/
cached <- function(name, expr) {
  path <- file.path(dir_data, paste0(name, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  saveRDS(expr, path)
  expr
}

# Same road types as day 3, read from the OSM extract that r5r uses
road_types <- c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential", "living_street", "service", "motorway_link", "trunk_link", "primary_link", "secondary_link")

roads <- cached("osm-roads-morning-rush", {
  st_read(file.path(dir_data, "r5", "uppsala.osm.pbf"), layer = "lines", query = sprintf("SELECT highway FROM lines WHERE highway IN (%s)", paste0("'", road_types, "'", collapse = ", ")), wkt_filter = st_as_text(st_as_sfc(st_bbox(c(xmin = 17.40, ymin = 59.73, xmax = 17.90, ymax = 59.97), crs = 4326))), quiet = TRUE)
}) |>
  st_transform(crs_se) |>
  mutate(major = highway %in% c("motorway", "trunk", "primary", "secondary"))

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

n_arriving <- nrow(arrivals)

p <- ggplot() +
  # geom_sf(data = kommun, fill = clr_land, color = clr_grid, linewidth = 0.3) +
  geom_sf(data = filter(roads, !major), color = clr_road, linewidth = 0.5) +
  geom_sf(data = filter(roads, major), color = clr_road, linewidth = 1) +
  geom_path(data = trip_paths, aes(x, y, group = interaction(trip_id, piece), alpha = piece_time, linewidth = piece_time), color = clr_accent2, lineend = "butt", linejoin = "round") +
  scale_alpha_continuous(range = c(0, 0.3), guide = "none") +
  scale_linewidth_continuous(range = c(0, 2.5), guide = "none") +
  coord_map_box(bb_view) +
  theme_map()

save_map(p, file.path(dir_2026, "04-clusters", "04-clusters-morning-rush.png"), title = "The eight o'clock crowd\\ heads for Uppsala C", subtitle = str_glue("{n_arriving} bus trips arriving 07:30–08:00 on a Wednesday,\\", " fainter where the bus passes earlier"), caption = caption_src("UL GTFS via Trafiklab, OpenStreetMap"))
