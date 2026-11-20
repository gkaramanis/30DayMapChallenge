library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")

# Evaluate expr once and cache the result as rds in data/
cached <- function(name, expr) {
  path <- file.path(dir_data, paste0(name, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  out <- expr
  saveRDS(out, path)
  out
}

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

# SCB boundaries: nested zips, SWEREF 99 TM with no CRS set
scb_zip <- file.path(dir_data, "scb-digitala-granser-260225.zip")
if (!file.exists(scb_zip)) download.file("https://scb.se/contentassets/3443fea3fa6640f7a57ea15d9a372d33/shape_svenska_260225.zip", scb_zip, mode = "wb")

lan <- read_sf(sprintf("/vsizip/{/vsizip/%s/LanSweref99TM.zip}", scb_zip)) |>
  st_set_crs(crs_se) |>
  filter(LnKod == "03")

# GTFS Sverige 2 from Samtrafiken's open archive
gtfs_day <- "20261105"
gtfs_zip <- file.path(dir_data, "gtfs-sverige-2", paste0("sweden-", gtfs_day, ".zip"))
dir.create(dirname(gtfs_zip), showWarnings = FALSE)
if (!file.exists(gtfs_zip)) download.file(sprintf("https://data.samtrafiken.se/trafiklab/gtfs-sverige-2/%s/%s/sweden-%s.zip", substr(gtfs_day, 1, 4), substr(gtfs_day, 5, 6), gtfs_day), gtfs_zip, mode = "wb")

# Rail departures per stop that day: every call but a trip's last
rail_departures <- cached(paste0("gtfs-rail-departures-", gtfs_day), local({
  dir <- tempfile()
  unzip(gtfs_zip, files = c("stops.txt", "trips.txt", "routes.txt", "stop_times.txt", "calendar.txt", "calendar_dates.txt"), exdir = dir)
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  csv <- \(f) sprintf("read_csv('%s/%s.txt', all_varchar = true) AS %s", dir, f, f)
  weekday <- tolower(format(as.Date(gtfs_day, "%Y%m%d"), "%A"))
  DBI::dbGetQuery(con, glue::glue("
    WITH services AS (
      SELECT service_id FROM {csv('calendar')} WHERE {weekday} = '1' AND '{gtfs_day}' BETWEEN start_date AND end_date
      UNION SELECT service_id FROM {csv('calendar_dates')} WHERE date = '{gtfs_day}' AND exception_type = '1'
      EXCEPT SELECT service_id FROM {csv('calendar_dates')} WHERE date = '{gtfs_day}' AND exception_type = '2'),
    calls AS (
      SELECT stop_id, stop_sequence::INT AS seq, max(stop_sequence::INT) OVER (PARTITION BY trip_id) AS last_seq
      FROM {csv('stop_times')} JOIN {csv('trips')} USING (trip_id) JOIN {csv('routes')} USING (route_id) JOIN services USING (service_id)
      WHERE route_type::INT = 2 OR route_type::INT BETWEEN 100 AND 199)
    SELECT stop_id, stop_name, stop_lat::DOUBLE AS lat, stop_lon::DOUBLE AS lon, count(*) AS departures
    FROM calls JOIN {csv('stops')} USING (stop_id) WHERE seq < last_seq GROUP BY ALL
  "))
}))

# County stations plus Märsta and Arlanda C, without the Lennakatten heritage line
stations <- st_as_sf(rail_departures, coords = c("lon", "lat"), crs = 4326) |>
  st_transform(crs_se) |>
  filter(lengths(st_intersects(geometry, lan)) > 0 | stop_name %in% c("Märsta station", "Arlanda Centralstation"), !str_detect(stop_name, "Lennakatten")) |>
  mutate(name = str_remove(stop_name, " station$") |> str_replace("Centralstation", "C"))

hex <- h3jsr::polygon_to_cells(st_transform(lan, 4326), res = 7)[[1]] |>
  h3jsr::cell_to_polygon(simple = FALSE) |>
  st_transform(crs_se)

# Bike time to each station, up to an hour. LTS 4 allows country roads; r5r's default of 2 drops most of them.
ride_times <- cached("cycling-h3-7-rail-stations-20261105", {
  options(java.parameters = "-Xmx6G")
  r5 <- r5r::build_network(file.path(dir_data, "r5"))
  on.exit(r5r::stop_r5(r5))
  origins <- h3jsr::cell_to_point(hex$h3_address, simple = FALSE) |> rename(id = h3_address)
  destinations <- stations |> select(id = stop_id) |> st_transform(4326)
  r5r::travel_time_matrix(r5, origins = origins, destinations = destinations, mode = "BICYCLE", departure_datetime = as.POSIXct("2026-11-05 08:00", tz = "Europe/Stockholm"), max_trip_duration = 60, max_lts = 4)
})

# Hex scale by ride time to the nearest station, in 15-minute steps
nearest <- ride_times |>
  slice_min(travel_time_p50, by = from_id, with_ties = FALSE) |>
  left_join(st_drop_geometry(stations) |> select(stop_id, departures), by = join_by(to_id == stop_id)) |>
  mutate(k = case_when(travel_time_p50 <= 15 ~ 0.96, travel_time_p50 <= 30 ~ 0.74, travel_time_p50 <= 45 ~ 0.54, .default = 0.36))

hex_ride <- inner_join(hex, nearest, by = join_by(h3_address == from_id))

shrink <- function(x, k) {
  centre <- st_centroid(st_geometry(x))
  st_set_geometry(x, st_sfc(pmap(list(st_geometry(x), centre, k), \(g, cc, f) (g - cc) * f + cc), crs = st_crs(x)))
}

# Labels to the right of each station, or to the left where the right is taken
station_labels <- function(data, side) {
  ggrepel::geom_text_repel(data = data, aes(label = name, geometry = geometry), stat = "sf_coordinates", family = f1, size = 3.8, color = clr_text, seed = 1, max.overlaps = Inf, nudge_x = side * 18000, hjust = (1 - side) / 2, direction = "y", min.segment.length = 0, segment.color = clr_text2, segment.size = 0.25, bg.color = clr_bg, bg.r = 0.2)
}

labels_left <- c("Heby", "Morgongåva", "Enköping")

p <- ggplot() +
  geom_sf(data = shrink(hex, 0.96), fill = clr_land, color = clr_bg, linewidth = 0.5) +
  geom_sf(data = shrink(hex_ride, hex_ride$k), aes(fill = departures), color = NA) +
  geom_sf(data = stations, shape = 21, fill = clr_bg, color = clr_text, size = 1.6, stroke = 0.6) +
  station_labels(filter(stations, !name %in% labels_left), 1) +
  station_labels(filter(stations, name %in% labels_left), -1) +
  scale_fill_stepsn(colors = c(clr_accent, clr_accent2), limits = c(0, 225), breaks = c(0, 75, 150, 225)) +
  coord_map_box(c(st_geometry(hex), st_buffer(st_geometry(filter(stations, name == "Märsta")), 3000))) +
  guides(fill = guide_coloursteps(title = "Train departures per day\nat the nearest station", theme = theme(legend.key.width = unit(9, "lines"), legend.key.height = unit(0.5, "lines")))) +
  theme_map() +
  theme(legend.position.inside = c(0.98, 0.98), legend.justification.inside = c(1, 1), legend.direction = "horizontal")

save_map(p, here::here("2026/20-hexagons/20-hexagons-cycling.png"),
  title = "Most people in Uppsala\\ County can cycle to a train",
  subtitle = "The bigger the hexagon, the shorter\\ the bike ride to a station, up to an hour",
  caption = caption_src("Samtrafiken, SCB, OpenStreetMap")
)
