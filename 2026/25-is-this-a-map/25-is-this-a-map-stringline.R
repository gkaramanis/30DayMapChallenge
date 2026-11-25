library(tidyverse)
library(sf)
library(patchwork)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")

# UL GTFS needs a key from https://developer.trafiklab.se in ~/.Renviron (TRAFIKLAB_GTFS_REGIONAL_KEY)
gtfs_path <- file.path(dir_data, "ul.zip")
if (!file.exists(gtfs_path)) {
  download.file(paste0("https://opendata.samtrafiken.se/gtfs/ul/ul.zip?key=", Sys.getenv("TRAFIKLAB_GTFS_REGIONAL_KEY")), gtfs_path, mode = "wb", headers = c("Accept-Encoding" = "gzip"))
}

# SCB urban areas (tätorter) in Uppsala County
tatorter_path <- file.path(dir_data, "scb-tatorter-lan-2023.rds")
if (!file.exists(tatorter_path)) {
  tatorter_url <- paste0("https://geodata.scb.se/geoserver/stat/wfs?service=WFS&version=2.0.0&request=GetFeature&typeNames=stat:Tatorter_2023&outputFormat=application/json&CQL_FILTER=", URLencode("lan='03'", reserved = TRUE))
  saveRDS(st_read(tatorter_url, quiet = TRUE), tatorter_path)
}

gtfs <- tidytransit::read_gtfs(gtfs_path)
on <- as.Date("2026-11-25")
route <- "804"

# Services running on the date, from calendar and calendar_dates
weekday <- c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")[lubridate::wday(on, week_start = 1)]
exceptions <- filter(gtfs$calendar_dates, date == on)
services <- gtfs$calendar |>
  filter(start_date <= on, end_date >= on, .data[[weekday]] == 1) |>
  pull(service_id) |>
  setdiff(exceptions$service_id[exceptions$exception_type == 2]) |>
  union(exceptions$service_id[exceptions$exception_type == 1])

trips <- gtfs$trips |>
  filter(service_id %in% services, direction_id == 0) |>
  inner_join(filter(gtfs$routes, route_short_name == route), by = "route_id")

stop_times <- gtfs$stop_times |>
  inner_join(select(trips, trip_id, shape_id), by = "trip_id") |>
  mutate(time = hms::as_hms(departure_time))

# The trip with the most stops sets the stop order and the route shape
ref_trip <- stop_times |>
  count(trip_id, shape_id) |>
  slice_max(n, n = 1, with_ties = FALSE)

shape <- tidytransit::shapes_as_sf(gtfs$shapes) |>
  filter(shape_id == ref_trip$shape_id) |>
  st_transform(crs_se)

route_km <- as.numeric(st_length(shape)) / 1000

ref_stops <- stop_times |>
  filter(trip_id == ref_trip$trip_id) |>
  select(stop_id) |>
  left_join(select(gtfs$stops, stop_id, stop_name, stop_lon, stop_lat), by = "stop_id") |>
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) |>
  st_transform(crs_se) |>
  mutate(km = as.numeric(st_line_project(st_geometry(shape), geometry)) / 1000, stop_label = str_remove(stop_name, "^Uppsala ")) |>
  st_drop_geometry()

stop_km <- set_names(ref_stops$km, ref_stops$stop_label)

stringlines <- inner_join(stop_times, select(ref_stops, stop_id, km), by = "stop_id")

# Slow town stretches, where trips run at about 24 km/h against 60 km/h on the open road
town_bands <- tibble(ymin = stop_km[c("Depå Fyrislund", "Enköpings företagspark")], ymax = stop_km[c("Flogsta Centrum", "Enköping Lasarettet")])

axis_stops <- stop_km[c("Depå Fyrislund", "Centralstationen", "Flogsta Centrum", "Örsundsbro centrum", "Enköpings företagspark", "Enköping station", "Enköping Lasarettet")]

town_outlines <- readRDS(tatorter_path) |>
  filter(tatort %in% c("Uppsala", "Enköping")) |>
  st_transform(crs_se) |>
  st_make_valid()

# Uppsala is labeled east of its outline, Enköping west of its own
town_labels <- town_outlines |>
  mutate(bbox = map(geometry, st_bbox), east = tatort == "Uppsala") |>
  st_drop_geometry() |>
  transmute(tatort, x = map2_dbl(bbox, east, \(b, e) if (e) b[["xmax"]] + 1500 else b[["xmin"]] - 1500), y = map_dbl(bbox, \(b) mean(b[c("ymin", "ymax")])), hjust = if_else(east, 0, 1))

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

p_map <- ggplot() +
  geom_sf(data = town_outlines, fill = clr_accent, alpha = 0.15, color = clr_accent, linewidth = 0.3) +
  geom_sf(data = st_simplify(shape, dTolerance = 60), color = clr_text2, linewidth = 0.8) +
  geom_text(data = town_labels, aes(x, y, label = tatort, hjust = hjust), family = f1, size = 5, color = clr_text) +
  coord_sf(clip = "off") +
  theme_void()

p_string <- ggplot(stringlines, aes(time, km, group = trip_id)) +
  geom_rect(data = town_bands, aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax), inherit.aes = FALSE, fill = clr_accent, alpha = 0.15) +
  geom_line(linewidth = 0.35, color = clr_accent2, alpha = 0.8) +
  scale_x_time(breaks = hms::hms(hours = 6:12), labels = \(x) sprintf("%02d:00", as.integer(x) %/% 3600L)) +
  scale_y_reverse(breaks = axis_stops, labels = names(axis_stops), limits = c(route_km, 0)) +
  coord_cartesian(xlim = hms::hms(hours = c(6, 12)), expand = FALSE) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = f1, base_size = 14) +
  theme(panel.grid.major = element_line(color = clr_grid, linewidth = 0.3), panel.grid.minor = element_blank(), axis.text = element_text(color = clr_text2, size = 11), plot.margin = margin(10, 10, 10, 10))

p <- p_map / p_string +
  plot_layout(heights = c(1, 2.2))

save_map(p, here::here("2026/25-is-this-a-map/25-is-this-a-map-stringline.png"),
  title = "A third of the road, \\ half the trip",
  subtitle = paste0("Every ", route, " bus to Enköping, ", str_squish(format(on, "%e %B %Y")), " \\ Orange marks the slow town stretches"),
  caption = caption_src("UL GTFS via Trafiklab, SCB")
)
