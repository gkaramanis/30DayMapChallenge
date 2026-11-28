library(tidyverse)
library(sf)

source(here::here("2026", "R", "helpers.R"))

dir_data <- file.path(dir_2026, "data")

ul_gtfs_path <- function() {
    file.path(dir_data, "ul.zip")
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


# Proxy for crowding: scheduled bus trips per hour on each stop-to-stop link, 07-09 on Fri 2026-11-27
gtfs <- ul_gtfs()
on <- as.Date("2026-11-27")

bb_city <- st_bbox(c(xmin = 17.55, ymin = 59.80, xmax = 17.76, ymax = 59.90), crs = 4326) |> st_as_sfc() |> st_transform(crs_se)
city_box <- st_bbox(bb_city)
view <- map_box_view(c(mean(city_box[c("xmin", "xmax")]), mean(city_box[c("ymin", "ymax")])), (city_box[["ymax"]] - city_box[["ymin"]]) * map_box$width / map_box$height)
view_sfc <- st_as_sfc(view)

stops <- stops_sf(gtfs) |> st_transform(crs_se) |> select(stop_id)

links <- stop_times_on(gtfs, on) |>
  filter(route_type %in% c(3, 700, 701, 702, 704, 714)) |>
  arrange(trip_id, stop_sequence) |>
  group_by(trip_id) |>
  mutate(next_stop = lead(stop_id), next_dep = lead(arr_s)) |>
  ungroup() |>
  filter(!is.na(next_stop), between(dep_s, 7 * 3600, 9 * 3600)) |>
  mutate(a = pmin(stop_id, next_stop), b = pmax(stop_id, next_stop)) |>
  count(a, b, name = "trips") |>
  mutate(per_hour = trips / 2)

xy <- stops |> mutate(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]) |> st_drop_geometry()

segs <- links |>
  left_join(xy, by = c("a" = "stop_id")) |>
  left_join(xy, by = c("b" = "stop_id"), suffix = c("_a", "_b")) |>
  filter(!is.na(x_a), !is.na(x_b)) |>
  mutate(geometry = pmap(list(x_a, y_a, x_b, y_b), \(x1, y1, x2, y2) st_linestring(matrix(c(x1, x2, y1, y2), 2)))) |>
  st_as_sf(sf_column_name = "geometry", crs = crs_se) |>
  st_filter(view_sfc) |>
  arrange(per_hour)

roads <- readRDS(file.path(dir_data, "osm-roads-city.rds"))$osm_lines |> select(highway) |> st_transform(crs_se) |> st_filter(view_sfc)

p <- ggplot() +
  geom_sf(data = roads, color = clr_road, linewidth = 0.2) +
  geom_sf(data = segs, aes(color = per_hour, linewidth = per_hour)) +
  scale_color_viridis_c(option = "inferno", direction = -1, trans = "sqrt") +
  scale_linewidth(range = c(0.2, 2.5), guide = "none") +
  coord_map_box(view) +
  theme_map()

ggsave(here::here("2026/28-feeling/alternatives/28-feeling-crowding.png"), p, width = map_box$width, height = map_box$height, dpi = 150)
