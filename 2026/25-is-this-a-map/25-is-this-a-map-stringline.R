library(tidyverse)
library(sf)
library(patchwork)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = 12, height = 8, dpi = 320)

gtfs <- ul_gtfs()
on <- typical_weekday(gtfs)
line <- "1"

st_line <- stop_times_on(gtfs, on) |>
  filter(route_short_name == line)
if ("direction_id" %in% names(st_line)) st_line <- filter(st_line, direction_id == 0)

# Stop order and distance from the trip with the most stops
ref_trip <- st_line |>
  count(trip_id) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  pull(trip_id)

ref_shape <- gtfs$trips |>
  filter(trip_id == ref_trip) |>
  pull(shape_id)

shape <- tidytransit::shapes_as_sf(gtfs$shapes) |>
  filter(shape_id == ref_shape) |>
  st_transform(crs_se)

stops <- stops_sf(gtfs) |>
  st_transform(crs_se)

ref_stops <- st_line |>
  filter(trip_id == ref_trip) |>
  arrange(stop_sequence) |>
  distinct(stop_id) |>
  left_join(stops |> select(stop_id, stop_name), by = "stop_id") |>
  st_as_sf() |>
  mutate(km = as.numeric(st_line_project(rep(st_geometry(shape), n()), geometry)) / 1000)

stringlines <- st_line |>
  inner_join(st_drop_geometry(ref_stops) |> select(stop_id, km), by = "stop_id") |>
  mutate(time = hms::as_hms(dep_s))

p_string <- ggplot(stringlines, aes(time, km, group = trip_id)) +
  geom_line(linewidth = 0.25, color = "#00a651", alpha = 0.8) +
  scale_x_time(breaks = hms::hms(hours = seq(5, 24, 2)), labels = function(x) sprintf("%02d:00", as.numeric(x) %/% 3600)) +
  scale_y_continuous(breaks = ref_stops$km[seq(1, nrow(ref_stops), by = 3)], labels = ref_stops$stop_name[seq(1, nrow(ref_stops), by = 3)]) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = f1) +
  theme(panel.grid.minor = element_blank(), axis.text.y = element_text(size = 7))

p_map <- ggplot() +
  geom_sf(data = shape, color = "#00a651", linewidth = 1) +
  geom_sf(data = ref_stops, size = 0.8) +
  theme_void()

p_map + p_string +
  plot_layout(widths = c(1, 3)) +
  plot_annotation(
    title = paste0("Line ", line, ", all day"),
    subtitle = paste0("Every trip of UL line ", line, " in one direction as a line through time and space, ", format(on, "%e %B %Y")),
    caption = caption_src("UL GTFS via Trafiklab"),
    theme = theme_void(base_family = f1) +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(family = f2, size = 20, face = "bold"),
        plot.subtitle = element_text(size = 10, margin = margin(5, 0, 10, 0)),
        plot.caption = element_text(size = 8),
        plot.margin = margin(10, 10, 10, 10)
      )
  )

record_polaroid()
