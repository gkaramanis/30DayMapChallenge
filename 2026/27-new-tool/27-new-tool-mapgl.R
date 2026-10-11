library(tidyverse)
library(sf)
library(mapgl)

source(here::here("2026/R/helpers.R"))

gtfs <- ul_gtfs()
on <- typical_weekday(gtfs)

trips_per_shape <- gtfs$trips |>
  filter(service_id %in% services_on(gtfs, on), !is.na(shape_id)) |>
  left_join(gtfs$routes |> select(route_id, route_short_name), by = "route_id") |>
  group_by(shape_id) |>
  summarise(trips = n(), lines = paste(sort(unique(route_short_name)), collapse = ", "))

shapes <- tidytransit::shapes_as_sf(gtfs$shapes) |>
  inner_join(trips_per_shape, by = "shape_id") |>
  mutate(popup = paste0("<b>Line ", lines, "</b><br>", trips, " trips")) |>
  arrange(trips)

stops <- stops_sf(gtfs) |>
  select(stop_name)

m <- maplibre(style = carto_style("dark-matter"), center = c(17.64, 59.86), zoom = 11) |>
  add_line_layer(
    id = "routes",
    source = shapes,
    line_color = interpolate(column = "trips", values = c(1, 50, 200), stops = c("#2a9d8f", "#e9c46a", "#e76f51")),
    line_width = interpolate(column = "trips", values = c(1, 200), stops = c(0.5, 5)),
    line_opacity = 0.8,
    popup = "popup",
    hover_options = list(line_color = "white")
  ) |>
  add_circle_layer(
    id = "stops",
    source = stops,
    circle_radius = 2,
    circle_color = "white",
    circle_opacity = 0.6,
    tooltip = "stop_name",
    min_zoom = 13
  )

m

htmlwidgets::saveWidget(m, file.path(dir_2026, "27-new-tool", "27-new-tool-mapgl.html"), selfcontained = TRUE)
