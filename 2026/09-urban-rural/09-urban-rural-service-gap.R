library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = 8, height = 9, dpi = 320)

gtfs <- ul_gtfs()
on <- typical_weekday(gtfs)

# Stops with at least 10 departures on a weekday
frequent <- stop_times_on(gtfs, on) |>
  count(stop_id, name = "departures") |>
  filter(departures >= 10)

stops <- stops_sf(gtfs) |>
  semi_join(frequent, by = "stop_id") |>
  st_transform(crs_se)

lan <- uppsala_lan() |> st_transform(crs_se)

# 1 km grid aligned with SCB's SWEREF 99 TM grid
bb <- st_bbox(lan)
grid <- st_make_grid(lan, cellsize = 1000, offset = floor(bb[c("xmin", "ymin")] / 1000) * 1000) |>
  st_as_sf() |>
  st_filter(lan) |>
  rename(geometry = x)

centroids <- st_centroid(grid)
nearest <- st_nearest_feature(centroids, stops)
grid$dist_km <- as.numeric(st_distance(centroids, stops[nearest, ], by_element = TRUE)) / 1000

# Optional: SCB population per 1 km square (open geodata, "Statistik på rutor"),
# saved as data/scb-befolkning-1km.gpkg. Keeps populated squares only.
pop_path <- file.path(dir_data, "scb-befolkning-1km.gpkg")
if (file.exists(pop_path)) {
  pop <- read_sf(pop_path) |> st_transform(crs_se)
  grid <- grid |> st_filter(st_centroid(pop))
}

ggplot() +
  geom_sf(data = lan, fill = "grey95", color = NA) +
  geom_sf(data = grid, aes(fill = dist_km), color = NA) +
  geom_sf(data = lan, fill = NA, color = "grey40", linewidth = 0.2) +
  scale_fill_stepsn(colors = MetBrewer::met.brewer("OKeeffe2"), breaks = c(1, 2, 5, 10, 20), limits = c(0, NA)) +
  coord_sf(expand = FALSE) +
  labs(
    title = "How far to a bus that actually comes?",
    subtitle = paste0("Distance from each 1 km square to the nearest stop with 10+ departures on ", format(on, "%e %B %Y")),
    caption = caption_src("UL GTFS via Trafiklab, SCB, OpenStreetMap"),
    fill = "km"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "top",
    legend.key.width = unit(2.5, "lines"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(family = f2, size = 18, face = "bold"),
    plot.subtitle = element_text(size = 10, margin = margin(5, 0, 10, 0)),
    plot.caption = element_text(size = 8),
    plot.margin = margin(10, 10, 10, 10)
  )

record_polaroid()
