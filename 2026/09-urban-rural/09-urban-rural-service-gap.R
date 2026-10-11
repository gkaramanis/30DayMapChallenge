library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = map_width, height = map_height, units = "in", dpi = 320)

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
  geom_sf(data = lan, fill = clr_land, color = NA) +
  geom_sf(data = grid, aes(fill = dist_km), color = NA) +
  geom_sf(data = lan, fill = NA, color = clr_text2, linewidth = 0.2) +
  scale_fill_steps(low = "#DADFF0", high = clr_accent2, breaks = c(1, 2, 5, 10, 20), limits = c(0, NA), guide = guide_coloursteps(title.position = "top", title.hjust = 0.5)) +
  coord_sf(expand = FALSE) +
  labs(
    title = "How far to a bus\nthat actually comes?",
    subtitle = paste0("Distance from each 1 km square to the nearest\nstop with 10+ departures on ", str_squish(format(on, "%e %B %Y"))),
    caption = caption_src("UL GTFS via Trafiklab, SCB, OpenStreetMap"),
    fill = "km"
  ) +
  theme_map() +
  theme(legend.key.width = unit(3, "lines"), legend.key.height = unit(0.8, "lines"))

record_polaroid()
