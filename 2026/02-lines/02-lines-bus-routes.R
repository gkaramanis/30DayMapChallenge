library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = map_width, height = map_height, units = "in", dpi = 320)

gtfs <- ul_gtfs()
on <- typical_weekday(gtfs)

trips_per_shape <- gtfs$trips |>
  filter(service_id %in% services_on(gtfs, on), !is.na(shape_id)) |>
  count(shape_id, name = "trips")

shapes <- tidytransit::shapes_as_sf(gtfs$shapes) |>
  inner_join(trips_per_shape, by = "shape_id") |>
  st_transform(crs_se) |>
  arrange(trips)

city <- bb_sf(bb_city)
centre <- st_coordinates(st_centroid(city))
half_width <- 5300
half_height <- half_width / 1.1

water <- osm_water()

ggplot() +
  geom_sf(data = water, fill = clr_water, color = NA) +
  geom_sf(data = shapes, aes(linewidth = trips, color = trips), alpha = 0.7) +
  scale_linewidth(range = c(0.15, 2.2), guide = "none") +
  scale_color_gradient(low = clr_accent, high = clr_accent2, trans = "log10", breaks = c(10, 100, 1000)) +
  coord_sf(xlim = centre[1] + c(-1, 1) * half_width, ylim = centre[2] + c(-1, 1) * half_height, expand = FALSE) +
  labs(
    title = "Uppsala's bus network",
    subtitle = paste0("Line width shows trips per day\non ", str_squish(format(on, "%e %B %Y"))),
    caption = caption_src("UL GTFS via Trafiklab"),
    color = "Trips per day"
  ) +
  theme_map() +
  theme(legend.key.width = unit(2, "cm"), legend.key.height = unit(0.35, "cm"))

record_polaroid()
