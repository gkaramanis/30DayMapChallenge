library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = 9, height = 9, dpi = 320)

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

col_bg <- "#0b0d17"

ggplot() +
  geom_sf(data = shapes, aes(linewidth = trips, color = trips), alpha = 0.6) +
  scale_linewidth(range = c(0.1, 2.5)) +
  scale_color_gradientn(colors = MetBrewer::met.brewer("Tam"), trans = "log10") +
  coord_sf(xlim = st_bbox(city)[c(1, 3)], ylim = st_bbox(city)[c(2, 4)], expand = FALSE) +
  labs(
    title = "Uppsala's bus network",
    subtitle = paste0("Line width shows trips per day on ", format(on, "%e %B %Y")),
    caption = caption_src("UL GTFS via Trafiklab")
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = col_bg, color = NA),
    plot.title = element_text(family = f2, size = 20, face = "bold", color = "white"),
    plot.subtitle = element_text(size = 11, color = "grey80", margin = margin(5, 0, 10, 0)),
    plot.caption = element_text(size = 8, color = "grey70"),
    plot.margin = margin(10, 10, 10, 10)
  )

record_polaroid()
