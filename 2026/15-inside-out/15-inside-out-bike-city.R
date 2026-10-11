library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = 9, height = 9, dpi = 320)

city <- bb_sf(bb_city)

cycleways <- osm_cycleways() |> st_filter(city)
roads <- osm_roads() |> st_filter(city)
water <- osm_water()

col_bg <- "#fbf7ef"
col_bike <- "#2d6a4f"

ggplot() +
  geom_sf(data = water, fill = "#dbe9f0", color = NA) +
  geom_sf(data = roads, color = "grey70", linewidth = 0.1) +
  geom_sf(data = cycleways, color = col_bg, linewidth = 2.2) +
  geom_sf(data = cycleways, color = col_bike, linewidth = 1.2) +
  coord_sf(xlim = st_bbox(city)[c(1, 3)], ylim = st_bbox(city)[c(2, 4)], expand = FALSE) +
  labs(
    title = "Uppsala, drawn for bikes",
    subtitle = "Cycleways as the main roads, car roads as hairlines",
    caption = caption_src("OpenStreetMap")
  ) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = col_bg, color = NA),
    plot.title = element_text(family = f2, size = 20, face = "bold", color = col_bike),
    plot.subtitle = element_text(size = 11, margin = margin(5, 0, 10, 0)),
    plot.caption = element_text(size = 8),
    plot.margin = margin(10, 10, 10, 10)
  )

record_polaroid()
