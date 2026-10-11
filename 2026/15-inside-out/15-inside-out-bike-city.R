library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = map_width, height = map_height, units = "in", dpi = 320)

city <- bb_sf(bb_city)

cycleways <- osm_cycleways() |> st_filter(city)
roads <- osm_roads() |> st_filter(city)
water <- osm_water()

ggplot() +
  geom_sf(data = water, fill = clr_water, color = NA) +
  geom_sf(data = roads, color = clr_text2, alpha = 0.6, linewidth = 0.1) +
  geom_sf(data = cycleways, color = clr_bg, linewidth = 2.2) +
  geom_sf(data = cycleways, color = clr_accent2, linewidth = 1.2) +
  coord_sf(xlim = st_bbox(city)[c(1, 3)], ylim = st_bbox(city)[c(2, 4)], expand = FALSE) +
  labs(
    title = "Uppsala, drawn for bikes",
    subtitle = "Cycleways as the main roads,\ncar roads as hairlines",
    caption = caption_src("OpenStreetMap")
  ) +
  theme_map()

record_polaroid()
