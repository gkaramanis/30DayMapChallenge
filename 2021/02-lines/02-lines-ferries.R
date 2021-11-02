library(tidyverse)
library(camcorder)
library(sf)
library(rmapshaper)

gg_record(dir = "2021/temp", device = "png", width = 12, height = 10, units = "in", dpi = 320)

gr_ferries <- read_sf(here::here("2021", "data", "greece_ferries", "greece_ferries.shp"))

boundary <- read_sf("~/Downloads/gadm36_GRC_shp/gadm36_GRC_0.shp")

f1 = "Montserrat Black"

ggplot(gr_ferries) +
  geom_sf(data = boundary, size = 0.15, fill = "grey85", color = "grey40") +
  geom_sf(size = 0.5, color = "#7500FF", alpha = 0.6) +
  annotate("text", x = 30, y = 44.9, label = "Ferry Routes in Greece", family = f1, size = 16, color = "#1269C7", hjust = 1) +
  annotate("text", x = 30, y = 44.2, label = "Source: OpenStreetMap · Graphic: Georgios Karamanis", family = f1, size = 6, color = "#1269C7", hjust = 1) +
  # labs(
  #   title = "Ferry Routes in Greece",
  #   subtitle = "Source: OpenStreetMap · Graphic: Georgios Karamanis"
  # ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey97", color = NA)
  )
