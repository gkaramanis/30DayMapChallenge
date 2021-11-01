library(tidyverse)
library(sf)
library(janitor)

bygg <- read_sf(here::here("2020", "data", "Byggnader-shp", "Byggnader.shp")) %>% 
  clean_names() %>% 
  st_zm() %>% 
  st_transform(4326)

box <- c(xmin = 17.52, xmax = 17.75, ymin = 59.78, ymax = 59.9)

bygg_cropped <- st_crop(bygg, st_bbox(box))

ggplot(bygg_cropped) +
  geom_sf(fill = "hotpink", color = NA) +
  annotate("text", x = 17.71, y = 59.8, label = "UPPSALA", family = "Futura Condensed Medium", size = 16, color = "hotpink3") +
  annotate("text", x = 17.71, y = 59.79, label = "Source: opendata.uppsala.se\nGraphic: Georgios Karamanis", family = "IBM Plex Sans Condensed", size = 2.5, color = "hotpink3") +
  coord_sf(xlim = c(17.52, 17.75), ylim = c(59.78, 59.9)) +
  theme_minimal(base_family = "Atkinson Hyperlegible") +
  theme(
    plot.background = element_rect(fill = "grey98", color = NA),
    axis.title = element_blank(),
    panel.grid = element_line(size = 0.15)
  ) 

ggsave(here::here("2020", "3-polygons", "3-polygons.png"), dpi = 320, width = 7, height = 7)
 