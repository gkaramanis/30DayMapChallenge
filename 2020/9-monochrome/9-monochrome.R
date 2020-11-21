library(tidyverse)
library(sf)
library(janitor)

box <- c(xmin = 17.52, xmax = 17.75, ymin = 59.78, ymax = 59.9)

bicycle_roads <- read_sf(here::here("2020", "data", "Uppsala\ kommun\ Shape_GeoDatabas__3359", "Uppsala_kommun_ShapeNVDB_DKCykelVgsKat.shp")) %>% 
  clean_names() %>% 
  st_transform(4326) %>% 
  st_crop(st_bbox(box))

lights <- read_sf(here::here("2020", "data", "Uppsala\ kommun\ Shape_GeoDatabas__3359", "Uppsala_kommun_ShapeNVDB_DKGCM_belyst.shp")) %>% 
  clean_names() %>% 
  st_transform(4326) %>% 
  st_crop(st_bbox(box)) %>% 
  st_segmentize(1.5)

ggplot() +
  geom_sf(data = bicycle_roads, color = "grey30", size = 0.1) +
  geom_sf(data = lights, color = "grey98", size = 0.4, alpha = 0.6, linetype = "dotted") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey10", color = NA)
  ) +
  ggsave(here::here("2020", "9-monochrome", "9-monochrome.png"), dpi = 320, height = 5.174, width = 5)
