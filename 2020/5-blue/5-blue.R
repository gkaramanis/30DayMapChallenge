library(tidyverse)
library(sf)
library(janitor)


vattendrag <- read_sf(here::here("2020", "data", "flodeslinjer_vd_l_2016_3", "vd_l_2016_3.shp")) %>%
  clean_names()

vattenytor <- read_sf(here::here("2020", "data", "Vattenytor_vy_y_2016_3", "vy_y_2016_3.shp")) %>%
  clean_names()

vandringsleder <- read_sf(here::here("2020", "data", "Vandringsleder-shp", "Vandringsleder.shp")) %>% 
  clean_names()

blue <- vandringsleder %>% 
  filter(namn == "Blå spåret")

box <- c(xmin = 17.6, xmax = 17.7, ymin = 59.78, ymax = 59.8)

vägtrafiknät <- read_sf(here::here("2020", "2-lines", "data", "Uppsala kommun Shape_GeoDatabas__3359", "Uppsala_kommun_ShapeNVDB_DKVagtrafiknat.shp")) %>% 
  clean_names() %>% 
  st_transform(4326) %>% 
  st_crop(st_bbox(box))

uppsala_vattendrag <- vattendrag %>% 
  st_transform(4326) %>% 
  st_crop(st_bbox(box))

uppsala_vattenytor <- vattenytor %>% 
  st_transform(4326) %>% 
  st_crop(st_bbox(box))

ggplot() +
  geom_sf(data = vägtrafiknät, color = "grey80", size = 0.5) +
  geom_sf(data = uppsala_vattendrag, color = "cornflowerblue", size = 1) +
  geom_sf(data = blue, color = "blue3", size = 0.8) +
  geom_sf(data = uppsala_vattenytor, fill = "cornflowerblue", color = NA) +
  annotate("text", 17.637, 59.791, label = "Blå spåret", size = 7, family = "Atkinson Hyperlegible Bold", color = "blue3") +
  annotate("text", 17.681, 59.781, label = "Sources: SMHI, Uppsala kommun\nMap: Georgios Karamanis", size = 2.8, family = "IBM Plex Sans Light", color = "grey50", hjust = 1, lineheight = 0.9) +
  scale_fill_identity() +
  coord_sf(expand = FALSE) +
  theme_void() +
  ggsave(here::here("2020", "5-blue", "5-blue.png"), width = 8, height = 3.85)
