library(sf)
library(tidyverse)
library(janitor)
library(maps)
library(sfheaders)

sweden <- map_data(map = "world") %>% 
  filter(region == "Sweden") %>% 
  sf_polygon(x = "long", y = "lat", polygon_id = "subregion" , keep = TRUE) %>% 
  st_set_crs(4326)

water <- read_sf(here::here("2020", "data", "Vattenytor_vy_y_2016_3", "vy_y_2016_3.shp")) %>% 
  clean_names() %>% 
  st_transform(4326)

water_islands <- water %>% 
  filter(country == "SE") %>% 
  mutate(color = if_else(ytkod == 80, "black", "cadetblue3"))

islands <- water_islands %>% 
  filter(ytkod == 80)

water <- water_islands %>% 
  filter(ytkod != 80)

ggplot() +
  geom_sf(data = water, aes(color = color), size = 0.05, fill = "cadetblue2") +
  geom_sf(data = islands, aes(fill = color), size = 0.05, color = NA) +
  geom_sf(data = sweden, color = "grey20", fill = NA, size = 0.1) +
  scale_color_identity() +
  scale_fill_identity() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey90", color = NA)
  ) +
  ggsave(here::here("2020", "16-islands", "16-islands.png"), dpi = 320, height = 12, width = 5.319)

