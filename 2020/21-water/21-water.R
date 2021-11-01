library(sf)
library(tidyverse)
library(janitor)
library(maps)

water <- read_sf(here::here("2020", "data", "Vattenytor_vy_y_2016_3", "vy_y_2016_3.shp")) %>% 
  clean_names() %>% 
  st_transform(4326)

surface_water <- water %>% 
  filter(ytkod != 80) %>% 
  filter(country == "SE")

ggplot(surface_water) +
  geom_sf(color = "darkblue", size = 0.025, fill = "cornflowerblue") +
  scale_color_identity() +
  scale_fill_identity() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "whitesmoke", color = NA)
  ) 

ggsave(here::here("2020", "21-water", "21-water.png"), dpi = 320, height = 12, width = 5.325)

