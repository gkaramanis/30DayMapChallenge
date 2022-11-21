library(tidyverse)
library(sf)
library(camcorder)

gg_record(here::here("30daymap-temp"), width = 24, height = 24, dpi = 320)

st_layers("~/Downloads/kontur_population.gpkg")

# Kontur dataset not in GitHub repo!
# Download from https://data.humdata.org/dataset/kontur-population-dataset
pop <- read_sf(here::here("2022/data/kontur_population.gpkg"), query = "select * from population where population >= 1000")

crs_string <- "+proj=ortho +lat_0=30 +lon_0=70 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs"

ocean <- st_point(x = c(0, 0)) %>%
  st_buffer(dist = 6371000) %>%
  st_sfc(crs = crs_string)

land <- read_sf(here::here("2022/data/ne_10m_land/ne_10m_land.shp")) %>% 
  st_intersection(ocean %>% st_transform(4326)) %>% 
  st_transform(crs = crs_string)

pop_ortho <- pop %>% 
  st_transform(4326) %>% 
  st_intersection(ocean %>% st_transform(4326)) %>% 
  st_transform(crs = crs_string)

ggplot() +
  geom_sf(data = ocean, fill = "#001E2D", color = "grey5") +
  geom_sf(data = land, fill = "black", color = NA) +
  geom_sf(data = pop_ortho, aes(color = population), linewidth = 0.1) +
  scale_color_distiller(palette = "OrRd", trans = "pseudo_log") +
  labs(
    caption = "Cells with ≥ 1 000 people\nSource: Kontur Population\nGraphic: Georgios Karamanis"
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey10", color = NA),
    plot.caption = element_text(color = "lightgoldenrod1", family = "Outfit", margin = margin(0, 0, 20, 0), hjust = 0.5, size = 20)
  )
