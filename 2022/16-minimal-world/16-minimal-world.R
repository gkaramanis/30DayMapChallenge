library(tidyverse)
library(sf)
library(camcorder)

gg_record(here::here("30daymap-temp"), width = 10, height = 10, dpi = 320)

# code modified from https://stackoverflow.com/questions/72945320/sfst-transform-returns-empty-geometry
gisco_countries <- giscoR::gisco_countries

crs_string <- "+proj=ortho +lat_0=33 +lon_0=40 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs"

ocean <- st_point(x = c(0, 0)) %>%
  st_buffer(dist = 6371000) %>%
  st_sfc(crs = crs_string)

world <- gisco_countries %>% 
  st_intersection(ocean %>% st_transform(4326)) %>% # select visible area only
  st_transform(crs = crs_string) %>%  # reproject to ortho
  rmapshaper::ms_simplify(keep = 0.03) %>% 
  smoothr::smooth(method = "densify") %>% 
  rmapshaper::ms_dissolve()
  
ggplot(world) +
  geom_sf(data = ocean, fill = "black", color = "black", linewidth = 1) +
  geom_sf(fill = "white", color = NA) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA)
  )

