library(tidyverse)
library(marmap)
library(tidyterra)
library(ggnewscale)
library(camcorder)

gg_record(here::here("30daymap-temp"), width = 15, height = 15, dpi = 320)

# Choose higher number for resolution for prototyping (faster)
bathy <- getNOAA.bathy(-180, 180, -90, 90, resolution = 4)

bathy_df <- bathy %>% 
  marmap::as.raster() %>% 
  raster::projectRaster(crs = "+proj=ortho +lat_0=53 +lon_0=14") %>% 
  raster::as.data.frame(xy = TRUE)

# Circle to hide cut-off areas of the raster image
globe <- sf::st_point(x = c(0,0)) %>%
  sf::st_buffer(dist = 6391000) %>%
  sf::st_sfc(crs = "+proj=ortho +lat_0=53 +lon_0=14")

ggplot() +
  geom_sf(data = globe, fill = "#3C7AF1", color = "#3C7AF190", size = 5) +
  geom_raster(data = bathy_df %>% filter(layer < 0), aes(x, y, fill = layer)) +
  scale_fill_hypso_c("gmt_globe_bathy") +
  new_scale_fill() +
  geom_raster(data = bathy_df %>% filter(layer >= 0), aes(x, y, fill = layer)) +
  scale_fill_hypso_c("gmt_globe_hypso") +
  labs(
    caption = "Data: NOAA · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = "Outfit") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#261E2C", color = NA),
    plot.caption = element_text(color = "lightblue2", size = 12, hjust = 0.5, margin = margin(-20, 0, 20, 0))
  )
  
