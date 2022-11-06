library(tidyverse)
library(sf)
library(camcorder)

gg_record(here::here("30daymap-temp"), width = 15, height = 15, dpi = 320)

# https://energydata.info/dataset/global-transmission-network
# OSM data 2016
power <- read_sf(here::here("2022/data/osm_power_tmm/osm_power_tmm.shp"))

power_simpl <- power %>% 
  st_simplify()

ocean <- st_point(x = c(0,0)) %>%
  st_buffer(dist = 6371000) %>%
  st_sfc(crs = "+proj=ortho +lat_0=53 +lon_0=14")

ggplot(power_simpl) +
  geom_sf(data = ocean, fill = "#180025", color = "grey5") +
  geom_sf(size = 0.5, color = "blue", alpha = 0.25) +
  geom_sf(size = 0.05, color = "cadetblue1") +
  coord_sf(crs = "+proj=ortho +lat_0=53 +lon_0=14") +
  labs(
    caption = "Global Power Transmission Network · Source: OSM (2016) via ENERGYDATA.INFO · Graphic: Georgios Karamanis"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey10", color = NA),
    plot.caption = element_text(color = "cadetblue2", family = "Outfit", margin = margin(0, 0, 20, 0), hjust = 0.5)
  )
