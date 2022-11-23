library(tidyverse)
library(sf)
library(ggtext)
library(camcorder)

gg_record(here::here("30daymap-temp"), width = 10, height = 11, dpi = 320)

crs_string <- "+proj=ortho +lat_0=20 +lon_0=-10 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs"

ocean <- st_point(x = c(0, 0)) %>%
  st_buffer(dist = 6371000) %>%
  st_sfc(crs = crs_string)

gisco_countries <- giscoR::gisco_countries

world <- gisco_countries %>% 
  st_intersection(ocean %>% st_transform(4326)) %>% 
  st_transform(crs = crs_string)

# https://data.amerigeoss.org/dataset/major-ocean-currents-arrowpolys-100m-76
curr <- read_sf(here::here("2022/data/Major_Ocean_Currents/Major_Ocean_Currents.shp"))

ggplot() +
  geom_sf(data = ocean, fill = "#6699CC") +
  geom_sf(data = world, fill = "cornsilk2", color = NA) +
  geom_sf(data = curr, aes(fill = TEMP), color = "black", linewidth = 0.1) +
  ggrepel::geom_text_repel(data = curr, aes(label = NAME, geometry = geometry), color = "white", family = "Outfit", stat = "sf_coordinates", bg.color = "#1f4277", bg.r = 0.08, seed = 99, fontface = "bold", size = 3) +
  scale_fill_manual(values = c("#A2D0E9", "#FC766A")) +
  labs(
    title = "<span style='color:#FC766A'>Warm</span> and <span style='color:#A2D0E9'>Cold</span><br>major ocean currents",
    caption = "Source: AmeriGEO · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = "Outfit") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#1f4277", color = NA),
    plot.title = element_markdown(color = "white", hjust = 0.5, face = "bold", size = 26, lineheight = 1),
    plot.caption = element_text(color = "white", hjust = 0.5),
    plot.margin = margin(10, 0, 10, 0)
  )
  

  
  