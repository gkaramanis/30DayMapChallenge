library(tidyverse)
library(sf)
library(camcorder)

gg_record(here::here("30daymap-temp"), width = 10, height = 10, dpi = 320)

# https://glad.geog.umd.edu/projects/gfm/global/gindex.html
forests <- read_sf(here::here("2022/data/biomes/biomes.shp")) %>% 
  st_set_crs(4326)

world <- read_sf(here::here("2022/data/ne_10m_land/"))

ggplot() +
  geom_sf(data = world, color = NA, fill = "#180025") +
  geom_sf(data = forests, aes(fill = BIOME), size = 0.1, color = NA) +
  MetBrewer::scale_fill_met_d("Egypt") +
  coord_sf(crs = "+proj=adams_ws1", expand = FALSE) +
  labs(
    caption = "Sources: Global Forest Monitoring Project & Natural Earth · Graphic: Georgios Karamanis",
    fill = "Forest Biome"
  ) +
  theme_minimal(base_family = "Outfit") +
  theme(
    legend.position = c(0.15, 0.3),
    legend.title = element_text(color = "grey95", size = 14),
    legend.text = element_text(color = "grey95", size = 12),
    plot.background = element_rect(fill = "#545b72", color = NA),
    panel.grid = element_line(size = 0.2, color = "#180025"),
    plot.caption = element_text(hjust = 0.5, color = "grey95", size = 8)
  )
