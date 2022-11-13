library(tidyverse)
library(camcorder)

gg_record(here::here("30daymap-temp"), width = 12, height = 8, dpi = 320)

world <- map_data("world")

ggplot() +
  geom_map(data = world, map = world, aes(x=long, y=lat, map_id = region, fill = substr(region, 1, 1))) +
  coord_map(xlim = c(-180, 180)) +
  scale_fill_viridis_d(option = "turbo") +
  labs(caption = "Graphic: Georgios Karamanis") +
  theme_void(base_family = "Outfit") +
  theme(
    plot.background = element_rect(fill = "grey97", color = NA)
  )