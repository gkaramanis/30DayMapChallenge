library(tidyverse)
library(sf)
library(camcorder)

gg_record(here::here("30daymap-temp"), width = 10, height = 10, dpi = 320)

trains <- read_sf(here::here("2022/data/global-trains.json"))

trains_simpl <- sf::st_simplify(trains)

ocean <- st_point(x = c(0,0)) %>%
  st_buffer(dist = 6371000) %>%
  st_sfc(crs = "+proj=ortho +lat_0=53 +lon_0=14")

ggplot(trains_simpl) +
  geom_sf(data = ocean, fill = "grey9", color = "grey5") +
  geom_sf(size = 0.4, alpha = 0.5, color = "grey40") +
  geom_sf(aes(color = status, size = status, linetype = status)) +
  scale_color_manual(values = c("white", "hotpink3", "goldenrod1"), breaks = c("Open", "Closed", "Unknown")) +
  scale_size_manual(values = c(0.1, 0.2, 0.2), breaks = c("Open", "Closed", "Unknown")) +
  scale_linetype_manual(values = c("solid", "dotted", "dashed"), breaks = c("Open", "Closed", "Unknown")) +
  guides(color = guide_legend(override.aes = list(size = 0.7))) +
  labs(
    title = "Global Railways",
    caption = "Source: WFPGeoNode · Graphic: Georgios Karamanis"
  ) +
  coord_sf(crs = "+proj=ortho +lat_0=53 +lon_0=14") +
  theme_void(base_family = "Outfit") +
  theme(
    legend.text = element_text(color = "grey95"),
    legend.position = c(0.08, 0.94),
    legend.title = element_blank(),
    plot.background = element_rect(fill = "grey17", color = NA),
    plot.title = element_text(size = 20, color = "grey95"),
    plot.caption = element_text(color = "grey95"),
    plot.margin = margin(10, 0, 10, 0)
  )
