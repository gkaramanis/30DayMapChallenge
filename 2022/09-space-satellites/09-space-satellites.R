library(rvest)
library(tidyverse)
library(ggshadow)
library(camcorder)

gg_record(here::here("30daymap-temp"), width = 12, height = 12, dpi = 320)

url <- "https://www.satellite-calculations.com/Satellite/satellitelist.php"

sat_raw <- read_html(url) %>% 
  html_nodes("table") %>% 
  .[[3]] %>% 
  html_table() %>% 
  janitor::clean_names()

satellites <- sat_raw %>% 
  mutate(
    lon = parse_number(str_remove(current_sat_longitude, "°.")),
    lat = parse_number(str_remove(current_lat, "°.")),
    lon = if_else(str_detect(current_sat_longitude, "W"), -lon, lon),
    lat = if_else(str_detect(current_sat_longitude, "S"), -lat, lat),
    launchdate = as.numeric(substr(launchdate, 1, 4))
  )

world <- map_data("world")

ggplot() +
  geom_map(data = world, map = world, aes(long, lat, map_id = region), fill = "darkslateblue") +
  geom_glowpoint(data = satellites, aes(x = lon, y = lat, color = launchdate), size = 0.5, shape = 21) +
  scale_color_distiller(palette = "Oranges", breaks = seq(1970, 2020, 10), guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
  coord_map("gilbert", xlim = c(-180, 180)) +
  labs(
    caption = "Source: satellite-calculations.com · Graphic: Georgios Karamanis",
    color = "Launch Year"
  ) +
  theme_void(base_family = "Outfit") +
  theme(
    legend.position = "top",
    legend.text = element_text(color = "grey90"),
    legend.title = element_text(color = "grey90"),
    legend.key.width = unit(3.5, "lines"),
    legend.key.height = unit(0.5, "lines"),
    plot.background = element_rect(fill = "grey13", color = NA),
    plot.margin = margin(20, 0, 20, 0),
    plot.caption = element_text(color = "grey90", hjust = 0.5, margin = margin(-20, 0, 20, 0))
  )
  
