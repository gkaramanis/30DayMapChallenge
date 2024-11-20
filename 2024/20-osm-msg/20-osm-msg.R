library(tidyverse)
library(sf)
library(camcorder)

gg_record(here::here("30daymap-temp/"), width = 11, height = 8, dpi = 320)

f1 <- "Caladea"
f1b <- "Sofia Sans Extra Condensed"

msg_area_osm <- read_sf(here::here("2024/data/msg_osm.geojson"))

msg <- msg_area_osm %>% 
  filter(alt_name == "The Garden")

features_in_msg <- msg_area_osm %>%
  mutate(is_valid = st_is_valid(.)) %>%
  filter(is_valid) %>% 
  st_filter(msg, .predicate = st_within) %>% 
  filter(railway != "stop" | is.na(railway))

trees <- msg_area_osm %>% 
  select(natural) %>% 
  filter(str_detect(natural, "tree"))

ggplot() +
  geom_sf(data = msg_area_osm %>% filter(st_geometry_type(geometry) == "POINT"), color = "grey45", size = 0.5, shape = 15) +
  geom_sf(data = msg_area_osm %>% filter(st_geometry_type(geometry) != "POINT"), color = "grey75", linewidth = 0.1, fill = NA) +
  geom_sf(data = features_in_msg, fill = "#FF3366", color = "#FF6699", linewidth = 0.5, size = 2, alpha = 0.2, shape = 15) +
  geom_sf(data = trees, size = 3, shape = 21, stroke = 0.8, fill = "#9DC183", color = "#7A9A5B", alpha = 0.9) +
  geom_sf(data = trees, size = 3, shape = 21, stroke = 0.8, fill = "#9DC183", color = "#7A9A5B", alpha = 0.85) +
  ggrepel::geom_text_repel(data = features_in_msg, aes(label = if_else(str_detect(name, "Square"), toupper(name), name), geometry = geometry), 
    stat = "sf_coordinates", family = f1b, fontface = "bold", size = 6,
    color = "white", bg.color = "black", bg.r = 0.08, 
    min.segment.length = Inf, seed = 99) +
  coord_sf(ylim = c(40.7485, 40.753), expand = FALSE) +
  labs(
    title = "Madison Square Garden",
    caption = "Data: OpenStreetMap · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "grey20", color = NA),
    plot.title = element_text(face = "bold", size = 24, color = "white", margin = margin(20, 0, -40, 0), hjust = 0.5),
    plot.caption = element_text(size = 11, color = "grey96", margin = margin(-20, 10, 10, 0), hjust = 0.5)
  ) 

