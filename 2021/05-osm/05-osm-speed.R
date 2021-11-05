library(osmdata)
library(tidyverse)
library(camcorder)
library(sf)
library(wesanderson)
library(ggtext)

gg_record(dir = "2021/temp", device = "png", width = 9.87, height = 13, units = "in", dpi = 320)

city = "Thessaloniki"

maxspeed_osm <- opq(city) %>% 
  add_osm_feature(key = "maxspeed") %>% 
  osmdata_sf()

maxspeed_sf <- maxspeed_osm$osm_lines %>% 
  select(osm_id, name, maxspeed, geometry) %>% 
  mutate(
    maxspeed = case_when(
      str_detect(maxspeed, "rban") ~ 50,
      str_detect(maxspeed, "living") ~ 30,
      maxspeed == 2 ~ 20,
      TRUE ~ as.numeric(maxspeed)
    ),
    maxspeed = ifelse(maxspeed > 130, 130, maxspeed)
  ) %>% 
  filter(!is.na(maxspeed))


bb <- getbb(city)
greece <- read_sf("2021/data/gadm36_GRC_shp/gadm36_GRC_0.shp")

pal <- wes_palette("Zissou1")

f1 = "Futura"
f2 = "Porpora"

ggplot(maxspeed_sf) +
  geom_sf(data = greece, fill = "linen", size = 0.3, color = "orange4") +
  geom_sf(aes(color = maxspeed, size = maxspeed)) +
  scale_color_stepsn(colors = pal, breaks = seq(10, 130, 20)) +
  scale_size_continuous(range = c(0.1, 0.9), guide = "none") +
  coord_sf(xlim = c(bb[1, 1], bb[1, 2]), ylim = c(bb[2, 1], bb[2, 2])) +
  guides(color = guide_colorsteps(label.position = "left", show.limits = TRUE, title = "Speed limit")) +
  annotate("text", bb[1, 1] + 0.05, bb[2, 1] + 0.064, hjust = 0, vjust = 1, size = 12, family = f1, label = city) +
  annotate("text", bb[1, 1] + 0.05, bb[2, 1] + 0.049, hjust = 0, vjust = 1, size = 6, family = f2, label = "Source: OpenStreetMap\nGraphic: Georgios Karamanis", lineheight = 0.9) +
  theme_void(base_family = f2, base_size = 20) +
  theme(
    legend.position = c(0.12, 0.18),
    legend.key.height = unit(1.6, "line"),
    legend.text = element_text(size = 12),
    legend.title = element_text(margin = margin(0, 0, 10, -15)),
    panel.background = element_rect(fill = "lightsteelblue1", color = NA)
  )


