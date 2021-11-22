library(sf)
library(tidyverse)
library(camcorder)

greece <- read_sf("2021/data/gadm36_GRC_shp/gadm36_GRC_0.shp")

plates <- read_sf("2021/data/fraxen tectonicplates master GeoJSON/PB2002_boundaries.json")

earthquakes <- read_tsv("2021/data/earthquakes-2021-11-22_05-44-36_+0100.tsv") %>% 
  janitor::clean_names() %>% 
  filter(str_detect(location_name, "GREECE"))

gg_record(dir = "2021/temp", device = "png", width = 10, height = 10, units = "in", dpi = 320)

f1 = "Porpora"

ggplot() +
  geom_sf(data = greece, fill = "#292627", color = NA) +
  geom_sf(data = plates, color = "cadetblue3", size = 1, linetype = "21") +
  geom_point(data = earthquakes, aes(x = longitude, y = latitude, size = mag), color = "darkorange", shape = 21, fill = "orange", stroke = 1, alpha = 0.7) +
  annotate("text", x = 19.5, y = 34.4, label = "Tectonic plate boundaries\nand earthquakes in Greece", hjust = 0, vjust = 1, size = 7, family = f1, fontface = "bold", color = "grey97", lineheight = 0.9) +
  annotate("text", x = 19.5, y = 33.8, label = "Sources: Hugo Ahlenius, Nordpil and Peter Bird (tectonic plates),\nNOAA (Significant Earthquake Database, from 2150 B.C.)\nGraphic: Georgios Karamanis", hjust = 0, vjust = 1, size = 4, family = f1, color = "grey97", lineheight = 1) +
  scale_size_continuous(range = c(0, 5)) +
  coord_sf(xlim = c(19, 30), ylim = c(33, 42), expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#58507E", color = NA)
  )
 