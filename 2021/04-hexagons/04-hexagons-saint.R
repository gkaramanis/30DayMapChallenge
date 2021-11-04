library(tidyverse)
library(camcorder)
library(wesanderson)
library(sf)
library(ggtext)

gg_record(dir = "2021/temp", device = "png", width = 10, height = 12, units = "in", dpi = 320)

col_names = c("geonameid", "name", "asciiname", "alternatenames", "latitude", "longitude", "feature class", "feature code", "country code", "cc2", "admin1 code", "admin2 code", "admin3 code", "admin4 code", "population", "elevation", "dem", "timezone", "modification date")

gr_ppl <- read_tsv("2021/data/GR/GR.txt", col_names = col_names) %>%
  janitor::clean_names() %>% 
  filter(feature_code == "PPL")

gr_saint <- gr_ppl %>% 
  filter(str_detect(asciiname, "\\bA[gy]h*i[oa]"))

pal <- wes_palette("Zissou1")

boundary <- read_sf("2021/data/gadm36_GRC_shp/gadm36_GRC_0.shp")

ggplot(gr_saint) +
  geom_sf(data = boundary, size = 0.15, fill = "transparent", color = "grey50") +
  geom_hex(aes(x = longitude, y = latitude), bins = 30, color = "grey15", alpha = 0.8) +
  scale_fill_stepsn(colors = pal, n.breaks = 6) +
  labs(
    title = '"Holy" Places of Greece',
    subtitle = "Names of populated places with<br>the words *Saint, Santa,* or *Holy (Άγιος, Αγία, Άγιο)* ",
    caption = "Source: GeoNames · Graphic: Georgios Karamanis"
  ) +
  coord_sf() +
  theme_void(base_family = "Cochin") +
  theme(
    legend.position = "bottom",
    legend.key.height = unit(0.8, "lines"),
    legend.title = element_blank(),
    plot.background = element_rect(fill = "#FFF8E7", color = NA),
    plot.title = element_text(size = 30, hjust = 0.5, face = "bold"),
    plot.subtitle = element_markdown(size = 20, hjust = 0.5, lineheight = 1.1),
    plot.caption = element_text(size = 13, hjust = 0.5, margin = margin(20, 0, 0, 0))
  )
