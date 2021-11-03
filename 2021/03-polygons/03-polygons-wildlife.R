library(ggplot2)
library(camcorder)
library(sf)
library(ggpattern)
library(ggtext)

gg_record(dir = "2021/temp", device = "png", width = 12, height = 10, units = "in", dpi = 320)

wildlife <- read_sf("2021/data/katafygia_agrias_zwhs/katafygia_agrias_zwhs.shp") %>% 
  rmapshaper::ms_simplify()

boundary <- read_sf("2021/data/gadm36_GRC_shp/gadm36_GRC_0.shp")

ggplot(wildlife) +
  geom_sf(data = boundary, size = 0.3, fill = "mediumpurple3") +
  geom_sf_pattern(pattern = "stripe", pattern_spacing = 0.002, pattern_density = 0.9, pattern_fill = "yellow", pattern_size = 0.2) +
  labs(
    caption = "**Wildlife<br>Sanctuaries of Greece**<br><span style='font-size:18pt'>Source: geodata.gov.gr · Graphic: Georgios Karamanis</span>"
  ) +
  theme_void(base_family = "Futura") +
  theme(
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.caption = element_markdown(size = 28, hjust = 0, margin = margin(-100, 0, 0, 0)),
    plot.margin = margin(0, 0, 0, 0)
  )
