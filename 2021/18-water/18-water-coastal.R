library(tidyverse)
library(sf)
library(camcorder)
library(ggtext)

gg_record(dir = "2021/temp", device = "png", width = 10, height = 9, units = "in", dpi = 320)

# Download data from https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/population-distribution-demography/degurba
coastal <- read_sf("2021/data/DGURBA-2018-01M-SH/DGURBA_2018_01M.shp") %>% 
  filter(str_detect(GISCO_ID, "^EL"))

waters <- read_sf("2021/data/parakties_ydatines_epifaneies/parakties_ydatines_epifaneies.shp")

f1 = "Porpora"

ggplot(coastal) +
  geom_sf(data = waters, size = 0.2, fill = "azure2") +
  geom_sf(aes(fill = ifelse(COASTAL == "Yes", "antiquewhite4", "linen"), color = after_scale(colorspace::darken(fill, 0.5))), size = 0.1) +
  annotate("richtext", 19.5, 35.5, label = "Coastal land areas and<br>coastal water bodies<br><span style = 'font-size:8pt'>Data: Eurostat, geodata.gov.gr - Graphic: Georgios Karamanis</span>", hjust = 0, vjust = 1, color = "azure2", family = f1, size = 7, fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt")) +
  scale_fill_identity() +
  labs() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey40", color = NA)
  )
