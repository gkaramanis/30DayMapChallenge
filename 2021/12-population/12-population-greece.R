library(tidyverse)
library(sf)
library(camcorder)
library(scico)
library(cartogram)

gg_record(dir = "2021/temp", device = "png", width = 8, height = 10, units = "in", dpi = 320)

muni <- read_sf("2021/data/kallikratikoi_dimoi/ΚΑΛΛΙΚΡΑΤΙΚΟΙ_ΔΗΜΟΙ.shp") %>% 
  janitor::clean_names() %>% 
  filter(nchar(kalcode) == 4)

census_2011 <- readxl::read_xls("2021/data/Kallikratis_me_plithismous_1991_2011.xls", skip = 1) %>% 
  janitor::clean_names() %>% 
  filter(nchar(kodikos_20_3_2011) == 4) %>% 
  select(kalcode = kodikos_20_3_2011, pop = plethysmoi) %>% 
  mutate(pop = as.numeric(pop))

muni_pop <- muni %>% 
  left_join(census_2011) %>% 
  select(pop, geometry)

# This step takes long time!
muni_pop_carto_cont <- cartogram_cont(muni_pop, weight = "pop")

options(scipen=10000)

f1 = "Porpora"
f2 = "Publico Headline"

ggplot(muni_pop_carto_cont) +
  geom_sf(aes(fill = pop), size = 0.1, color = "black") +
  scale_fill_scico(palette = "devon", direction = -1, end = 0.975, labels = function(x) format(x, big.mark = " ", trim = TRUE), name = NULL) +
  labs(
    title = "2011 Greek Census",
    subtitle = "Population by municipality",
    caption = "Data: Hellenic Statistical Authority · Graphic: Georgios Karamanis"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(3, "line"),
    legend.key.height = unit(0.5, "line"),
    legend.text = element_text(family = f1),
    plot.background = element_rect(fill = "#FFFEFD", color = NA),
    plot.title = element_text(size = 30, family = f1, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 16, family = f2, hjust = 0.5),
    plot.caption = element_text(size = 10, family = f2, hjust = 0.5, margin = margin(20, 0, 0, 0))
  )
