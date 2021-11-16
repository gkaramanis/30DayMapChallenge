library(tidyverse)
library(sf)
library(camcorder)

gg_record(dir = "2021/temp", device = "png", width = 10, height = 10, units = "in", dpi = 320)

kalamaria <- read_sf("~/Downloads/oikodomika_tetragwna/oikodomika_tetragwna.shp")

kalamaria_streets <- read_sf("~/Downloads/odikoi_axones/odikoi_axones.shp")

kalamaria_numbers <- read_sf("~/Downloads/arithmhsh_dromwn/arithmhsh_dromwn.shp", options = "ENCODING=WINDOWS-1253")
  
kalamaria_min_nr <- kalamaria_numbers %>% 
  mutate(nr = as.numeric(Arithmos)) %>% 
  group_by(Odos) %>% 
  slice_min(nr) %>% 
  ungroup()

ggplot(kalamaria) +
  geom_sf(fill = "grey90", size = 0.25, color = "grey80") +
  geom_sf(data = kalamaria_streets, color = "purple", size = 0.5, alpha = 0.7) +
  geom_sf_text(data = kalamaria_min_nr, aes(label = nr, color = Odos), size = 5) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA)
  )

