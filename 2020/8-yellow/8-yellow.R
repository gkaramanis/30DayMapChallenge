library(tidyverse)
library(sf)
library(janitor)

roads <- read_sf(here::here("2020", "data", "tk_03_Sweref_99_TM_shape", "terrang", "03", "vl_03.shp"), options = "ENCODING=WINDOWS-1252") %>% 
  clean_names() %>% 
  st_transform(4326)

roads %>% 
  mutate(size = case_when(
    startsWith(kategori, "Motorväg") ~ 0.3,
    TRUE ~ 0.125
  )) %>% 
  ggplot() +
  geom_sf(aes(size = size), color = "grey20", alpha = 1) +
  scale_size_identity() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#FBE44D", color = NA)
  ) +
  ggsave(here::here("2020", "8-yellow", "8-yellow.png"), dpi = 320, height = 6, width = 4.93)
  