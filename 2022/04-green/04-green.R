library(tidyverse)
library(sf)
library(ggtext)
library(camcorder)

gg_record(here::here("30daymap-temp"), width = 12, height = 7, dpi = 320)

# https://www.worldatlas.com/articles/country-flags-with-green.html
green_flags <- read_tsv(here::here("2022/data/green-flags.tsv"))

# https://datahub.io/core/geo-countries#resource-geo-countries_zip
world <- read_sf(here::here("2022/data/countries.geojson")) %>% 
  mutate(color = if_else(ADMIN %in% green_flags$country, TRUE, FALSE))

world_simpl <- world %>% 
  rmapshaper::ms_simplify(keep = 0.2)

ggplot(world_simpl) +
  geom_sf(aes(fill = color), size = 0.15, color = "#4A2C7C") +
  scale_fill_manual(values = c("#F9F7FF", "darkolivegreen3")) +
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  labs(
    title = "Countries with <span style='color:darkolivegreen4'>green</span> in their flag",
    caption = "Sources: WorldAtlas & Datahub · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = "Outfit") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.grid = element_line(color = "#8B6ACC", size = 0.1),
    axis.text = element_text(color = "grey70"),
    plot.caption = element_text(hjust = 0.5),
    plot.title = element_markdown(hjust = 0.5, size = 20, family = "Futura", face = "bold"),
    plot.margin = margin(10, 20, 10, 20)
  )
  
