library(tidyverse)
library(sf)
library(imago)
library(countrycode)
library(camcorder)

gg_record(here::here("30daymap-temp"), width = 12, height = 7, dpi = 320)

# 2021-11-18 - 2022-11-18
trends <- read_csv(here::here("2022/data/dataviz_trends.csv")) %>% 
  rename(country = 1, interest = 2) %>% 
  filter(!is.na(interest)) %>% 
  mutate(iso3c = countrycode(country, origin = "country.name", destination = "iso3c"))

world <- imago::imago() %>% 
  filter(st_geometry_type(geometry) != "POINT") %>% 
  left_join(trends)

pal <- scico::scico(n = 10, palette = "lajolla", end = 0.8)

ggplot(world) +
  geom_sf(aes(fill = interest), linewidth = 0.1, color = "#002C3B") +
  scale_fill_stepsn(colors = pal, na.value = "grey95", breaks = seq(0, 100, 10)) +
  # scico::scale_fill_scico(palette = "lajolla", end = 0.8, na.value = "grey97") +
  labs(
    title = "Search interest for 'data visualization'",
    subtitle = "November 2021 to November 2022",
    caption = "Source: Google Trends · Graphic: Georgios Karamanis",
    fill = NULL
  ) +
  theme_void(base_family = "Outfit") +
  theme(
    legend.position = "top",
    legend.key.width = unit(4, "line"),
    legend.key.height = unit(0.7, "lines"),
    legend.text = element_text(color = "#096884"),
    plot.background = element_rect(fill = "#A2D0E9", color = NA),
    plot.title = element_text(hjust = 0.5, color = "#096884", face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(5, 0, 10, 0), color = "#096884"),
    plot.caption = element_text(hjust = 0.5, color = "#096884")
  )
