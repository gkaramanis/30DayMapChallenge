library(tidyverse)
library(sf)
library(ggsn)
library(camcorder)

gg_record(here::here("30daymap-temp"), width = 12, height = 7, dpi = 320)

# https://web.archive.org/web/20130728230041/http://library.thinkquest.org/C006628/data/1279.zip
fantasy <- read_sf(here::here("2022/Data/1279/cntry1279.shp")) %>% 
  janitor::clean_names() %>% 
  mutate(area = st_area(geometry)) %>% 
  mutate(name2 = if_else(name == "unclaimed" & abbrevname != "", abbrevname, name))
  
f1 <- "Broadsheet"

ggplot(fantasy) +
  geom_sf(fill = NA, color = "#A7AEA7", linewidth = 4) +
  geom_sf(fill = NA, color = "#B04A4ABB", linewidth = 1) +
  geom_sf(fill = "#E8D497", color = "#B04A4A", linewidth = 0.1) +
  geom_sf_text(aes(label = name2, size = area), check_overlap = TRUE, family = f1) +
  scale_size_continuous(range = c(3, 8)) +
  north(fantasy, symbol = 14, scale = 0.12, location = "bottomright") +
  labs(
    title = "The World, 1279",
    caption = "Source: library.thinkquest.org (via Internet Archive) · Graphic: Georgios Karamanis"
    ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#BAC4BA", color = NA),
    plot.title = element_text(hjust = 0.5, size = 30, margin = margin(0, 0, 10, 0)),
    plot.caption = element_text(hjust = 0.5, margin = margin(10, 0, 0, 0), size = 11)
  )
