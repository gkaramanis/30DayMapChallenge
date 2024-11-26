library(tidyverse)
library(sf)
library(rvest)
library(camcorder)

gg_record(here::here("30daymap-temp/"), width = 10, height = 7, dpi = 320)

url <- "https://en.wikipedia.org/wiki/1950_FIBA_World_Championship"

fiba_wc <- read_html(url) %>% 
  html_table() %>% 
  .[[134]] %>% 
  janitor::clean_names()

sf_use_s2(FALSE)

world_1945 <- read_sf("https://raw.githubusercontent.com/aourednik/historical-basemaps/refs/heads/master/geojson/world_1945.geojson") %>% 
  janitor::clean_names() %>%
  group_by(name) %>% 
  reframe(geometry = st_union(geometry)) %>% 
  `st_geometry<-`("geometry") %>% 
  left_join(fiba_wc, by = c("name" = "team")) %>% 
  st_buffer(., -0.0001) %>% # Prevent artifacts from reprojecting
  st_transform(crs = '+proj=vandg4 +lon_0=0 +x_0=0 +y_0=0 +R_A +datum=WGS84 +units=m +no_defs') %>% 
  mutate(
    label = paste0(rank, ".", toupper(name), " (", record, ")")
  )

f1 <- "Input Serif Compressed"
f2 <- "Montagu Slab 144pt"

ggplot(world_1945) +
  geom_sf(aes(fill = factor(rank)), linewidth = 0.15, color = "#353231") +
  geom_sf(data = . %>% filter(rank <= 3), fill = NA, color = "#353231", linewidth = 0.3) +
  ggrepel::geom_text_repel(data = . %>% filter(!is.na(rank)), aes(label = label, geometry = geometry, size = rank <= 3), stat = "sf_coordinates", family = f1, force_pull = 0, bg.color = "white", seed = 888) +
  scale_fill_manual(values = c("#FFD700", "#A9A9A9", "#CD7F32", rep("#E8E6D9", 7)), na.value = "grey99") +
  scale_size_manual(values = c(2.7, 4.2)) +
  labs(
    title = "1950 FIBA World Championship",
    subtitle = "Final rankings and records of the inaugural FIBA World Championship",
    caption = "Data: Wikipedia · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f2) +
  theme(
    plot.background = element_rect(fill = "#DCDCDC", color = NA),
    panel.grid = element_line(linewidth = 0.3, color = "#C0C0C0"),
    legend.position = "none",
    plot.title = element_text(size = 24, hjust = 0.5, margin = margin(10, 0, 0, 0), face = "bold"),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(5, 0, 10, 0)),
    plot.caption = element_text(size = 8, color = "#353231", hjust = 0.5, margin = margin(10, 0, 10, 0))
  ) 
