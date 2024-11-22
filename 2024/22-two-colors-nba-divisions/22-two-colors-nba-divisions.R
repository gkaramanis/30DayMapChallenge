library(tidyverse)
library(sf)
library(rvest)
library(ggpattern)
library(rnaturalearth)
library(camcorder)

gg_record(here::here("30daymap-temp/"), width = 8, height = 8, dpi = 320)

proj_string <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" 

na_map <- ne_states(country = c("united states of america", "canada"), returnclass = "sf") %>% 
  filter(!name %in% c("Alaska", "Hawaii", "Yukon", "Northwest Territories", "Nunavut")) %>% 
  select(name) %>% 
  st_transform(proj_string) 

url <- "https://en.wikipedia.org/wiki/National_Basketball_Association"

nba_div_raw <- read_html(url) %>% 
  html_table() %>% 
  .[[3]] %>% 
  janitor::clean_names()

nba_div <- nba_div_raw %>%
  mutate(
    lat = str_extract(coordinates, "\\d+\\.\\d+°N"),
    lon = str_extract(coordinates, "-?\\d+\\.\\d+(?=°W)"),
    lat = as.numeric(str_remove(lat, "°N")),
    lon = as.numeric(lon) * -1,  # Make longitude negative for Western hemisphere
    state = str_extract(location, ",\\s*([^,]+)$") %>% str_remove("^,\\s*"),
    state = if_else(state == "D.C", "District of Columbia", state)
  )

na_nba_div <- na_map %>% 
  left_join(nba_div, by = c("name" = "state"))

divisions_sf <- nba_div %>% 
  st_as_sf(coords = c("lon", "lat")) %>% 
  st_set_crs(4326) %>% 
  st_transform(proj_string) %>% 
  group_by(division) %>% 
  summarize(geometry = st_union(geometry)) %>% 
  st_centroid()

conferences_sf <- nba_div %>% 
  st_as_sf(coords = c("lon", "lat")) %>% 
  st_set_crs(4326) %>% 
  st_transform(proj_string) %>% 
  group_by(conference) %>% 
  summarize(geometry = st_union(geometry)) %>% 
  st_centroid()

f1 <- "Sofia Sans Extra Condensed"

ggplot(na_nba_div) +
  geom_sf(fill = "white") +
  geom_sf(data = . %>% filter(!is.na(division)), aes(
    fill = interaction(division, conference)
  ),
  linewidth = 0.6,
  color = "black",
  show.legend = FALSE
  ) +
  geom_sf_pattern(data = . %>% filter(conference == "Western"), aes(pattern_angle = division), pattern_spacing = 0.015, pattern_density = 0.01, pattern_color = "white", fill = NA, color = "black", show.legend = FALSE) +
  geom_sf_pattern(data = . %>% filter(conference == "Eastern"), aes(pattern_angle = division), pattern_spacing = 0.015, pattern_density = 0.01, pattern_color = "black", fill = NA, color = "black", show.legend = FALSE) +
  geom_sf(data = . %>% filter(!is.na(division)),
  linewidth = 0.6,
  color = "black",
  fill = NA
  ) +
  ggrepel::geom_text_repel(data = divisions_sf, aes(label = division, geometry = geometry), stat = "sf_coordinates", bg.color = "white", family = f1, fontface = "bold", size = 6, seed = 9) +
  ggrepel::geom_text_repel(data = conferences_sf, aes(label = toupper(conference), geometry = geometry, color = conference), stat = "sf_coordinates", bg.color = "white", family = f1, fontface = "bold", size = 10, seed = 99, force_pull = 100) +
  scale_linewidth_identity() +
  scale_pattern_angle_discrete(range = c(0, 200)) +
  scale_fill_manual(values = paste0("grey", c(20, 35, 50, 60, 75, 85)), na.value = "white") +
  # scale_fill_grey(na.value = "white", start = 0.4, end = 0.85) +
  scale_color_grey(na.value = "white", start = 0, end = 0.45) +
  labs(
    title = "NBA Conferences & Divisions",
    caption = "Source: Wikipedia · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 0.5, size = 12)
  )
