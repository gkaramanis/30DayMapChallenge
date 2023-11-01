library(tidyverse)
library(rvest)
library(tidygeocoder)
library(camcorder)

gg_record(here::here("30daymap-temp/"), width = 8, height = 8, dpi = 320)

events_raw <- read_html("https://en.wikipedia.org/wiki/List_of_largest_LGBT_events") %>% 
  html_table() %>% 
  .[[4]] %>% 
  janitor::clean_names()

events <- events_raw %>% 
  mutate(
    rank = str_remove(rank, " \\(tie\\)"),
    organizers_statistics = str_remove(organizers_statistics, "\\+*\\[.+"),
    organizers_statistics = parse_number(organizers_statistics),
    authorities_statistics = str_remove(authorities_statistics, "\\+*\\[.+"),
    authorities_statistics = parse_number(authorities_statistics),
    attendance = if_else(!is.na(organizers_statistics), organizers_statistics, authorities_statistics)
  ) %>% 
  group_by(city) %>% 
  slice_max(order_by = year, n = 1) %>% 
  ungroup()

world <- rnaturalearthdata::countries50 %>% 
  sf::st_as_sf()

coords <- geo(events$city)

events_coords <- events %>% 
  left_join(coords, by = c("city" = "address")) %>% 
  sf::st_as_sf(coords = c("long", "lat"), crs = sf::st_crs(world))

f1 <- "Outfit"
f2 <- "JetBrains Mono"

ggplot(events_coords) +
  geom_sf(data = world, linewidth = 0.1, fill = "#FFFBFF", color = "#180025") +
  ggrepel::geom_text_repel(aes(label = paste0(city, "\n", scales::number(attendance, accuracy = 0.1, scale_cut = scales::cut_short_scale()), " (", year, ")"), geometry = geometry), stat = "sf_coordinates", family = f2, size = 2.5, max.overlaps = 16, seed = 99, segment.size = 0.2, point.padding = 0.1, bg.color = "#F8F8FF") +
  geom_sf(aes(size = attendance, color = type), alpha = 0.5) +
  # Title
  annotate("text", 0, 0.95e7, label = "Largest\nLGBT events", size = 15, family = f1, lineheight = 0.9, fontface = "bold", color = "#180025") +
  # Caption
  annotate("text", 5e5, -1.1e7, label = "Source: Wikipedia's list of largest LGBT events\nGraphic: Georgios Karamanis", size = 3.2, family = f1, lineheight = 0.9, color = "#180025") +
  MetBrewer::scale_color_met_d("Lakota") +
  coord_sf(crs = "+proj=adams_ws1", expand = FALSE) +
  guides(
    size = FALSE,
    color = guide_legend(override.aes = list(size = 3))
    ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = c(0.58, 0.17),
    legend.title = element_blank(),
    plot.background = element_rect(fill = "#F8F8FF", color = NA),
    panel.grid = element_line(size = 0.1, color = "purple4")
  )
  
