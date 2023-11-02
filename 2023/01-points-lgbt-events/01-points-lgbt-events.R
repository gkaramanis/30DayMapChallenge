library(tidyverse)
library(rvest)
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

coords <- tribble(
  ~city,              ~lat,   ~long,
  "Amsterdam",        52.379189,   4.899431,
  "Atlanta",          33.749001,  -84.387978,
  "Belo Horizonte",   -19.919592, -43.938363,
  "Berlin",           52.520008,   13.404954,
  "Boston",           42.360081,  -71.058884,
  "Brighton",         50.822536,  -0.137163,
  "Brussels",         50.850346,   4.351721,
  "Buenos Aires",     -34.611778, -58.417306,
  "Charlotte",        35.227085,  -80.843124,
  "Chicago",          41.878113,  -87.629799,
  "Cologne",          50.937531,   6.960279,
  "Columbus",         39.961178,  -82.998794,
  "Copenhagen",       55.676098,  12.568337,
  "Denver",           39.739235, -104.990250,
  "Hamburg",          53.550003,   9.992379,
  "Houston",          29.760427, -95.369804,
  "Istanbul",         41.008238,  28.978359,
  "London",           51.507351,  -0.127758,
  "Los Angeles",      34.052235, -118.243683,
  "Madrid",           40.416775,  -3.703790,
  "Melbourne",        -37.813628, 144.963058,
  "Mexico City",      19.432608, -99.133209,
  "Miami",            25.761680, -80.191790,
  "Minneapolis",      44.977753, -93.265015,
  "Montreal",         45.501690, -73.567253,
  "Munich",           48.137154,  11.576124,
  "New Orleans",      29.951065, -90.071533,
  "New York City",    40.712776, -74.005974,
  "Palermo",          38.115688,  13.361267,
  "Paris",            48.856613,   2.352222,
  "Quezon City",      14.676041, 121.043700,
  "Rio de Janeiro",   -22.906846, -43.172897,
  "Rome",             41.902782,  12.496366,
  "San Diego",        32.715736, -117.161087,
  "San Francisco",    37.774929, -122.419416,
  "Seoul",            37.566535, 126.977969,
  "St. Petersburg",   27.770860, -82.679268, # St. Petersburg, US
  "Sydney",           -33.868820, 151.209296,
  "São Paulo",        -23.550520, -46.633308,
  "Taipei",           25.032969, 121.565414,
  "Tel Aviv",         32.085300,  34.781769,
  "Tokyo",            35.682839, 139.759455,
  "Toronto",          43.651070, -79.347015,
  "Vienna",           48.208176,  16.373819
)
  
events_coords <- events %>% 
  left_join(coords) %>% 
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
  
