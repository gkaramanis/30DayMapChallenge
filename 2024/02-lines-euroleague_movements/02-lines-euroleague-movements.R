library(tidyverse)
library(sf)
library(rvest)
library(geosphere)
library(marquee)
library(camcorder)

gg_record(here::here("30daymap-temp/"), width = 10, height = 8.5, dpi = 320)

team_locations <- tribble(
  ~team,              ~latitude,  ~longitude,
  "ALBA Berlin",      52.5200,    13.4050,    # Berlin, Germany
  "Anadolu Efes",     41.0082,    28.9784,   # Istanbul, Turkey
  "ASVEL",            45.7640,    4.8357,    # Lyon, France
  "Barcelona",        41.3851,    2.1734,    # Barcelona, Spain
  "Baskonia",         42.8467,    -2.6716,   # Vitoria-Gasteiz, Spain
  "Bayern Munich",    48.1351,    11.5820,   # Munich, Germany
  "Crvena Zvezda",    44.8125,    20.4612,   # Belgrade, Serbia
  "Fenerbahçe",       41.0082,    28.9784,   # Istanbul, Turkey
  "Maccabi Tel Aviv", 32.0853,    34.7818,   # Tel Aviv, Israel
  "Monaco",           43.7384,    7.4246,    # Monaco
  "Olimpia Milano",   45.4642,    9.1900,    # Milan, Italy
  "Olympiacos",       37.9838,    23.7275,   # Piraeus, Greece
  "Panathinaikos",    37.9838,    23.7275,   # Athens, Greece
  "Paris",            48.8566,    2.3522,    # Paris, France
  "Partizan",         44.8125,    20.4612,   # Belgrade, Serbia
  "Real Madrid",      40.4168,    -3.7038,   # Madrid, Spain
  "Virtus Bologna",   44.4949,    11.3426,   # Bologna, Italy
  "Žalgiris",         54.8985,    23.9036   # Kaunas, Lithuania
)

url <- "https://en.wikipedia.org/wiki/List_of_2024–25_EuroLeague_transactions#Between_two_EuroLeague_teams"

movements_between_raw <- read_html(url) %>% 
  html_table() %>% 
  .[[3]] %>% 
  janitor::clean_names() %>% 
  mutate(to = case_when(
    to == "Crvena zvezda" ~ "Crvena Zvezda",
    TRUE ~ to
  ))

movements_between <- movements_between_raw %>% 
  left_join(team_locations, by = c("from" = "team")) %>% 
  left_join(team_locations, by = c("to" = "team"), suffix = c(".from", ".to")) %>% 
  add_count(from) %>% 
  add_count(from, to) %>% 
  add_count(to, from) %>% 
  add_count(to) %>% 
  # add <> to links because of spaces in some team names
  mutate(
    from_l = paste0("![](<", here::here("2024/data/euroleague/"), from, ".png>)"),
    from_logo = paste0(from_l, " ", from, " (", n, ")"),
    from_label = paste0(from, if_else(nn > 1, paste0(" (", nnn, ")"), "")),
    to_l = paste0("![](<", here::here("2024/data/euroleague/"), to, ".png>)"),
    to_logo = paste0(to_l, " ", to, " (", nnnn, ")"),
    to_label = paste0(to, if_else(nn > 1, paste0(" (", nn, ")"), ""))
  )

movements_sf <- movements_between %>% 
  rowwise() %>%
  mutate(
    points = list(
      gcIntermediate(
        p1 = c(longitude.from, latitude.from),
        p2 = c(longitude.to, latitude.to),
        n = 5,
        addStartEnd = TRUE
      )
    ),
    geometry = list(st_linestring(points))) %>%
  ungroup() %>% 
  st_as_sf(crs = 4326)

world <- read_sf(here::here("2022/data/world.geo.json"))

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Publico Headline"

# Movements to
ggplot(movements_sf) +
  geom_sf(data = world, fill = "#F0EDE6", linewidth = 0.1, color = "#3C3836") +
  geom_point(aes(x = longitude.from, y = latitude.from), stat = "unique") +
  geom_sf(arrow = arrow(length = unit(0.2, "cm")), linewidth = 0.4) +
  ggrepel::geom_text_repel(aes(x = longitude.to, y = latitude.to, label = to_label), stat = "unique", size = 3, family = f1b, bg.color = "white", seed = 2024) +
  facet_wrap(vars(from_logo)) +
  coord_sf(xlim = c(-15, 45), ylim = c(30, 60)) +
  labs(
    title = "Where did they go? Player movements between Euroleague teams 2024-25",
    caption = "Source: Wikipedia · Graphic: Georgios Karamanis"
  ) +
  theme_dark(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.border = element_rect(fill = NA, color = "black"),
    panel.background = element_rect(fill = "#E8ECEF"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    strip.text = element_marquee(size = 10, colour = "white", margin = margin(5, 0, -5, 0), family = f1b),
    strip.background = element_rect(fill = "#3C3836", color = "black"),
    plot.title = element_text(family = f2, size = 17, face = "bold", hjust = 0.5),
    plot.margin = margin(10, 5, 5, 5)
  )
  

# Movements from
ggplot(movements_sf) +
  geom_sf(data = world, fill = "#F0EDE6", linewidth = 0.1, color = "#3C3836") +
  geom_point(aes(x = longitude.from, y = latitude.from), stat = "unique") +
  geom_sf(arrow = arrow(length = unit(0.2, "cm")), linewidth = 0.4) +
  ggrepel::geom_text_repel(aes(x = longitude.from, y = latitude.from, label = from_label), stat = "unique", size = 3, family = f1b, bg.color = "white", seed = 2024) +
  facet_wrap(vars(to_logo)) +
  coord_sf(xlim = c(-15, 45), ylim = c(30, 60)) +
  labs(
    title = "Where did they come from? Player movements between Euroleague teams 2024-25",
    caption = "Source: Wikipedia · Graphic: Georgios Karamanis"
  ) +
  theme_dark(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.border = element_rect(fill = NA, color = "black"),
    panel.background = element_rect(fill = "#E8ECEF"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    strip.text = element_marquee(size = 10, colour = "white", margin = margin(5, 0, -5, 0), family = f1b),
    strip.background = element_rect(fill = "#3C3836", color = "black"),
    plot.title = element_text(family = f2, size = 17, face = "bold", hjust = 0.5),
    plot.margin = margin(10, 5, 5, 5)
  )
