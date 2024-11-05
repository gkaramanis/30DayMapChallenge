library(tidyverse)
library(sf)
library(rgeoboundaries)
library(geosphere)
library(geomtextpath)
library(camcorder)

gg_record(here::here("30daymap-temp/"), width = 10, height = 7.5, dpi = 320)

nba_champions <- tribble(
  ~year, ~team,                  ~city,          ~longitude,  ~latitude,
  2004, "Detroit Pistons",       "Detroit",      -83.0458,    42.3314,
  2005, "San Antonio Spurs",     "San Antonio",  -98.4936,    29.4241,
  2006, "Miami Heat",            "Miami",        -80.1918,    25.7617,
  2007, "San Antonio Spurs",     "San Antonio",  -98.4936,    29.4241,
  2008, "Boston Celtics",        "Boston",       -71.0588,    42.3601,
  2009, "Los Angeles Lakers",    "Los Angeles",  -118.2437,   34.0522,
  2010, "Los Angeles Lakers",    "Los Angeles",  -118.2437,   34.0522,
  2011, "Dallas Mavericks",      "Dallas",       -96.7970,    32.7767,
  2012, "Miami Heat",            "Miami",        -80.1918,    25.7617,
  2013, "Miami Heat",            "Miami",        -80.1918,    25.7617,
  2014, "San Antonio Spurs",     "San Antonio",  -98.4936,    29.4241,
  2015, "Golden State Warriors", "Oakland",      -122.2712,   37.8044,
  2016, "Cleveland Cavaliers",   "Cleveland",    -81.6944,    41.4993,
  2017, "Golden State Warriors", "Oakland",      -122.2712,   37.8044,
  2018, "Golden State Warriors", "Oakland",      -122.2712,   37.8044,
  2019, "Toronto Raptors",       "Toronto",      -79.3832,    43.6532,
  2020, "Los Angeles Lakers",    "Los Angeles",  -118.2437,   34.0522,
  2021, "Milwaukee Bucks",       "Milwaukee",    -87.9065,    43.0389,
  2022, "Golden State Warriors", "San Francisco",-122.4194,   37.7749,
  2023, "Denver Nuggets",        "Denver",       -104.9903,   39.7392,
  2024, "Boston Celtics",        "Boston",       -71.0588,    42.3601
)

# Calculate distances
distances <- nba_champions %>% 
  filter(year > 2018) %>% 
  mutate(
    previous_longitude = lag(longitude),
    previous_latitude = lag(latitude),
    from_city = lag(city),
    to_city = city
  ) %>%
  rowwise() %>% 
  mutate(distance_km = distGeo(c(previous_longitude, previous_latitude), c(longitude, latitude)) / 1000) %>% 
  filter(!is.na(distance_km)) %>%
  select(year, from_city, city = to_city, distance_km) %>% 
  mutate(distance_label = paste0(scales::number(round(distance_km)), " km"))

# Reproject points and join distances
nba_champions_sf <- nba_champions %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs") %>% 
  mutate(
    longitude = sf::st_coordinates(.)[,1],
    latitude = sf::st_coordinates(.)[,2],
    xend = longitude,
    x = lag(longitude),
    yend = latitude,
    y = lag(latitude)
  ) %>% 
  left_join(distances)

# Fonts
f1 <- "Gabarito"
f2 <- "Graphik Compact"

# US states
states <- rnaturalearth::ne_states(country = "United States of America") %>% 
  st_transform(crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")

# World
world <- rnaturalearth::ne_countries(scale = 10) %>% 
  st_transform(crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs") %>% 
  rmapshaper::ms_dissolve()

# Colors
map_colors <- list(
  background = "#f5f5f5",
  water = "#e6e8ed",
  land = "#ffffff",
  states = "#d8d8d8",
  points = "#2d4059",
  arrows = "#c75d47"
)

ggplot(nba_champions_sf) +
  geom_sf(data = world, linewidth = 0.25, fill = map_colors$water, color = NA) +
  geom_sf(data = states, fill = map_colors$land, color = map_colors$states, linewidth = 0.25) +
  geom_point(
    data = . %>% filter(year > 2018), 
    aes(longitude, latitude), 
    size = 4,
    color = map_colors$points
  ) +
  geom_curve(
    data = . %>% filter(year > 2019), 
    aes(x = x, y = y, xend = xend, yend = yend), 
    arrow = arrow(length = unit(0.4, "cm")), 
    linewidth = 1.5, 
    color = alpha(map_colors$arrows, 0.2), 
    curvature = 0.35,
    lineend = "round"
  ) +
  geom_textcurve(
    data = . %>% filter(year > 2019), 
    aes(x = x, y = y, xend = xend, yend = yend, label = distance_label), 
    arrow = arrow(length = unit(0.4, "cm")), 
    linewidth = 0.8,
    family = f1,
    size = 4.5,
    vjust = 0.6,
    color = map_colors$arrows,
    curvature = 0.35,
    lineend = "round"
  ) +
  ggrepel::geom_text_repel(
    data = . %>% filter(year > 2018),
    aes(longitude, latitude, label = paste(year, city)), 
    family = f1, 
    size = 4,
    fontface = "bold",
    bg.color = alpha("white", 0.8),
    bg.r = 0.15,
    point.padding = 1,
    seed = 999
  ) +
  coord_sf(xlim = c(-2.1e6, 2.4e6), ylim = c(-2.2e6, 0.8e6)) +
  labs(
    title = glue::glue("The NBA trophy has traveled {scales::number(round(sum(distances$distance_km)))} kilometers ({scales::number(round(sum(distances$distance_km) / 1.609))} miles) since the 2019/2020 season"),
    caption = "Source: Wikipedia · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f2) +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(0, 0, 10, 0)),
    plot.caption = element_text(hjust = 0, size = 10),
    plot.margin = margin(10, 10, 10, 10)
  )
  
