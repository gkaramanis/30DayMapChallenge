library(tidyverse)
library(sf)
library(rvest)
library(camcorder)

gg_record(here::here("30daymap-temp/"), width = 11, height = 8, dpi = 320)

# Scrape NBA team relocation data from Wikipedia
url <- "https://en.wikipedia.org/wiki/List_of_relocated_NBA_teams"

nba_relocated_raw <- read_html(url) %>% 
  html_table() %>% 
  .[[2]] %>% 
  janitor::clean_names()
  
nba_relocated <- nba_relocated_raw %>% 
  mutate(
    team = str_remove(team, "\\^"),
    relocated_to = str_remove(relocated_to, "\\*|§"),
    relocated_to = case_when(
      str_detect(relocated_to, "Tampa") ~ "Tampa",
      TRUE ~ relocated_to
    )
  )

# Create a manual dataset of team locations with coordinates
nba_teams <- tribble(
  ~team, ~city, ~longitude, ~latitude,
  "Tri-Cities Blackhawks", "Moline", -90.5151, 41.5067,
  "Milwaukee Hawks", "Milwaukee", -87.9065, 43.0389,
  "Fort Wayne Pistons", "Fort Wayne", -85.1289, 41.0793,
  "Rochester Royals", "Rochester", -77.6109, 43.1566,
  "Minneapolis Lakers", "Minneapolis", -93.2650, 44.9778,
  "Philadelphia Warriors", "Philadelphia", -75.1652, 39.9526,
  "Philadelphia 76ers", "Philadelphia", -75.1652, 39.9526,
  "Chicago Zephyrs", "Chicago", -87.6298, 41.8781,
  "Syracuse Nationals", "Syracuse", -76.1474, 43.0481,
  "St. Louis Hawks", "St. Louis", -90.1994, 38.6270,
  "New Jersey Americans", "Newark", -74.1724, 40.7357,
  "New Jersey Nets", "Brooklyn", -73.9442, 40.6782,
  "San Diego Rockets", "San Diego", -117.1611, 32.7157,
  "Cincinnati Royals", "Cincinnati", -84.5120, 39.1031,
  "Baltimore Bullets", "Baltimore", -76.6122, 39.2904,
  "Kansas City–Omaha Kings", "Kansas City", -94.5786, 39.0997,
  "Kansas City Kings", "Kansas City", -94.5786, 39.0997,
  "New York Nets", "New York City", -74.0060, 40.7128,
  "Buffalo Braves", "Buffalo", -78.8784, 42.8864,
  "New Orleans Jazz", "New Orleans", -90.0715, 29.9511,
  "New Orleans Hornets", "New Orleans", -90.0715, 29.9511,
  "San Diego Clippers", "San Diego", -117.1611, 32.7157,
  "Vancouver Grizzlies", "Vancouver", -123.1207, 49.2827,
  "Charlotte Hornets", "Charlotte", -80.8431, 35.2271,
  "New Orleans/Oklahoma City Hornets", "New Orleans", -90.0715, 29.9511,
  "Seattle SuperSonics", "Seattle", -122.3321, 47.6062,
  "Toronto Raptors", "Toronto", -79.3832, 43.6532,
  "Detroit Pistons", "Detroit", -83.0458, 42.3314,
  "Los Angeles Lakers", "Los Angeles", -118.2437, 34.0522,
  "San Francisco Warriors", "San Francisco", -122.4194, 37.7749,
  "Capital Bullets", "Washington D.C.", -77.0369, 38.9072,
  "Atlanta Hawks", "Atlanta", -84.3880, 33.7490,
  "Houston Rockets", "Houston", -95.3698, 29.7604,
  "Utah Jazz", "Salt Lake City", -111.8910, 40.7608,
  "Los Angeles Clippers", "Los Angeles", -118.2437, 34.0522,
  "Sacramento Kings", "Sacramento", -121.4944, 38.5816,
  "Memphis Grizzlies", "Memphis", -90.0490, 35.1495,
  "Oklahoma City Thunder", "Oklahoma City", -97.5164, 35.4676,
  "Brooklyn Nets", "Brooklyn", -73.9442, 40.6782,
  "Tampa", "Tampa", -82.4572, 27.9506
)

# Join the datasets to create origin-destination pairs with coordinates
nba_fromto <- nba_relocated %>% 
  select(team, last, relocated_to) %>% 
  left_join(nba_teams, by = c("team" = "team")) %>% 
  left_join(nba_teams, by = c("relocated_to" = "team")) %>% 
  mutate(distance = sqrt((longitude.y - longitude.x)^2 + (latitude.y - latitude.x)^2))

# Set up fonts
f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Publico Headline"

# Get US map data for background
us <- rnaturalearth::ne_countries(continent = "North America", scale = 10)

# Set up Lambert Azimuthal Equal Area projection
target_proj <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"

# Convert the data to spatial features and transform to target projection
nba_fromto_sf <- nba_fromto %>%
  filter(distance > 25) %>%
  # Create sf points for start and end locations
  mutate(
    start_point = map2(longitude.x, latitude.x, ~st_point(c(.x, .y))),
    end_point = map2(longitude.y, latitude.y, ~st_point(c(.x, .y)))
  ) %>%
  mutate(
    start_point = st_sfc(start_point, crs = 4326),
    end_point = st_sfc(end_point, crs = 4326)
  ) %>%
  # Transform to target projection
  mutate(
    start_point = st_transform(start_point, target_proj),
    end_point = st_transform(end_point, target_proj)
  ) %>%
  # Extract coordinates
  mutate(
    x = sf::st_coordinates(start_point)[,1],
    y = sf::st_coordinates(start_point)[,2],
    xend = sf::st_coordinates(end_point)[,1],
    yend = sf::st_coordinates(end_point)[,2]
  )

# Create plot
ggplot(nba_fromto_sf) +
  # Add US map background
  geom_sf(data = us, fill = "#f8f7f4", color = "#2f3037") +
  # Add points for origin and destination cities
  geom_point(aes(x, y, color = team), size = 5, alpha = 0.3) +
  geom_point(aes(xend, yend, color = team), size = 10, alpha = 0.2) +
  # Add curved arrows with team names along the path
  geomtextpath::geom_textcurve(
    aes(x = x, xend = xend, y = y, yend = yend, label = team, color = team),
    size = 4.5,
    arrow = arrow(length = unit(0.3, "cm")), 
    family = f1b, 
    curvature = 0.1, 
    vjust = 0.6, 
    hjust = 0.05
  ) +
  # Add destination city labels
  ggrepel::geom_text_repel(
    aes(xend, yend, label = paste0(relocated_to, "\n", last)), 
    size = 4.5,
    family = f1b, 
    lineheight = 1,
    nudge_y = -100000, 
    min.segment.length = 10, 
    bg.color = "white", 
    bg.r = 0.15,
    seed = 999, 
    fontface = "bold",
    color = "#1d3557"
  ) +
  MetBrewer::scale_color_met_d("Lakota", direction = -1) +
  # Set map projection and limits
  coord_sf(crs = target_proj, xlim = c(-2200000, 2200000), ylim = c(-2000000, 800000)) +  
  # Titles and caption
  labs(
    title = "The longest NBA team relocations",
    caption = "Source: Wikipedia · Graphic: Georgios Karamanis"
  ) +
  # Theme
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.border = element_rect(fill = NA, color = "black"),
    panel.background = element_rect(fill = "#f8f9fc"),
    plot.title = element_text(family = f2, size = 30, face = "bold", color = "#1d3557", margin = margin(0, 0, 10, 0), hjust = 0.5),
    plot.caption = element_text(family = f1, size = 10, color = "#353B3F", hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  ) 
  
  

               