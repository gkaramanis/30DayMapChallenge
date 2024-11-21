library(tidyverse)
library(sf)
library(camcorder)
library(rnaturalearth)

gg_record(here::here("30daymap-temp/"), width = 10, height = 8, dpi = 320)

us_map <- ne_states(country = "united states of america", returnclass = "sf") %>%
  st_transform("+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") %>% 
  filter(!name %in% c("Alaska", "Hawaii"))

# https://en.wikipedia.org/wiki/List_of_NBA_rivalries
nba_rivalries <- tribble(
  ~team1,         ~team2,           ~conference_type, ~division,        ~status,
  # Interconference
  "Celtics",      "Lakers",         "Interconference", NA,           "Active",
  "Cavaliers",    "Warriors",       "Interconference", NA,           "Active",
  "Lakers",       "Pistons",        "Interconference", NA,           "Active",
  # Eastern Conference - Atlantic
  "76ers",        "Celtics",        "Eastern",         "Atlantic",      "Active",
  "Celtics",      "Knicks",         "Eastern",         "Atlantic",      "Active",
  "Knicks",       "Nets",           "Eastern",         "Atlantic",      "Active",
  "Nets",         "Raptors",        "Eastern",         "Atlantic",      "Active",
  # Eastern Conference - Central
  "Bulls",        "Cavaliers",      "Eastern",         "Central",       "Active",
  "Bulls",        "Pistons",        "Eastern",         "Central",       "Active",
  # Eastern Conference - Southeast
  "Heat",         "Magic",          "Eastern",         "Southeast",     "Active",
  # Eastern Conference - Interdivisional
  "Bulls",        "Knicks",         "Eastern",         "Interdivisional", "Active",
  "Celtics",      "Heat",           "Eastern",         "Interdivisional", "Active",
  "Celtics",      "Pistons",        "Eastern",         "Interdivisional", "Active",
  "Heat",         "Knicks",         "Eastern",         "Interdivisional", "Active",
  "Knicks",       "Pacers",         "Eastern",         "Interdivisional", "Active",
  "Heat",         "Pacers",         "Eastern",         "Interdivisional", "Active",
  # Western Conference - Pacific
  "Kings",        "Lakers",         "Western",         "Pacific",       "Active",
  "Kings",        "Warriors",       "Western",         "Pacific",       "Active",
  "Lakers",       "Clippers",       "Western",         "Pacific",       "Active",
  "Lakers",       "Suns",           "Western",         "Pacific",       "Active",
  "Lakers",       "Warriors",       "Western",         "Pacific",       "Active",
  "SuperSonics",  "Trail Blazers",  "Western",         "Pacific",       "Defunct",
  # Western Conference - Southwest
  "Mavericks",    "Spurs",          "Western",         "Southwest",     "Active",
  "Rockets",      "Spurs",          "Western",         "Southwest",     "Active",
  "Mavericks",    "Rockets",        "Western",         "Southwest",     "Active",
  # Western Conference - Northwest
  "Jazz",         "Nuggets",        "Western",         "Northwest",     "Active",
  "Nuggets",      "Timberwolves",   "Western",         "Northwest",     "Active",
  # Western Conference - Interdivisional
  "Jazz",         "Rockets",        "Western",         "Interdivisional", "Active",
  "Lakers",       "Spurs",          "Western",         "Interdivisional", "Active",
  "Spurs",        "Suns",           "Western",         "Interdivisional", "Active"
)

# Team coordinates
team_locations <- tribble(
  ~team,           ~lon,       ~lat,        ~city,            ~state,
  "76ers",         -75.1719,   39.9012,     "Philadelphia",   "PA",
  "Bulls",         -87.6742,   41.8807,     "Chicago",        "IL",
  "Cavaliers",     -81.6882,   41.4965,     "Cleveland",      "OH",
  "Celtics",       -71.0622,   42.3662,     "Boston",         "MA",
  "Clippers",      -117.6731,  34.1030,     "Los Angeles",    "CA",
  "Heat",          -80.1874,   25.7814,     "Miami",          "FL",
  "Jazz",          -111.9011,  40.7683,     "Salt Lake City", "UT",
  "Kings",         -121.4994,  38.5802,     "Sacramento",     "CA",
  "Knicks",        -73.9934,   40.7505,     "New York",       "NY",
  "Lakers",        -118.2673,  33.9430,     "Los Angeles",    "CA",
  "Magic",         -81.3837,   28.5392,     "Orlando",        "FL",
  "Mavericks",     -96.8100,   32.7905,     "Dallas",         "TX",
  "Nets",          -73.9753,   40.6826,     "Brooklyn",       "NY",
  "Nuggets",       -105.0077,  39.7487,     "Denver",         "CO",
  "Pacers",        -86.1555,   39.7640,     "Indianapolis",   "IN",
  "Pistons",       -83.2455,   42.3410,     "Detroit",        "MI",
  "Raptors",       -79.3791,   43.6435,     "Toronto",        "ON",
  "Rockets",       -95.3621,   29.7508,     "Houston",        "TX",
  "Spurs",         -98.4375,   29.4271,     "San Antonio",    "TX",
  "Suns",          -112.0712,  33.4457,     "Phoenix",        "AZ",
  "SuperSonics",   -122.3321,  47.6062,     "Seattle",        "WA",
  "Timberwolves",  -93.2761,   44.9795,     "Minneapolis",    "MN",
  "Trail Blazers", -122.6668,  45.5316,     "Portland",       "OR",
  "Warriors",      -122.3875,  37.7683,     "San Francisco",  "CA"
)

# Transform team coordinates to Albers projection
team_coords <- team_locations %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(st_crs(us_map))

# Calculate conflicts and join in one pipe
team_coords <- team_coords %>%
  left_join(
    nba_rivalries %>%
      pivot_longer(c(team1, team2), values_to = "team") %>%
      count(team, name = "n_conflicts"),
    by = "team"
  )

# Fonts and colors
f1 <- "Departure Mono"

deep_navy <- "#1A1A2E"
neon_blue <- "#08F7FE"
sage_green <- "#9DC88D"
slate_blue <- "#2A2A45"

conference_colors <- c(
  "Western" = "#F6019D",
  "Interconference" = "#9000FF",
  "Eastern" = "#02FAF2"
)

# Create plot
ggplot() +
  # Base map
  geom_sf(data = us_map, fill = slate_blue, color = sage_green, alpha = 0.4, linewidth = 0.1) +
  # Rivalries
  geom_curve(data = conflict %>% 
             mutate(conference_type = factor(conference_type, 
                    levels = c("Western", "Interconference", "Eastern"))), 
             aes(x = x, xend = xend, y = y, yend = yend, color = conference_type), 
             linewidth = 1.5, alpha = 0.5) +
  scale_color_manual(values = conference_colors) +
  # Teams
  geom_sf(data = team_coords, aes(size = n_conflicts), alpha = 0.8, color = sage_green, show.legend = FALSE) +
  ggrepel::geom_text_repel(data = team_coords, aes(label = team, geometry = geometry, size = n_conflicts), family = f1, color = sage_green, bg.color = deep_navy, alpha = 0.8, stat = "sf_coordinates", show.legend = FALSE) +
  scale_size_continuous(range = c(3, 6)) +
  labs(
    title = "CURRENT & HISTORICAL NBA RIVALRIES",
    caption = "Source: Wikipedia · Graphic: Georgios Karamanis",
    color = ""
    ) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = deep_navy, color = NA),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "top",
    legend.text = element_text(color = neon_blue, size = 11),
    legend.key.width = unit(2.5, "lines"),
    plot.title = element_text(color = sage_green, size = 22, hjust = 0.5, margin = margin(5, 0, 10, 0)),
    plot.caption = element_text(size = 11, hjust = 0.5, color = sage_green)
  ) 
