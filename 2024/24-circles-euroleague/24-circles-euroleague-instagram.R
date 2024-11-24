library(tidyverse)
library(sf)
library(rnaturalearth)
library(scales)
library(camcorder)

gg_record(here::here("30daymap-temp/"), width = 8.6, height = 8, dpi = 320)

# Euroleague teams
team_locations <- tribble(
~team,              ~latitude,  ~longitude,
"ALBA Berlin",      52.5200,    13.4050,   # Berlin, Germany
"Anadolu Efes",     41.0082,    28.9784,   # Istanbul, Turkey
"ASVEL",            45.7640,    4.8357,    # Lyon, France
"Barcelona",        41.3851,    2.1734,    # Barcelona, Spain
"Baskonia",         42.8467,    -2.6716,   # Vitoria-Gasteiz, Spain
"Bayern Munich",    48.1351,    11.5820,   # Munich, Germany
"Crvena zvezda",    44.8125,    20.4612,   # Belgrade, Serbia
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
"Žalgiris",         54.8985,    23.9036    # Kaunas, Lithuania
)

# Instagram followers 2024-11-24
team_insta <- tribble(
  ~team,             ~followers_m, 
  "ALBA Berlin",      0.0675,
  "Anadolu Efes",     0.255,
  "ASVEL",            0.0933,
  "Barcelona",        1.3,
  "Baskonia",         0.0966,
  "Bayern Munich",    0.3346,
  "Crvena zvezda",    0.3658,
  "Fenerbahçe",       1.4,
  "Maccabi Tel Aviv", 0.1596,
  "Monaco",           0.088,
  "Olimpia Milano",   0.1641,
  "Olympiacos",       0.3494,
  "Panathinaikos",    0.5755,
  "Paris",            0.1619,
  "Partizan",         0.4237,
  "Real Madrid",      5,
  "Virtus Bologna",   0.1102,
  "Žalgiris",         0.1902
) %>% 
  mutate(
    followers = number(followers_m * 1e6),
    followers_lab = case_when(
      followers_m > 1 ~ paste0(followers_m, " M"),
      followers_m < 1 ~ paste0(followers_m * 1000, " K")
    )
    )

# Get Europe map data
europe <- ne_countries(scale = "medium", returnclass = "sf")

# Create a pixelated version
bbox <- st_bbox(europe)
resolution <- 400  # Adjust this value to change pixel size

# Create grid points
grid <- expand.grid(
  x = seq(bbox["xmin"], bbox["xmax"], length.out = resolution * 1.4),
  y = seq(bbox["ymin"], bbox["ymax"], length.out = resolution)
) %>% 
  filter(between(x, -14.5, 45) & between(y, 30, 65))

# Convert to sf object
grid_sf <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(europe))

# Perform spatial join
pixelated_europe <- st_join(grid_sf, europe) %>% 
  select(name) %>% 
  filter(!is.na(name))

# Convert team_locations to sf object
team_locations_sf <- st_as_sf(team_locations, coords = c("longitude", "latitude"), crs = st_crs(europe))

# Find single closest pixelated point for each team
teams_pixelated <- st_nearest_feature(team_locations_sf, pixelated_europe) %>%
  slice(pixelated_europe, .) %>%  # Get the matching pixelated points
  bind_cols(
    team_locations_sf %>% st_drop_geometry()  # Add team data without geometry
  ) %>%
  left_join(team_insta)  # Add Instagram followers

f1 <- "Montagu Slab 144pt"
f2 <- "Graphik Compact"


# Colors
point_color <- "#F5F6F8" 
accent_color <- "#E85D04"
teams_color <- "#ff8b00"

set.seed(3)

# Plot
ggplot() +
  geom_sf(data = pixelated_europe, shape = 21, size = 2, alpha = 1, fill = point_color, stroke = 0.3) +
  ggforce::geom_mark_circle(
    data = teams_pixelated, 
    aes(
      group = team, 
      radius = followers,
      geometry = geometry, 
      label = paste0(team, "\n", followers_lab)
    ), 
    stat = "sf_coordinates", 
    label.fontsize = 9, 
    label.family = f2,
    label.buffer = unit(5, 'mm'), 
    expand = unit(0, 'mm'), 
    color = NA
  ) +
  geom_sf(data = teams_pixelated, aes(size = followers_m), shape = 21, fill = point_color) +
  geom_sf(data = teams_pixelated, aes(size = followers_m), shape = 21, fill = teams_color) +
  scale_size_area(max_size = 18) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Euroleague's Instagram following",
    caption = "Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "gray99", color = NA),
    panel.border = element_rect(fill = NA, color = "black"),
    legend.position = "none",
    plot.title = element_text(
      size = 30, 
      face = "bold", 
      margin = margin(8, 0, 5, 0),
      color = accent_color
    )
  )
  
