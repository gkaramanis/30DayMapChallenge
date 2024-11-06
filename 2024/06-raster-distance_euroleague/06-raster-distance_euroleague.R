library(tidyverse)
library(sf)
library(terra)
library(rnaturalearth)
library(camcorder)

gg_record(here::here("30daymap-temp/"), width = 8.5, height = 8, dpi = 320)

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
) %>% 
  group_by(longitude, latitude) %>% 
  summarise(teams = list(team)) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(teams = paste(teams, collapse = "\n")) 

# Create an sf object for team locations
teams_sf <- team_locations %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = "EPSG:4326")

# Get Europe map
europe <- ne_countries(scale = "medium", continent = c("Europe", "Asia", "Africa"), returnclass = "sf")

# Define the Lambert Azimuthal Equal Area projection for Europe
laea_crs <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

# Transform sf objects to the new projection
teams_sf_proj <- st_transform(teams_sf, laea_crs)
europe_proj <- st_transform(europe, laea_crs)

# Convert sf objects to SpatVector
teams_vect <- vect(teams_sf_proj)
europe_vect <- vect(europe_proj)

# Create a raster template
rast_template <- rast(ext(europe_vect), resolution = 10000, crs = crs(europe_vect))

# Calculate distances
distance_rast <- distance(rast_template, teams_vect)

# Mask the distance raster with Europe's shape
masked_distance <- mask(distance_rast, europe_vect)

# Convert to data frame for ggplot
distance_df <- as.data.frame(masked_distance, xy = TRUE)
colnames(distance_df)[3] <- "distance"

# Create distance categories
distance_df <- distance_df %>%
  arrange(distance) %>% 
  mutate(category = case_when(
    distance < 100000 ~ "< 100 km",
    distance < 250000 ~ "100-250 km",
    distance < 500000 ~ "250-500 km",
    distance < 1000000 ~ "500-1 000 km",
    TRUE ~ "> 1 000 km"
  )) %>%
  mutate(category = fct_inorder(category))

# Colors for distance zones - smoother transition from blue to red
distance_colors <- c("#1c4680", "#3a6ca4", "#7195c3", "#a37f92", "#b56b6b")

f1 <- "Gabarito"
f2 <- "Graphik"
f2b <- "Graphik Compact"

# Plot
ggplot() +
  geom_raster(data = distance_df, aes(x = x, y = y, fill = category)) +
  geom_sf(data = europe_proj, fill = NA, color = "#ffffff40", size = 0.2) +
  geom_sf(data = teams_sf_proj, color = "#d32f2f", size = 2) +
  ggrepel::geom_text_repel(
    data = teams_sf_proj, 
    aes(label = teams, geometry = geometry), 
    stat = "sf_coordinates",
    lineheight = 0.8,
    family = f1,
    bg.color = alpha("white", 0.8),
    color = "#1a1a1a",
    seed = 999,
    hjust = 0
  ) +
  scale_fill_manual(values = distance_colors, name = "") +
  coord_sf(crs = laea_crs, datum = laea_crs, default_crs = 4326, xlim = c(-15, 60), ylim = c(25, 70)) +
  labs(
    title = "Distance to nearest Euroleague basketball team",
    caption = "Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f2) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.9, 0.9),
    legend.text = element_text(color = "white", size = 10, face = "bold"),
    legend.key = element_rect(color = "white"),
    plot.background = element_rect(fill = "#f8f9fa", color = NA),
    plot.title = element_text(color = "#1a1a1a", size = 18, face = "bold", margin = margin(10, 0, 10, 0))
  )
  
