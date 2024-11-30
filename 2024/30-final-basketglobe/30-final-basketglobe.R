library(tidyverse)
library(sf)
library(ggpath)
library(grid)
library(ggpp)
library(camcorder)

gg_record(here::here("30daymap-temp/"), width = 8, height = 8, dpi = 320)

# World
world <- rnaturalearth::ne_countries(returnclass = "sf") %>% 
  st_transform(crs = "+proj=ortho +lat_0=40 +lon_0=50") %>% 
  rmapshaper::ms_dissolve()

# "Ocean"
ocean <- st_point(x = c(0,0)) %>%
  st_buffer(dist = 6371000) %>%
  st_sfc(crs = "+proj=ortho +lat_0=40 +lon_0=50")

# Cities
col_names = c("geonameid", "name", "asciiname", "alternatenames", "latitude", "longitude", "feature class", "feature code", "country code", "cc2", "admin1 code", "admin2 code", "admin3 code", "admin4 code", "population", "elevation", "dem", "timezone", "modification date")

cities <- read_tsv(here::here("2022/data/cities5000.txt"), col_names = col_names)

cities_sf <- st_as_sf(cities, coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(crs = "+proj=ortho +lat_0=40 +lon_0=50")

# Basketball image
bball <- here::here("2024/30-final-basketglobe/basketball8.png")

# Shadow
shadow_grob <- grid.circle(gp = gpar(col = NA, fill = radialGradient(colours = c(NA, "#7D94B0"), stops = c(0.85, 1), cy2 = 0.6)))

shadow_df <- data.frame(x = 0, y = 0) %>% 
  mutate(grob = list(shadow_grob))

# Plot
ggplot() +
  geom_sf(data = ocean, fill = "#9ac7e8", color = "#9ac7e8", linewidth = 1) +
  geom_grob(data = shadow_df, aes(x, y, label = grob), vp.height = 0.92, vp.width = 0.92) +
  geom_sf(data = world, color = NA, fill = "#E8E6D9") +
  geom_sf(data = cities_sf, color = "#E67E22", size = 0.1) +
  geom_from_path(data = NULL, aes(1.4e5, -0.5e5, path = bball), width = 0.89, angle = -20, alpha = 1) +
  labs(
    caption = "Source: GeoNames · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = "Radio Canada") +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA),
    plot.caption = element_text(hjust = 0.5)
  )
    
