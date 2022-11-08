library(tidyverse)
library(sf)
library(camcorder)

gg_record(here::here("30daymap-temp"), width = 20, height = 20, dpi = 320)

col_names = c("geonameid", "name", "asciiname", "alternatenames", "latitude", "longitude", "feature class", "feature code", "country code", "cc2", "admin1 code", "admin2 code", "admin3 code", "admin4 code", "population", "elevation", "dem", "timezone", "modification date")

cities <- read_tsv(here::here("2022/data/cities5000.txt"), col_names = col_names)

cities_sf <- st_as_sf(cities, coords = c("longitude", "latitude"), crs = 4326)

ocean <- st_point(x = c(0,0)) %>%
  st_buffer(dist = 6371000) %>%
  st_sfc(crs = "+proj=ortho +lat_0=53 +lon_0=14")

ggplot(cities_sf) +
  geom_sf(data = ocean, fill = "#180025", color = "grey5") +
  geom_sf(size = 1, color = "orange", alpha = 0.25) +
  geom_sf(size = 0.01, color = "lightgoldenrod1") +
  coord_sf(crs = "+proj=ortho +lat_0=53 +lon_0=14") +
  labs(
    caption = "Source: GeoNames · Graphic: Georgios Karamanis"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey10", color = NA),
    plot.caption = element_text(color = "lightgoldenrod1", family = "Outfit", margin = margin(0, 0, 20, 0))
  )
  
