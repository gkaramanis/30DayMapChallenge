library(tidyverse)
library(sf)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")
centre <- st_point(c(17.646, 59.858)) |> st_sfc(crs = 4326) |> st_transform(crs_se) |> st_coordinates()
view <- map_box_view(centre, 6000)
view_sf <- st_as_sfc(view)

# Food places from the county cache, cropped to the view
food <- readRDS(file.path(dir_data, "osm-food-lan.rds"))
places <- bind_rows(food$osm_points |> select(osm_id, amenity), food$osm_polygons |> select(osm_id, amenity) |> st_centroid()) |> filter(amenity %in% c("restaurant", "fast_food", "cafe")) |> st_transform(crs_se) |> st_filter(view_sf)

cycleways <- readRDS(file.path(dir_data, "osm-cycleways-city.rds"))$osm_lines |> st_transform(crs_se) |> st_filter(st_buffer(view_sf, 1000))
roads <- readRDS(file.path(dir_data, "osm-roads-city.rds"))$osm_lines |> st_transform(crs_se) |> st_filter(view_sf)

nearest <- st_nearest_feature(places, cycleways)
places_dist <- places |> mutate(dist = as.numeric(st_distance(geometry, cycleways[nearest, ], by_element = TRUE)))

p <- ggplot() +
  geom_sf(data = roads, colour = clr_grid, linewidth = 0.2) +
  geom_sf(data = cycleways, colour = clr_accent2, linewidth = 0.4) +
  geom_sf(data = arrange(places_dist, dist), aes(colour = dist), size = 0.8) +
  scale_colour_viridis_c(option = "magma", direction = -1, trans = "sqrt", end = 0.9) +
  coord_map_box(view) +
  theme_map()

ggsave(here::here("2026/23-taste/alternatives/23-taste-bike-delivery.png"), p, width = map_box$width, height = map_box$height, dpi = 150)
