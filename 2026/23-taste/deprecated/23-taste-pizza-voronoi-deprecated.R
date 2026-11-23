library(tidyverse)
library(sf)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")
center <- st_point(c(17.646, 59.858)) |> st_sfc(crs = 4326) |> st_transform(crs_se) |> st_coordinates()
view <- map_box_view(center, 6000)
view_sf <- st_as_sfc(view)

# Pizza places up to 2 km outside the view, so edge cells get their true neighbors
food <- readRDS(file.path(dir_data, "osm-food-lan.rds"))
venues <- bind_rows(food$osm_points |> select(osm_id, name, amenity, cuisine), food$osm_polygons |> select(osm_id, name, amenity, cuisine) |> st_centroid()) |> filter(amenity %in% c("restaurant", "fast_food")) |> st_transform(crs_se)
pizza <- venues |> filter(str_detect(coalesce(cuisine, ""), "pizza") | str_detect(coalesce(name, ""), regex("pizz", ignore_case = TRUE))) |> st_filter(st_buffer(view_sf, 2000)) |> distinct(geometry, .keep_all = TRUE)

voronoi <- pizza |> st_union() |> st_voronoi(envelope = st_buffer(view_sf, 3000)) |> st_collection_extract("POLYGON")

# Area is measured before cropping to the view, so cells cut by the frame keep their full size
cells <- st_sf(geometry = voronoi) |> st_intersection(st_buffer(view_sf, 2000)) |> mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6) |> st_intersection(view_sf)

roads <- readRDS(file.path(dir_data, "osm-roads-city.rds"))$osm_lines |> st_transform(crs_se) |> st_filter(view_sf)

p <- ggplot() +
  geom_sf(data = cells, aes(fill = area_km2), color = clr_bg, linewidth = 0.6) +
  geom_sf(data = roads, color = clr_bg, linewidth = 0.15, alpha = 0.6) +
  geom_sf(data = st_filter(pizza, view_sf), color = clr_text, size = 1.2) +
  scale_fill_gradient(low = "#FBE3B8", high = clr_accent2, trans = "sqrt", name = "Area served, km²") +
  coord_map_box(view) +
  theme_map()

ggsave(here::here("2026/23-taste/alternatives/23-taste-pizza-voronoi.png"), p, width = map_box$width, height = map_box$height, dpi = 150)
