library(tidyverse)
library(sf)
library(sfnetworks)
library(tidygraph)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")
center <- st_point(c(17.646, 59.858)) |> st_sfc(crs = 4326) |> st_transform(crs_se) |> st_coordinates()
view <- map_box_view(center, 6000)
view_sf <- st_as_sfc(view)
extent <- st_buffer(view_sf, 2000)

# Pizza places up to 2 km outside the view, so edge streets get their true nearest place
food <- readRDS(file.path(dir_data, "osm-food-lan.rds"))
venues <- bind_rows(food$osm_points |> select(osm_id, name, amenity, cuisine), food$osm_polygons |> select(osm_id, name, amenity, cuisine) |> st_centroid()) |> filter(amenity %in% c("restaurant", "fast_food")) |> st_transform(crs_se)
pizza <- venues |> filter(str_detect(coalesce(cuisine, ""), "pizza") | str_detect(coalesce(name, ""), regex("pizz", ignore_case = TRUE))) |> st_filter(extent) |> distinct(geometry, .keep_all = TRUE)

roads <- readRDS(file.path(dir_data, "osm-roads-city.rds"))$osm_lines |> st_transform(crs_se) |> select(osm_id, highway)
cycleways <- readRDS(file.path(dir_data, "osm-cycleways-city.rds"))$osm_lines |> st_transform(crs_se) |> select(osm_id, highway)
streets <- bind_rows(roads, cycleways) |> filter(!highway %in% c("motorway", "motorway_link")) |> st_filter(extent) |> st_cast("LINESTRING")

# Split lines where they share vertices so crossings connect, then snap the pizza places onto the network
net <- as_sfnetwork(streets, directed = FALSE) |> convert(to_spatial_subdivision) |> st_network_blend(pizza) |> activate("edges") |> mutate(len = as.numeric(edge_length()))

nodes <- st_as_sf(net, "nodes")
edges <- st_as_sf(net, "edges")
sources <- st_nearest_feature(pizza, nodes)

# Meters along the network from every pizza place (rows) to every node (columns)
dist <- igraph::distances(net, v = sources, weights = edges$len)
node_pizza <- apply(dist, 2, which.min)
node_dist <- apply(dist, 2, min)

# Distance at each segment's midpoint, reached through its closer end; unreachable segments are dropped
segments <- edges |> mutate(dist = pmin(node_dist[from], node_dist[to]) + len / 2) |> filter(is.finite(dist)) |> st_intersection(view_sf)

p <- ggplot() +
  geom_sf(data = arrange(segments, dist), aes(color = dist), linewidth = 0.35) +
  geom_sf(data = st_filter(pizza, view_sf), color = clr_text, size = 1.2) +
  scale_color_gradient(high = clr_accent, low = clr_accent2, limits = c(0, 1000), oob = scales::oob_squish, breaks = c(0, 500, 1000), labels = c("0", "500", "1,000+"), name = "Distance to nearest pizza, m") +
  coord_map_box(view) +
  theme_map()

ggsave(here::here("2026/23-taste/alternatives/23-taste-pizza-network.png"), p, width = map_box$width, height = map_box$height, dpi = 150)
