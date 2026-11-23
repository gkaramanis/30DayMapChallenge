library(tidyverse)
library(sf)
library(sfnetworks)
library(tidygraph)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")
dir.create(dir_data, showWarnings = FALSE, recursive = TRUE)

bb_city <- c(17.55, 59.80, 17.76, 59.90)

# Cached OSM query
osm_get <- function(name, bb, key, value) {
  path <- file.path(dir_data, paste0("osm-", name, ".rds"))
  if (!file.exists(path)) osmdata::opq(bb, timeout = 600) |> osmdata::add_osm_feature(key, value) |> osmdata::osmdata_sf() |> saveRDS(path)
  readRDS(path)
}

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

view <- map_box_view(sf_project("EPSG:4326", paste0("EPSG:", crs_se),cbind(17.646, 59.858)), 6000)
view_sf <- st_as_sfc(view)

# Margin for edge streets
extent <- st_buffer(view_sf, 2000)

food <- osm_get("food-city", bb_city, "amenity", c("restaurant", "fast_food"))
pizza <- bind_rows(food$osm_points, st_centroid(food$osm_polygons)) |> select(name, cuisine) |> st_transform(crs_se) |> filter(str_detect(paste(cuisine, name), regex("pizz", ignore_case = TRUE))) |> st_filter(extent) |> distinct(geometry, .keep_all = TRUE)

roads <- osm_get("roads-city", bb_city, "highway", c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential", "living_street", "service", "motorway_link", "trunk_link", "primary_link", "secondary_link"))$osm_lines
cycleways <- osm_get("cycleways-city", bb_city, "highway", "cycleway")$osm_lines
streets <- bind_rows(roads, cycleways) |> select(highway) |> filter(!highway %in% c("motorway", "motorway_link")) |> st_transform(crs_se) |> st_filter(extent) |> st_cast("LINESTRING")

# Connect crossings, blend pizza in, keep largest component
net <- as_sfnetwork(streets, directed = FALSE) |> convert(to_spatial_subdivision) |> st_network_blend(pizza) |> activate("nodes") |> filter(group_components() == 1) |> activate("edges") |> mutate(weight = as.numeric(edge_length()))

# Closest facility: meters to nearest pizza per node
node_dist <- st_network_cost(net, from = pizza, weights = "weight") |> apply(2, min)

# Midpoint distance per segment
segments <- st_as_sf(net, "edges") |> mutate(dist = pmin(node_dist[from], node_dist[to]) + weight / 2) |> st_intersection(view_sf) |> arrange(dist)

p <- ggplot() +
  geom_sf(data = segments, aes(color = dist), linewidth = 0.35) +
  geom_sf(data = st_filter(pizza, view_sf), fill = clr_text, size = 2.5, shape = 21, color = clr_bg, stroke = 1) +
  scale_color_gradientn(colors = c(clr_accent2, clr_accent, clr_grid), limits = c(0, 1000), oob = scales::oob_squish, breaks = c(0, 250, 500, 750, 1000), labels = c("0", "250", "500", "750", "1,000+"), name = NULL) +
  coord_map_box(view) +
  theme_map() +
  theme(
    legend.position = c(0.87, 0.22),
    legend.title = element_text(hjust = 0),
    legend.key.height = unit(1.5, "lines"),
    legend.key.width = unit(0.5, "lines")
  )

save_map(
  p, here::here("2026/23-taste/23-taste-pizza-network.png"),
  title = "Uppsala's streets \\ and their nearest pizza",
  subtitle = "Distance in meters along streets and \\ bike paths to the nearest pizza place",
  caption = caption_src("OpenStreetMap")
)

