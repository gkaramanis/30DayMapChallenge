library(tidyverse)
library(sf)
library(sfnetworks)
library(tidygraph)

source(here::here("2026", "R", "helpers.R"))

cycleways <- readRDS(file.path(dir_2026, "data", "osm-cycleways-city.rds"))$osm_lines |>
  select(geometry) |>
  st_transform(crs_se) |>
  st_cast("LINESTRING")

net <- as_sfnetwork(cycleways, directed = FALSE) |>
  convert(to_spatial_subdivision) |>
  convert(to_largest_component) |>
  activate("edges") |>
  mutate(w = as.numeric(st_length(geometry)))

# Key nodes, lon/lat
hub <- c(17.645, 59.858)
ends <- tribble(
  ~route, ~lon, ~lat,
  "north", 17.65, 59.895,
  "south", 17.65, 59.812,
  "east", 17.74, 59.855,
  "west", 17.57, 59.85,
  "northeast", 17.72, 59.885,
  "southwest", 17.59, 59.82,
  "southeast", 17.70, 59.82
)

to_node <- function(lon, lat) {
  pt <- st_sfc(st_point(c(lon, lat)), crs = 4326) |> st_transform(crs_se)
  st_nearest_feature(pt, activate(net, "nodes") |> st_as_sf())
}

hub_id <- to_node(hub[1], hub[2])

route_nodes <- function(route, lon, lat) {
  path <- st_network_paths(net, from = hub_id, to = to_node(lon, lat), weights = "w")
  nodes <- activate(net, "nodes") |> st_as_sf()
  ids <- path$node_paths[[1]]
  xy <- st_coordinates(nodes[ids, ])
  # Keep a few waypoints along the path
  keep <- unique(round(seq(1, nrow(xy), length.out = 4)))
  tibble(route = route, x = xy[keep, 1], y = xy[keep, 2])
}

waypoints <- pmap_dfr(ends, route_nodes)

# Octilinear elbow between two points: diagonal first, then straight
octi_pair <- function(x1, y1, x2, y2) {
  dx <- x2 - x1
  dy <- y2 - y1
  d <- min(abs(dx), abs(dy))
  ex <- x1 + sign(dx) * d
  ey <- y1 + sign(dy) * d
  tibble(x = c(x1, ex, x2), y = c(y1, ey, y2))
}

octi_route <- function(df) {
  map_dfr(seq_len(nrow(df) - 1), function(i) octi_pair(df$x[i], df$y[i], df$x[i + 1], df$y[i + 1])) |>
    distinct()
}

tube <- waypoints |>
  group_split(route) |>
  set_names(map_chr(group_split(waypoints, route), \(d) d$route[1])) |>
  map_dfr(octi_route, .id = "route")

stations <- waypoints |>
  distinct(x, y)

p <- ggplot() +
  geom_path(data = tube, aes(x, y, color = route, group = route), linewidth = 2.5, lineend = "round", linejoin = "round") +
  geom_point(data = stations, aes(x, y), shape = 21, fill = "white", color = "black", size = 3, stroke = 1) +
  coord_equal() +
  theme_void() +
  theme(legend.position = "none", plot.background = element_rect(fill = clr_bg, color = NA))

ggsave(here::here("2026/25-is-this-a-map/alternatives/25-is-this-a-map-cycle-tube.png"), p, width = map_box$width, height = map_box$height, dpi = 150)
