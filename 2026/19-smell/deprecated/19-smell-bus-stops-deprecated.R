library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")

# Bounding box (xmin, ymin, xmax, ymax) in WGS84
bb_city <- c(17.55, 59.80, 17.76, 59.90)

# Cache as rds in data/
cached <- function(name, expr) {
  path <- file.path(dir_data, paste0(name, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  saveRDS(expr, path)
  expr
}

# features: named list of key = values; each key is queried separately and the results combined
osm_get <- function(name, bb, features) {
  cached(paste0("osm-", name), {
    res <- imap(features, \(value, key) osmdata::opq(bb, timeout = 600) |> osmdata::add_osm_feature(key, value) |> osmdata::osmdata_sf())
    do.call(c, unname(res))
  })
}

# OSM tag column, or NA when no feature carries the tag
tag <- function(df, key) if (key %in% names(df)) df[[key]] else rep(NA_character_, nrow(df))

city <- st_bbox(setNames(bb_city, c("xmin", "ymin", "xmax", "ymax")), crs = 4326) |> st_as_sfc() |> st_transform(crs_se)

# View in the map box shape and as tall as the city box, so streets reach every edge
city_bb <- st_bbox(city)
view <- map_box_view(c(mean(city_bb[c("xmin", "xmax")]), mean(city_bb[c("ymin", "ymax")])), (city_bb[["ymax"]] - city_bb[["ymin"]]) * map_box$width / map_box$height)
view_sf <- st_as_sfc(view)

roads <- osm_get("roads-city", bb_city, list(highway = c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential", "living_street", "service", "motorway_link", "trunk_link", "primary_link", "secondary_link")))$osm_lines |> st_transform(crs_se) |> st_filter(view_sf)

# Cafés and bakeries as points; mapped buildings become their centroids
food <- osm_get("cafes-bakeries-city", bb_city, list(amenity = "cafe", shop = "bakery"))
food_pts <- bind_rows(food$osm_points |> select(osm_id, any_of(c("amenity", "shop"))), food$osm_polygons |> select(osm_id, any_of(c("amenity", "shop"))) |> st_centroid())
cafes <- food_pts |> filter(tag(pick(everything()), "amenity") %in% "cafe" | tag(pick(everything()), "shop") %in% "bakery") |> st_transform(crs_se)

# UL stops, one per parent station rather than per platform, kept only if a trip calls there; ul.zip comes from 17-light-dark's GTFS download
gtfs_stops <- read_csv(unz(file.path(dir_data, "ul.zip"), "stops.txt"), col_types = cols(.default = "c"))
called_at <- read_csv(unz(file.path(dir_data, "ul.zip"), "stop_times.txt"), col_types = cols(.default = "c"), col_select = stop_id) |> distinct()
served_parents <- gtfs_stops |> semi_join(called_at, by = join_by(stop_id)) |> distinct(parent_station)
stops <- gtfs_stops |> filter(location_type == "1") |> semi_join(served_parents, by = join_by(stop_id == parent_station)) |> st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) |> st_transform(crs_se) |> st_filter(view_sf)

# Line from each stop to its nearest café or bakery, walk time at 5 km/h as the crow flies
nearest_cafe <- cafes[st_nearest_feature(stops, cafes), ]
noses <- st_sf(walk_min = as.numeric(st_distance(stops, nearest_cafe, by_element = TRUE)) / (5000 / 60), geometry = st_nearest_points(stops, nearest_cafe, pairwise = TRUE))

# Smell fades with distance: short lines bright and thick, long ones faint and thin
noses_faded <- noses |> mutate(fade = exp(-walk_min / 6), alpha = 0.15 + 0.85 * fade, width = 0.25 + 1.3 * fade) |> arrange(alpha)

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

p <- ggplot() +
  geom_sf(data = roads, colour = clr_grid, linewidth = 0.25) +
  geom_sf(data = noses_faded, aes(alpha = alpha, linewidth = width), colour = clr_accent) +
  geom_sf(data = stops, colour = clr_text2, size = 0.3) +
  geom_sf(data = cafes |> st_filter(view_sf), colour = clr_text, size = 0.9) +
  scale_alpha_identity() +
  scale_linewidth_identity() +
  coord_sf(xlim = as.numeric(view[c("xmin", "xmax")]), ylim = as.numeric(view[c("ymin", "ymax")]), crs = crs_se, expand = FALSE) +
  theme_map()

save_map(p, here::here("2026/19-smell/19-smell-bus-stops.png"),
  title = "Outside the centre, bus\\ stops are far from coffee",
  subtitle = "Lines join each bus stop to its nearest café\\ or bakery and fade as the distance grows",
  caption = caption_src("UL GTFS via Trafiklab, OpenStreetMap")
)
