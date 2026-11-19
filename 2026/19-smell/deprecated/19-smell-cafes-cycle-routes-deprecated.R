library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")

# Bounding box (xmin, ymin, xmax, ymax) in WGS84, for the OSM queries
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

# Central Uppsala, the same view as day 15
centre <- st_point(c(17.646, 59.858)) |> st_sfc(crs = 4326) |> st_transform(crs_se) |> st_coordinates()
view <- map_box_view(centre, 6000)
view_sf <- st_as_sfc(view)

# Cycle route classes from NVDB, cut to this view for day 15
cycle_routes <- st_read(file.path(dir_data, "nvdb/cykelvagsklass-uppsala-centrum.gpkg"), quiet = TRUE) |> mutate(width = if_else(Forbindelsekategori == "Lokal cykelväg", 0.35, 0.8))

roads <- osm_get("roads-city", bb_city, list(highway = c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential", "living_street", "service", "motorway_link", "trunk_link", "primary_link", "secondary_link")))$osm_lines |> st_transform(crs_se) |> st_filter(view_sf)

# Cafés and bakeries as points; mapped buildings become their centroids
food <- osm_get("cafes-bakeries-city", bb_city, list(amenity = "cafe", shop = "bakery"))
food_pts <- bind_rows(food$osm_points |> select(osm_id, any_of(c("amenity", "shop"))), food$osm_polygons |> select(osm_id, any_of(c("amenity", "shop"))) |> st_centroid())
cafes <- food_pts |> filter(tag(pick(everything()), "amenity") %in% "cafe" | tag(pick(everything()), "shop") %in% "bakery") |> st_transform(crs_se) |> st_filter(view_sf)

# Line from each café to the nearest point on a cycle route
nearest_route <- cycle_routes[st_nearest_feature(cafes, cycle_routes), ]
sniffs <- st_sf(dist_m = as.numeric(st_distance(cafes, nearest_route, by_element = TRUE)), geometry = st_nearest_points(cafes, nearest_route, pairwise = TRUE))

# Lines fade with distance: short ones bright and thick, long ones faint and thin
sniffs_faded <- sniffs |> mutate(fade = exp(-dist_m / 150), alpha = 0.2 + 0.8 * fade, width = 0.4 + 1.4 * fade) |> arrange(alpha)

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

p <- ggplot() +
  geom_sf(data = roads, colour = clr_grid, linewidth = 0.25) +
  geom_sf(data = cycle_routes, aes(linewidth = width), colour = colorspace::darken(clr_grid, 0.25)) +
  geom_sf(data = sniffs_faded, aes(alpha = alpha, linewidth = width), colour = clr_accent) +
  geom_sf(data = cafes, colour = clr_text, size = 0.9) +
  scale_alpha_identity() +
  scale_linewidth_identity() +
  coord_sf(xlim = as.numeric(view[c("xmin", "xmax")]), ylim = as.numeric(view[c("ymin", "ymax")]), crs = crs_se, expand = FALSE) +
  theme_map()

save_map(p, here::here("2026/19-smell/19-smell-cafes-cycle-routes.png"),
  title = "Cafés and\\ cycle routes",
  subtitle = "Lines join each café or bakery to the nearest\\ cycle route and fade as the distance grows",
  caption = caption_src("Trafikverket NVDB, OpenStreetMap")
)
