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

city <- st_bbox(setNames(bb_city, c("xmin", "ymin", "xmax", "ymax")), crs = 4326) |> st_as_sfc() |> st_transform(crs_se)

# View in the map box shape and as tall as the city box, so streets reach every edge
city_bb <- st_bbox(city)
view <- map_box_view(c(mean(city_bb[c("xmin", "xmax")]), mean(city_bb[c("ymin", "ymax")])), (city_bb[["ymax"]] - city_bb[["ymin"]]) * map_box$width / map_box$height)
view_sf <- st_as_sfc(view)

roads <- osm_get("roads-city", bb_city, list(highway = c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential", "living_street", "service", "motorway_link", "trunk_link", "primary_link", "secondary_link")))$osm_lines |> st_transform(crs_se) |> st_filter(view_sf)

# Fuel stations as points; mapped forecourts become their centroids
fuel <- osm_get("fuel-city", bb_city, list(amenity = "fuel"))
fuel_pts <- bind_rows(fuel$osm_points |> filter(amenity %in% "fuel") |> select(osm_id), fuel$osm_polygons |> select(osm_id) |> st_centroid()) |> st_transform(crs_se) |> st_filter(view_sf)

# Pumps and forecourts mapped separately at one site merge into one station
sites <- st_buffer(fuel_pts, 75) |> st_union() |> st_cast("POLYGON")
stations <- fuel_pts |> mutate(site = map_int(st_intersects(geometry, sites), 1)) |> summarise(.by = site, geometry = st_centroid(st_union(geometry))) |> st_as_sf()

# Distance from Stora torget to the nearest station, in km
torget <- st_sfc(st_point(sf_project("EPSG:4326", "EPSG:3006", cbind(17.6389, 59.8586))[1, ]), crs = crs_se)
nearest_km <- min(as.numeric(st_distance(stations, torget))) / 1000

# Glow from stacked buffers, faint at the edge and stronger towards the pumps
glow <- map(seq(60, 300, by = 60), \(r) geom_sf(data = st_buffer(stations, r), fill = clr_accent, colour = NA, alpha = 0.15))

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

p <- ggplot() +
  geom_sf(data = roads, colour = clr_grid, linewidth = 0.25) +
  glow +
  geom_sf(data = stations, colour = clr_text, size = 0.8) +
  coord_sf(xlim = as.numeric(view[c("xmin", "xmax")]), ylim = as.numeric(view[c("ymin", "ymax")]), crs = crs_se, expand = FALSE) +
  theme_map()

save_map(p, here::here("2026/19-smell/19-smell-petrol.png"),
  title = "No smell of petrol\\ in Uppsala's centre",
  subtitle = paste0(nrow(stations), " fuel stations, the nearest ", format(round(nearest_km, 1), nsmall = 1), " km\\ from Stora torget"),
  caption = caption_src("OpenStreetMap")
)
