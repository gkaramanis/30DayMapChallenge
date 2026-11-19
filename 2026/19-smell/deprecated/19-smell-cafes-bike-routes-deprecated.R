library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")
dir.create(dir_data, showWarnings = FALSE, recursive = TRUE)

# Bounding boxes (xmin, ymin, xmax, ymax) in WGS84
bb_city <- c(17.55, 59.80, 17.76, 59.90)

# Bbox vector as a polygon, in SWEREF 99 TM
bb_sf <- function(bb) {
  st_bbox(setNames(bb, c("xmin", "ymin", "xmax", "ymax")), crs = 4326) |>
    st_as_sfc() |>
    st_transform(crs_se)
}

# Evaluate expr once and cache the result as rds in data/
cached <- function(name, expr) {
  path <- file.path(dir_data, paste0(name, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  out <- expr
  saveRDS(out, path)
  out
}

# OSM tag column, or NA when no feature carries the tag
tag <- function(df, key) {
  if (key %in% names(df)) df[[key]] else rep(NA_character_, nrow(df))
}

# features: named list of key = values, e.g. list(amenity = "cafe", shop = "bakery").
# Each key is queried separately and the results combined (OR). NA means any value.
osm_get <- function(name, bb, features) {
  cached(paste0("osm-", name), {
    res <- imap(features, function(value, key) {
      q <- osmdata::opq(bb, timeout = 600)
      q <- if (all(is.na(value))) osmdata::add_osm_feature(q, key) else osmdata::add_osm_feature(q, key, value)
      osmdata::osmdata_sf(q)
    })
    do.call(c, unname(res))
  })
}

osm_cycleways <- function(bb = bb_city, name = "cycleways-city") {
  osm_get(name, bb, list(highway = "cycleway"))$osm_lines |>
    select(osm_id, any_of(c("name", "highway", "surface", "lit"))) |>
    st_transform(crs_se)
}


gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

city <- bb_sf(bb_city)

# Central Uppsala, about 4 km across, centred on Stora torget
centre <- sf_project("EPSG:4326", "EPSG:3006", cbind(17.6389, 59.8586))
view <- map_box_view(centre, 4000)
view_sf <- st_as_sfc(view)

# Cafés and bakeries as points; mapped buildings become their centroids
food <- osm_get("cafes-bakeries-city", bb_city, list(amenity = "cafe", shop = "bakery"))
food_pts <- bind_rows(food$osm_points |> select(osm_id, any_of(c("name", "amenity", "shop"))), food$osm_polygons |> select(osm_id, any_of(c("name", "amenity", "shop"))) |> st_centroid())
smells <- food_pts |> filter(tag(pick(everything()), "amenity") %in% "cafe" | tag(pick(everything()), "shop") %in% "bakery") |> st_transform(crs_se) |> st_filter(view_sf)

roads <- osm_get("roads-city", bb_city, list(highway = c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential", "living_street", "service", "motorway_link", "trunk_link", "primary_link", "secondary_link")))$osm_lines |> st_transform(crs_se) |> st_filter(view_sf)
cycleways <- osm_cycleways() |> st_filter(view_sf)

# Smell reaches 50 m; cycleway inside that reach is where a cyclist smells it
reach <- st_union(st_buffer(smells, 50))
cycleways_reached <- st_intersection(cycleways, reach)
share_near <- mean(lengths(st_intersects(smells, st_buffer(cycleways, 50))) > 0)

# Glow from stacked buffers, faint at 50 m and stronger towards the café
glow <- map(seq(10, 50, by = 10), \(r) geom_sf(data = st_buffer(smells, r), fill = clr_accent, colour = NA, alpha = 0.18))

p <- ggplot() +
  geom_sf(data = roads, colour = clr_grid, linewidth = 0.25) +
  glow +
  geom_sf(data = cycleways, colour = colorspace::darken(clr_grid, 0.25), linewidth = 0.5) +
  geom_sf(data = cycleways_reached, colour = clr_accent2, linewidth = 1.1) +
  geom_sf(data = smells, colour = clr_text, size = 0.6) +
  coord_sf(xlim = as.numeric(view[c("xmin", "xmax")]), ylim = as.numeric(view[c("ymin", "ymax")]), crs = crs_se, expand = FALSE) +
  theme_map()

save_map(p, here::here("2026/19-smell/19-smell-cafes-bike-routes.png"),
  title = "Follow your nose",
  subtitle = paste0("Smells of cafés and bakeries reach 50 m (amber).\\ ", scales::percent(share_near, accuracy = 1), " of them reach a cycleway (blue)"),
  caption = caption_src("OpenStreetMap")
)
