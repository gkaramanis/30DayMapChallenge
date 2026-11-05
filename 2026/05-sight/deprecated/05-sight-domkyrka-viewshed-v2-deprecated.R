library(tidyverse)
library(sf)
library(terra)
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

# Water and roads for base maps
osm_water <- function(bb = bb_city, name = "water-city") {
  w <- osm_get(name, bb, list(natural = "water", waterway = "river"))
  bind_rows(w$osm_polygons |> select(osm_id), w$osm_multipolygons |> select(osm_id), w$osm_lines |> filter(waterway == "river") |> st_transform(crs_se) |> st_buffer(8) |> st_transform(4326) |> select(osm_id)) |>
    st_transform(crs_se)
}


gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

# Lantmäteriet height data via STAC, CC BY 4.0. Needs a Geotorget login in LM_USER and LM_PASS,
# with Markhöjdmodell Nedladdning and Laserdata Nedladdning, skog ordered (free)
stac_hojd <- "https://api.lantmateriet.se/stac-hojd/v1"

# Data file URLs of the tiles in a STAC collection that intersect bb
lm_tiles <- function(collection, bb) {
  url <- str_glue("{stac_hojd}/collections/{collection}/items?bbox={str_flatten(bb, ',')}&limit=1000")
  map_chr(jsonlite::read_json(url)$features, \(f) f$assets$data$href)
}

# The server refuses some requests at random with 403, so those are retried
lm_download <- function(href, dir) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(dir, basename(href))
  if (file.exists(path)) return(path)
  part <- paste0(path, ".part")
  httr2::request(href) |> httr2::req_auth_basic(Sys.getenv("LM_USER"), Sys.getenv("LM_PASS")) |> httr2::req_retry(max_tries = 10, is_transient = \(r) httr2::resp_status(r) %in% c(403, 429, 500, 502, 503), backoff = \(i) 5 * i) |> httr2::req_perform(path = part)
  file.rename(part, path)
  path
}

# Markhöjdmodell grid 1 (bare earth, 1 m) and Laserdata skog (point cloud, 2021)
dtm_files <- map_chr(lm_tiles("dtm-cog", bb_city), \(h) lm_download(h, file.path(dir_data, "dem", "lm-dtm")))
laz_files <- map_chr(lm_tiles("dsm-skoglig-copc", bb_city), \(h) lm_download(h, file.path(dir_data, "dem", "lm-laser")))

# Surface model with roofs and tree tops: highest laser point per 5 m cell, noise classes 7 and 18 dropped
dsm_path <- file.path(dir_data, "dem", "lm-dsm-5m.tif")
if (!file.exists(dsm_path)) {
  ctg <- lidR::readLAScatalog(laz_files, filter = "-drop_class 7 18")
  lidR::opt_progress(ctg) <- FALSE
  writeRaster(lidR::rasterize_canopy(ctg, res = 5, algorithm = lidR::p2r()), dsm_path)
}
dsm <- rast(dsm_path) |> crop(vect(bb_sf(bb_city)))

# Bare earth on the same 5 m grid; 5 m cells keep the viewshed fast enough over ~12 km
dem <- vrt(dtm_files) |> crop(dsm) |> aggregate(fact = 5, fun = "mean")

domkyrka <- st_sfc(st_point(c(17.6336, 59.8582)), crs = 4326) |>
  st_transform(crs(dem))

city <- bb_sf(bb_city)
water <- osm_water()

# Streets, cycleways and railways (Lennakatten included) from the OSM extract that r5r uses
road_types <- c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential", "living_street", "motorway_link", "trunk_link", "primary_link", "secondary_link", "cycleway")
network <- cached("osm-network-sight", st_read(file.path(dir_data, "r5", "uppsala.osm.pbf"), query = sprintf("SELECT highway, railway FROM lines WHERE highway IN (%s) OR railway IN ('rail', 'narrow_gauge')", paste0("'", road_types, "'", collapse = ", ")), wkt_filter = st_as_text(st_as_sfc(st_bbox(setNames(bb_city, c("xmin", "ymin", "xmax", "ymax")), crs = 4326))), quiet = TRUE)) |>
  st_transform(crs_se) |>
  mutate(mode = case_when(!is.na(railway) ~ "Railway", highway == "cycleway" ~ "Cycleway", .default = "Street"))

# Building footprints from the same extract, drawn under the network
buildings <- cached("osm-buildings-sight", st_read(file.path(dir_data, "r5", "uppsala.osm.pbf"), query = "SELECT building FROM multipolygons WHERE building IS NOT NULL", wkt_filter = st_as_text(st_as_sfc(st_bbox(setNames(bb_city, c("xmin", "ymin", "xmax", "ymax")), crs = 4326))), quiet = TRUE)) |>
  st_transform(crs_se)

# Roofs and trees block the view, capped at 60 m above ground to drop stray returns and chimney tips.
# The network is cut down to the ground, so each stretch is seen from 1.5 m above it, not from the tree over it,
# and the cathedral's own roof and spires are cleared around the observer
network_cells <- rasterize(vect(st_transform(network, crs(dem))), dem, touches = TRUE)
tower_cells <- rasterize(vect(st_buffer(domkyrka, 60)), dem, touches = TRUE)
surface <- min(cover(dsm, dem), dem + 60) |> mask(network_cells, inverse = TRUE) |> mask(tower_cells, inverse = TRUE) |> cover(dem)

# Towers are 118.7 m; the line of sight runs to 110 m, near the spire tops
vis <- viewshed(surface, loc = st_coordinates(domkyrka)[1, ], observer = 110, target = 1.5)

# Network cells with the towers in view, as polygons to cut the lines with
seen <- ifel(vis == 1, 1, NA) |> mask(network_cells) |> as.polygons() |> st_as_sf() |> st_transform(crs_se)
network_seen <- st_intersection(network, st_geometry(seen))

seen_share <- bind_rows(all = network, seen = network_seen, .id = "set") |>
  mutate(km = as.numeric(st_length(geometry)) / 1000) |>
  st_drop_geometry() |>
  summarise(km = sum(km), .by = c(set, mode)) |>
  pivot_wider(names_from = set, values_from = km) |>
  mutate(share = seen / all)
print(seen_share)

# Two-tower silhouette of the domkyrka in a 100 x 140 box (towers and spires about equal, as on the
# real 118.7 m front), with a rose window, scaled to height metres and set with its base on the point
domkyrka_icon <- function(point, height) {
  outline <- matrix(c(10, 0, 10, 70, 7, 70, 7, 74, 14, 74, 22, 140, 30, 74, 37, 74, 37, 70, 34, 70, 34, 40, 50, 58, 66, 40, 66, 70, 63, 70, 63, 74, 70, 74, 78, 140, 86, 74, 93, 74, 93, 70, 90, 70, 90, 0, 10, 0), ncol = 2, byrow = TRUE)
  angle <- seq(2 * pi, 0, length.out = 25)
  rose <- cbind(50 + 6 * cos(angle), 26 + 6 * sin(angle))
  origin <- st_coordinates(point)[1, 1:2]
  to_map <- \(m) sweep((m - matrix(c(50, 0), nrow(m), 2, byrow = TRUE)) * height / 140, 2, origin, "+")
  st_sfc(st_polygon(list(to_map(outline), to_map(rose))), crs = st_crs(point))
}

frame <- st_bbox(city)
frame_sf <- st_as_sfc(frame)

p <- ggplot() +
  geom_sf(data = frame_sf, fill = clr_land, color = NA) +
  geom_sf(data = st_crop(water, frame_sf), fill = clr_water, color = NA) +
  geom_sf(data = st_crop(buildings, frame_sf), fill = colorspace::darken(clr_land, 0.12), color = NA) +
  geom_sf(data = st_crop(network, frame_sf), aes(linewidth = mode), color = colorspace::darken(clr_grid, 0.1), alpha = 0.5) +
  geom_sf(data = st_crop(network_seen, frame_sf), aes(linewidth = mode), color = clr_accent2) +
  geom_sf(data = domkyrka_icon(st_transform(domkyrka, crs_se), height = 600), fill = clr_text, color = clr_bg, linewidth = 0.5) +
  scale_linewidth_manual(values = c(Railway = 0.9, Street = 0.4, Cycleway = 0.25), guide = "none") +
  coord_map_box(frame) +
  theme_map()

save_map(p, here::here("2026/05-sight/05-sight-domkyrka-viewshed-v2.png"), title = "Follow the spires", subtitle = "Streets, cycleways and railways (blue) \\ where you can see the top of the domkyrka towers, \\ with buildings and trees in the way", caption = caption_src("Lantmäteriet height model and laser data, OpenStreetMap"))
