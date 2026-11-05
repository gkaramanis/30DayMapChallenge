library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")
dir.create(dir_data, showWarnings = FALSE, recursive = TRUE)

bb_city <- c(17.55, 59.80, 17.76, 59.90)

bb_sf <- function(bb) {
  st_bbox(setNames(bb, c("xmin", "ymin", "xmax", "ymax")), crs = 4326) |>
    st_as_sfc() |>
    st_transform(crs_se)
}

cached <- function(name, expr) {
  path <- file.path(dir_data, paste0(name, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  out <- expr
  saveRDS(out, path)
  out
}

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

osm_water <- function(bb = bb_city, name = "water-city") {
  w <- osm_get(name, bb, list(natural = "water", waterway = "river"))
  bind_rows(w$osm_polygons |> select(osm_id), w$osm_multipolygons |> select(osm_id), w$osm_lines |> filter(waterway == "river") |> st_transform(crs_se) |> st_buffer(8) |> st_transform(4326) |> select(osm_id)) |>
    st_transform(crs_se)
}

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

road_types <- c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential", "living_street", "motorway_link", "trunk_link", "primary_link", "secondary_link", "cycleway")
network <- cached("osm-network-sight", st_read(file.path(dir_data, "r5", "uppsala.osm.pbf"), query = sprintf("SELECT highway, railway FROM lines WHERE highway IN (%s) OR railway IN ('rail', 'narrow_gauge')", paste0("'", road_types, "'", collapse = ", ")), wkt_filter = st_as_text(st_as_sfc(st_bbox(setNames(bb_city, c("xmin", "ymin", "xmax", "ymax")), crs = 4326))), quiet = TRUE)) |>
  st_transform(crs_se) |>
  mutate(mode = case_when(!is.na(railway) ~ "Railway", highway == "cycleway" ~ "Cycleway", .default = "Street"))

city <- bb_sf(bb_city)
water <- osm_water()

# At each vertex, cast a ray in the local forward direction and walk ahead
# until the road centre deviates more than threshold metres from the ray
forward_sight <- function(coords, threshold, max_dist) {
  n <- nrow(coords)
  out <- numeric(n)
  for (i in seq_len(n - 1)) {
    dx <- coords[i + 1, 1] - coords[i, 1]
    dy <- coords[i + 1, 2] - coords[i, 2]
    seg_len <- sqrt(dx^2 + dy^2)
    if (seg_len < 1e-8) next
    ux <- dx / seg_len
    uy <- dy / seg_len
    for (j in (i + 1):n) {
      vx <- coords[j, 1] - coords[i, 1]
      vy <- coords[j, 2] - coords[i, 2]
      d <- sqrt(vx^2 + vy^2)
      if (d > max_dist) { out[i] <- max_dist; break }
      if (abs(vx * uy - vy * ux) > threshold) {
        if (j > i + 1) out[i] <- sqrt((coords[j - 1, 1] - coords[i, 1])^2 + (coords[j - 1, 2] - coords[i, 2])^2)
        break
      }
      out[i] <- d
    }
  }
  out
}

# Max of forward and backward sight distance at each vertex
sight_at_vertex <- function(coords, threshold = 6, max_dist = 1500) {
  pmax(forward_sight(coords, threshold, max_dist), rev(forward_sight(coords[nrow(coords):1, , drop = FALSE], threshold, max_dist)))
}

# Densify to catch gentle curves, then compute one median sight distance per road
net_lines <- st_segmentize(network, dfMaxLength = 20) |> st_cast("LINESTRING")

sight_per_road <- cached("sight-distance-per-road", map_dbl(seq_len(nrow(net_lines)), function(i) {
  coords <- st_coordinates(net_lines[i, ])[, 1:2, drop = FALSE]
  if (nrow(coords) < 3) return(as.numeric(st_length(net_lines[i, ])))
  median(sight_at_vertex(coords))
}, .progress = "Sight distance"))

net_with_sight <- net_lines |> mutate(sight_m = sight_per_road)

frame <- st_bbox(city)
frame_sf <- st_as_sfc(frame)

p <- ggplot() +
  geom_sf(data = frame_sf, fill = clr_land, color = NA) +
  geom_sf(data = st_crop(water, frame_sf), fill = clr_water, color = NA) +
  geom_sf(data = st_crop(net_with_sight, frame_sf), aes(color = sight_m), linewidth = 0.45) +
  scale_color_gradient(low = "#B85C18", high = "#1E2D6B", trans = "log10", limits = c(30, 1500), oob = scales::squish, breaks = c(50, 100, 200, 500, 1000), labels = \(x) paste0(x, " m"), name = NULL) +
  guides(color = guide_colorbar(barwidth = 0.8, barheight = 8)) +
  coord_map_box(frame) +
  theme_map() +
  theme(legend.position = "inside", legend.position.inside = c(0.02, 0.02), legend.justification.inside = c(0, 0))

save_map(p, here::here("2026/05-sight/05-sight-road-visibility.png"), title = "Straight ahead", subtitle = "How far you can see along each road \\ before it curves out of sight", caption = caption_src("OpenStreetMap"))
