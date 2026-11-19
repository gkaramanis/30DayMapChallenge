library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")
zip_ul <- file.path(dir_data, "ul.zip")

# Bounding box (xmin, ymin, xmax, ymax) in WGS84, for the OSM queries
bb_city <- c(17.55, 59.80, 17.76, 59.90)

# An ordinary Tuesday in the feed, for picking each line's usual route
day_date <- "20261124"

# Smell from a café fades as a Gaussian with this spread, in metres
smell_sigma <- 80

# Ground seen from the south: north-south distances shrink by tilt, smell lifts lines up the page
tilt <- 0.6
lift_scale <- 240
ground_w <- 5000

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

# Colour fg laid over bg at opacity a, as an opaque hex
blend <- function(fg, bg, a) {
  m <- outer(a, col2rgb(fg)[, 1]) + outer(1 - a, col2rgb(bg)[, 1])
  rgb(m[, 1], m[, 2], m[, 3], maxColorValue = 255)
}

# Ground box around central Uppsala, deep enough to fill the map box once tilted
centre <- st_point(c(17.646, 59.858)) |> st_sfc(crs = 4326) |> st_transform(crs_se) |> st_coordinates()
cx <- unname(centre[1, "X"])
cy <- unname(centre[1, "Y"])
ground_d <- ground_w * map_box$height / map_box$width / tilt
ground <- st_bbox(c(xmin = cx - ground_w / 2, ymin = cy - ground_d / 2, xmax = cx + ground_w / 2, ymax = cy + ground_d / 2), crs = crs_se) |> st_as_sfc()
half_h <- ground_d * tilt / 2

# Map coordinates to the tilted screen, centred on the city
tilt_xy <- function(x, y) tibble(sx = x - cx, sy = (y - cy) * tilt)

roads <- osm_get("roads-city", bb_city, list(highway = c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential", "living_street", "service", "motorway_link", "trunk_link", "primary_link", "secondary_link")))$osm_lines |> st_transform(crs_se) |> st_filter(ground)
roads_tilt <- (st_geometry(roads) - c(cx, cy)) * matrix(c(1, 0, 0, tilt), 2)

# Cafés and bakeries as points; mapped buildings become their centroids
food <- osm_get("cafes-bakeries-city", bb_city, list(amenity = "cafe", shop = "bakery"))
food_cols <- c("osm_id", "name", "amenity", "shop")
cafes <- bind_rows(food$osm_points |> select(any_of(food_cols)), food$osm_polygons |> select(any_of(food_cols)) |> st_centroid()) |> filter(amenity %in% "cafe" | shop %in% "bakery") |> st_transform(crs_se) |> st_filter(st_buffer(ground, 500))
cafes_xy <- st_coordinates(cafes)
cafes_tilt <- tilt_xy(cafes_xy[, "X"], cafes_xy[, "Y"]) |> mutate(inside = abs(sx) < ground_w / 2 & abs(sy) < half_h)

# Each city line's most common shape on the day; ul.zip comes from 17-light-dark's GTFS download
calendar <- read_csv(unz(zip_ul, "calendar.txt"), col_types = cols(.default = "c"))
calendar_dates <- read_csv(unz(zip_ul, "calendar_dates.txt"), col_types = cols(.default = "c"))
services <- union(calendar |> filter(tuesday == "1", start_date <= day_date, end_date >= day_date) |> pull(service_id), calendar_dates |> filter(date == day_date, exception_type == "1") |> pull(service_id))
routes <- read_csv(unz(zip_ul, "routes.txt"), col_types = cols(.default = "c"))
city_routes <- routes |> filter(as.integer(route_short_name) < 100)
line_shapes <- read_csv(unz(zip_ul, "trips.txt"), col_types = cols(.default = "c")) |> filter(service_id %in% services) |> semi_join(city_routes, by = join_by(route_id)) |> count(route_id, shape_id) |> slice_max(n, n = 1, by = route_id, with_ties = FALSE) |> left_join(city_routes |> select(route_id, route_short_name), by = join_by(route_id))
shapes <- read_csv(unz(zip_ul, "shapes.txt"), col_types = cols(.default = "c")) |> semi_join(line_shapes, by = join_by(shape_id))
lines <- shapes |> mutate(across(c(shape_pt_lat, shape_pt_lon), as.numeric), seq = as.integer(shape_pt_sequence)) |> arrange(shape_id, seq) |> summarise(geometry = st_sfc(st_linestring(cbind(shape_pt_lon, shape_pt_lat)), crs = 4326), .by = shape_id) |> st_as_sf() |> st_transform(crs_se) |> left_join(line_shapes, by = join_by(shape_id)) |> st_intersection(st_buffer(ground, 300))

# Vertices at most 10 m apart along each line. Clipping splits a line where it crosses itself,
# so the pieces are merged back first; densifying keeps their real ends, so pieces still touch.
line_parts <- lines |> select(route_short_name) |> mutate(geometry = st_line_merge(st_cast(geometry, "MULTILINESTRING"))) |> st_cast("MULTILINESTRING", warn = FALSE) |> st_cast("LINESTRING", warn = FALSE) |> mutate(line_id = row_number()) |> st_segmentize(10)
line_xy <- st_coordinates(line_parts)
line_pts <- tibble(x = line_xy[, "X"], y = line_xy[, "Y"], line_id = line_parts$line_id[line_xy[, "L1"]]) |> st_as_sf(coords = c("x", "y"), crs = crs_se, remove = FALSE)

# Smell at each point: Gaussian falloff summed over all cafés and bakeries
cafe_dist <- st_distance(line_pts, cafes) |> units::drop_units()
ridges <- line_pts |> st_drop_geometry() |> mutate(smell = rowSums(exp(-(cafe_dist / smell_sigma)^2 / 2)), tilt_xy(x, y), top = sy + lift_scale * sqrt(smell))

# Curtain under each ridge as one quad per step, painted back to front so near curtains hide far ones
quads <- ridges |> mutate(sx2 = lead(sx), sy2 = lead(sy), top2 = lead(top), .by = line_id) |> filter(!is.na(sx2), top - sy + top2 - sy2 > 3) |> mutate(q = row_number(), depth = (sy + sy2) / 2) |> arrange(desc(depth)) |> mutate(q = factor(q, levels = q))
quad_pts <- quads |> reframe(sx = c(sx, sx2, sx2, sx), sy = c(sy, sy2, top2, top), .by = q)
clr_curtain <- blend(clr_accent, clr_bg, 0.6)

# Ridge lines darken as they rise, so flat stretches recede
ridge_ink <- ridges |> mutate(ink = blend(clr_text, clr_bg, 0.25 + 0.75 * pmin(1, sqrt(smell))))

# Districts for orientation, placed on the ground
districts <- tribble(~name, ~lon, ~lat, "Luthagen", 17.622, 59.866, "Svartbäcken", 17.641, 59.874, "Gränby", 17.680, 59.871, "Fålhagen", 17.660, 59.854, "Kungsängen", 17.652, 59.846, "Eriksberg", 17.612, 59.848)
districts_xy <- districts |> st_as_sf(coords = c("lon", "lat"), crs = 4326) |> st_transform(crs_se) |> st_coordinates()
districts_tilt <- districts |> bind_cols(tilt_xy(districts_xy[, "X"], districts_xy[, "Y"]))

# Cafés in view within 100 m of a city bus line, behind the title's "two in three" (47 of 69)
cafes_view <- cafes |> st_filter(ground)
share_near <- mean(st_is_within_distance(cafes_view, st_union(st_geometry(lines)), 100, sparse = FALSE))

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

p <- ggplot() +
  geom_sf(data = roads_tilt, colour = clr_grid, linewidth = 0.2) +
  geom_polygon(data = quad_pts, aes(sx, sy, group = q), fill = clr_curtain, colour = clr_curtain, linewidth = 0.1) +
  geom_path(data = ridge_ink, aes(sx, top, group = line_id, colour = ink), linewidth = 0.4) +
  geom_point(data = cafes_tilt |> filter(inside), aes(sx, sy), colour = clr_text, size = 0.7) +
  geom_text(data = districts_tilt, aes(sx, sy, label = name), family = f1, colour = clr_text2, size = 3.6) +
  scale_colour_identity() +
  coord_sf(xlim = c(-ground_w / 2, ground_w / 2), ylim = c(-half_h, half_h), expand = FALSE) +
  theme_map()

save_map(p, here::here("2026/19-smell/19-smell-coffee-ridges.png"),
  title = "Two in three cafés sit\\ by a city bus line",
  subtitle = "Bus lines rise where they pass cafés (dots),\\ with central Uppsala seen at an angle from the south",
  caption = caption_src("UL GTFS via Trafiklab, OpenStreetMap")
)
