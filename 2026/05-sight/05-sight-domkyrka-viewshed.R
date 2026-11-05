library(tidyverse)
library(sf)
library(terra)
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

stac_hojd <- "https://api.lantmateriet.se/stac-hojd/v1"

lm_tiles <- function(collection, bb) {
  url <- str_glue("{stac_hojd}/collections/{collection}/items?bbox={str_flatten(bb, ',')}&limit=1000")
  map_chr(jsonlite::read_json(url)$features, \(f) f$assets$data$href)
}

lm_download <- function(href, dir) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(dir, basename(href))
  if (file.exists(path)) return(path)
  part <- paste0(path, ".part")
  httr2::request(href) |> httr2::req_auth_basic(Sys.getenv("LM_USER"), Sys.getenv("LM_PASS")) |> httr2::req_retry(max_tries = 10, is_transient = \(r) httr2::resp_status(r) %in% c(403, 429, 500, 502, 503), backoff = \(i) 5 * i) |> httr2::req_perform(path = part)
  file.rename(part, path)
  path
}

dtm_files <- map_chr(lm_tiles("dtm-cog", bb_city), \(h) lm_download(h, file.path(dir_data, "dem", "lm-dtm")))
laz_files <- map_chr(lm_tiles("dsm-skoglig-copc", bb_city), \(h) lm_download(h, file.path(dir_data, "dem", "lm-laser")))

dsm_path <- file.path(dir_data, "dem", "lm-dsm-5m.tif")
if (!file.exists(dsm_path)) {
  ctg <- lidR::readLAScatalog(laz_files, filter = "-drop_class 7 18")
  lidR::opt_progress(ctg) <- FALSE
  writeRaster(lidR::rasterize_canopy(ctg, res = 5, algorithm = lidR::p2r()), dsm_path)
}
dsm <- rast(dsm_path) |> crop(vect(bb_sf(bb_city)))

dem <- vrt(dtm_files) |> crop(dsm) |> aggregate(fact = 5, fun = "mean")

domkyrka <- st_sfc(st_point(c(17.6336, 59.8582)), crs = 4326) |>
  st_transform(crs(dem))

city <- bb_sf(bb_city)
water <- osm_water()

road_types <- c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential", "living_street", "motorway_link", "trunk_link", "primary_link", "secondary_link", "cycleway")
network <- cached("osm-network-sight-named", st_read(file.path(dir_data, "r5", "uppsala.osm.pbf"), query = sprintf("SELECT name, highway, railway FROM lines WHERE highway IN (%s) OR railway IN ('rail', 'narrow_gauge')", paste0("'", road_types, "'", collapse = ", ")), wkt_filter = st_as_text(st_as_sfc(st_bbox(setNames(bb_city, c("xmin", "ymin", "xmax", "ymax")), crs = 4326))), quiet = TRUE)) |>
  st_transform(crs_se)

network_cells <- rasterize(vect(st_transform(network, crs(dem))), dem, touches = TRUE)
tower_cells <- rasterize(vect(st_buffer(domkyrka, 60)), dem, touches = TRUE)
surface <- min(cover(dsm, dem), dem + 60) |> mask(network_cells, inverse = TRUE) |> mask(tower_cells, inverse = TRUE) |> cover(dem)

vis <- viewshed(surface, loc = st_coordinates(domkyrka)[1, ], observer = 110, target = 1.5)

# The 4 m buffer bridges the gaps where visible cells only touch at a corner

seen <- ifel(vis == 1, 1, NA) |> mask(network_cells) |> as.polygons() |> st_as_sf() |> st_transform(crs_se) |> st_buffer(4)

domkyrka_icon <- function(point, height) {
  outline <- matrix(c(10, 0, 10, 70, 7, 70, 7, 74, 14, 74, 22, 140, 30, 74, 37, 74, 37, 70, 34, 70, 34, 40, 50, 58, 66, 40, 66, 70, 63, 70, 63, 74, 70, 74, 78, 140, 86, 74, 93, 74, 93, 70, 90, 70, 90, 0, 10, 0), ncol = 2, byrow = TRUE)
  angle <- seq(2 * pi, 0, length.out = 25)
  rose <- cbind(50 + 6 * cos(angle), 26 + 6 * sin(angle))
  origin <- st_coordinates(point)[1, 1:2]
  to_map <- \(m) sweep((m - matrix(c(50, 0), nrow(m), 2, byrow = TRUE)) * height / 140, 2, origin, "+")
  st_sfc(st_polygon(list(to_map(outline), to_map(rose))), crs = st_crs(point))
}

icon <- domkyrka_icon(st_transform(domkyrka, crs_se), height = 600)

view_centre <- st_coordinates(st_transform(domkyrka, crs_se))[1, ] + c(1200, -300)
frame <- map_box_view(view_centre, width = 8400)
frame_sf <- st_as_sfc(frame)

network_seen <- st_intersection(st_crop(network, frame_sf), st_union(st_geometry(seen))) |> st_collection_extract("LINESTRING")

# Label the named ways whose visible stretch reaches farthest. Pieces within
# 10 m of each other count as one stretch, so junctions, cell gaps and
# parallel tracks don't split or double it. Reach is the distance between
# the stretch's two farthest points.

n_labels <- 10
label_size <- 3
m_per_in <- (frame[["xmax"]] - frame[["xmin"]]) / map_box$width
label_h <- label_size * .pt / 72 * m_per_in

stretch_ends <- function(stretch) {
  xy <- st_coordinates(st_convex_hull(stretch))[, 1:2]
  d <- as.matrix(dist(xy))
  xy[which(d == max(d), arr.ind = TRUE)[1, ], ]
}

stretches <- network_seen |> filter(!is.na(name)) |> st_buffer(10) |> group_by(name) |> summarise() |> st_cast("POLYGON") |> mutate(ends = map(geometry, stretch_ends), reach = map_dbl(ends, \(e) sqrt(sum((e[1, ] - e[2, ])^2)))) |> slice_max(reach, n = 1, by = name, with_ties = FALSE) |> slice_max(reach, n = n_labels)

label_box <- function(x, y, angle, w, h) {
  a <- angle * pi / 180
  corner <- cbind(c(-1, 1, 1, -1, -1) * w / 2, c(-1, -1, 1, 1, -1) * h / 2)
  st_sfc(st_polygon(list(cbind(x + corner[, 1] * cos(a) - corner[, 2] * sin(a), y + corner[, 1] * sin(a) + corner[, 2] * cos(a)))), crs = crs_se)
}

# Upright text 65% of the way out from the stretch's end nearest the
# cathedral, on whichever side covers less visible network and stays clear
# of the icon and the labels placed before

centre_se <- st_coordinates(st_transform(domkyrka, crs_se))[1, 1:2]

place_label <- function(placed, i) {
  ends <- stretches$ends[[i]]
  e <- ends[order(colSums((t(ends) - centre_se)^2)), ]
  rad <- atan2(e[2, 2] - e[1, 2], e[2, 1] - e[1, 1])
  upright <- if (abs(rad) > pi / 2) rad - sign(rad) * pi else rad
  w <- systemfonts::string_width(stretches$name[i], family = f1, size = label_size * .pt) / 72 * m_per_in
  sides <- tibble(name = stretches$name[i], x = e[1, 1] + 0.65 * (e[2, 1] - e[1, 1]) - c(-1, 1) * 0.8 * label_h * sin(upright), y = e[1, 2] + 0.65 * (e[2, 2] - e[1, 2]) + c(-1, 1) * 0.8 * label_h * cos(upright), angle = upright * 180 / pi)
  boxes <- map(1:2, \(j) label_box(sides$x[j], sides$y[j], sides$angle[j], w, label_h))
  taken <- c(icon, st_geometry(placed))
  ink <- map_dbl(boxes, \(b) sum(as.numeric(st_length(st_intersection(st_geometry(network_seen), b)))))
  clash <- map_lgl(boxes, \(b) any(lengths(st_intersects(taken, b)) > 0))
  best <- which.min(ink + 1e6 * clash)
  bind_rows(placed, st_sf(sides[best, ], geometry = boxes[[best]]))
}

street_labels <- reduce(seq_len(nrow(stretches)), place_label, .init = st_sf(name = character(), geometry = st_sfc(crs = crs_se))) |> st_drop_geometry()

p <- ggplot() +
  geom_sf(data = st_crop(water, frame_sf), fill = clr_water, color = NA) +
  geom_sf(data = st_crop(network, frame_sf), color = clr_road, linewidth = 0.15) +
  geom_sf(data = network_seen, color = clr_accent2, linewidth = 0.5, lineend = "round") +
  geom_sf(data = icon, fill = clr_text, color = clr_accent, linewidth = 0.5) +
  shadowtext::geom_shadowtext(data = street_labels, aes(x, y, label = name, angle = angle), family = f1, size = label_size, color = clr_text, bg.color = clr_bg) +
  coord_map_box(frame) +
  theme_map()

save_map(p, here::here("2026/05-sight/05-sight-domkyrka-viewshed.png"), 
  title = "Where you can see\\ the cathedral spires", 
  subtitle = "Uppsala streets, cycleways and railways where the \\ tops of the towers clear the buildings and trees", 
  caption = caption_src("Lantmäteriet, OpenStreetMap")
)
