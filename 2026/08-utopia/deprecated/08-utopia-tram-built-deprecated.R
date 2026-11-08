library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")
dir.create(dir_data, showWarnings = FALSE, recursive = TRUE)

# City bounding box (xmin, ymin, xmax, ymax) in WGS84
bb_city <- c(17.55, 59.80, 17.76, 59.90)

# Evaluate expr once and cache the result as rds in data/
cached <- function(name, expr) {
  path <- file.path(dir_data, paste0(name, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  saveRDS(expr, path)
  expr
}

# OSM tag column, or NA when no feature carries the tag
tag <- function(df, key) {
  if (key %in% names(df)) df[[key]] else rep(NA_character_, nrow(df))
}

# features: named list of key = values. Each key is queried separately and the
# results combined (OR).
osm_get <- function(name, bb, features) {
  cached(paste0("osm-", name), {
    res <- imap(features, function(value, key) {
      osmdata::opq(bb, timeout = 600) |>
        osmdata::add_osm_feature(key, value) |>
        osmdata::osmdata_sf()
    })
    do.call(c, unname(res))
  })
}

osm_water <- function(bb = bb_city, name = "water-city") {
  w <- osm_get(name, bb, list(natural = "water", waterway = "river"))
  bind_rows(
    w$osm_polygons |> select(osm_id),
    w$osm_multipolygons |> select(osm_id),
    w$osm_lines |>
      filter(waterway == "river") |>
      st_transform(crs_se) |>
      st_buffer(8) |>
      st_transform(4326) |>
      select(osm_id)
  ) |>
    st_transform(crs_se)
}

osm_roads <- function(bb = bb_city, name = "roads-city") {
  osm_get(name, bb, list(highway = c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential", "living_street", "service", "motorway_link", "trunk_link", "primary_link", "secondary_link")))$osm_lines |>
    select(osm_id, any_of(c("name", "highway"))) |>
    st_transform(crs_se)
}

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

# The tram is under construction (service planned 2029). OSM maps it as
# railway=construction + construction:railway=tram, or railway=tram once opened.
tram <- osm_get("tram-construction", bb_city, list(railway = c("construction", "tram")))

lines <- tram$osm_lines
tram_lines <- lines[lines$railway %in% "tram" | tag(lines, "construction:railway") %in% "tram", ] |>
  st_transform(crs_se)

points <- tram$osm_points
tram_stops <- points[tag(points, "station") %in% "tram" | tag(points, "railway") %in% "tram_stop", ] |>
  st_transform(crs_se)

# Base map reaches further south than bb_city so the tram can sit mid-map
bb_base <- c(17.52, 59.76, 17.78, 59.90)
roads <- osm_roads(bb_base, "roads-tram")
water <- osm_water(bb_base, "water-tram")

key_stops <- tram_stops |>
  filter(name %in% c("Uppsala C", "Akademiska sjukhuset", "Gottsunda torg", "Ultuna", "Bergsbrunna station"))

p <- ggplot() +
  geom_sf(data = water, fill = clr_water, color = NA) +
  geom_sf(data = roads, color = clr_road, linewidth = 0.25) +
  geom_sf(data = tram_lines, color = clr_accent2, linewidth = 1.8) +
  geom_sf(data = tram_stops, shape = 21, fill = clr_bg, color = clr_accent2, size = 3, stroke = 1) +
  ggrepel::geom_text_repel(data = filter(key_stops, name == "Bergsbrunna station"), aes(label = name, geometry = geometry), stat = "sf_coordinates", family = f1, fontface = "bold", size = 5.5, color = clr_text, seed = 1, nudge_x = -900, nudge_y = -600, min.segment.length = 0.2, segment.color = clr_text2) +
  ggrepel::geom_text_repel(data = filter(key_stops, name != "Bergsbrunna station"), aes(label = name, geometry = geometry), stat = "sf_coordinates", family = f1, fontface = "bold", size = 5.5, color = clr_text, seed = 1, nudge_x = 600, min.segment.length = 0.2, segment.color = clr_text2) +
  coord_map_box(st_bbox(st_buffer(st_union(tram_lines), 800))) +
  theme_map()

save_map(p, here::here("2026/08-utopia/08-utopia-tram-built.png"), title = "Uppsala spårväg,\\ opening day", subtitle = "The tram from Uppsala C to Gottsunda \\ and Bergsbrunna, drawn as if already running", caption = caption_src("OpenStreetMap (lines under construction)"))
