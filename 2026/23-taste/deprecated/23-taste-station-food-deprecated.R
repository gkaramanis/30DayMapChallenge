library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")
dir.create(dir_data, showWarnings = FALSE, recursive = TRUE)

bb_lan <- c(16.70, 59.30, 18.95, 60.75)

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

osm_boundary <- function(name, admin_level, bb) {
  cached(paste0("boundary-", janitor::make_clean_names(name)), {
    osmdata::opq(bb, timeout = 300) |>
      osmdata::add_osm_feature("boundary", "administrative") |>
      osmdata::add_osm_feature("admin_level", admin_level) |>
      osmdata::add_osm_feature("name", name) |>
      osmdata::osmdata_sf() |>
      pluck("osm_multipolygons") |>
      select(name) |>
      st_make_valid()
  })
}

uppsala_lan <- function() osm_boundary("Uppsala län", "4", bb_lan)


gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

lan <- uppsala_lan() |> st_transform(crs_se)

stations <- osm_get("stations-lan", bb_lan, list(railway = "station"))$osm_points |>
  select(name) |>
  filter(!is.na(name)) |>
  st_transform(crs_se) |>
  st_filter(lan)

food <- osm_get("food-lan", bb_lan, list(amenity = c("restaurant", "fast_food", "cafe")))
places <- bind_rows(food$osm_points |> select(osm_id, any_of("amenity")), food$osm_polygons |> select(osm_id, any_of("amenity")) |> st_centroid())
places <- places[tag(places, "amenity") %in% c("restaurant", "fast_food", "cafe"), ] |>
  st_transform(crs_se)

# 400 m is about a five-minute walk
counts <- st_join(st_buffer(stations, 400), places, join = st_intersects) |>
  st_drop_geometry() |>
  group_by(name) |>
  summarise(total = sum(!is.na(amenity)), restaurant = sum(amenity %in% "restaurant"), fast_food = sum(amenity %in% "fast_food"), cafe = sum(amenity %in% "cafe"))

stations <- stations |>
  left_join(counts, by = "name") |>
  arrange(total)

# Trafikverket's Järnvägsnät med grundegenskaper 3.0 (Lastkajen), in SWEREF 99 TM.
# Open running track: main lines (nhsp) and Lennakatten (tågspår)

rail_gpkg <- file.path(dir_data, "trv-jarnvagsnat", "Järnvägsnät_grundegenskaper3_0_GeoPackage.gpkg")
rail_area <- st_as_text(st_as_sfc(st_bbox(lan)))

rail <- read_sf(rail_gpkg, wkt_filter = rail_area) |>
  filter(SpTyp %in% c("nhsp", "tågspår"), Status == "Öppen")

view <- st_bbox(st_buffer(st_union(stations), 6000))

p <- ggplot() +
  geom_sf(data = lan, fill = clr_land, color = clr_grid, linewidth = 0.4) +
  geom_sf(data = st_intersection(rail, lan), color = clr_text2, linewidth = 0.3, alpha = 0.6) +
  geom_sf(data = stations, aes(size = total), shape = 21, fill = clr_accent2, color = clr_bg, alpha = 0.8, stroke = 0.4) +
  geom_sf(data = slice_max(stations, total, n = 1, with_ties = FALSE), aes(size = total), shape = 21, fill = clr_accent, color = clr_bg, stroke = 0.4) +
  ggrepel::geom_text_repel(data = slice_max(stations, total, n = 7, with_ties = FALSE), aes(label = paste0(name, " ", total), geometry = geometry), stat = "sf_coordinates", family = f1, size = 5, color = clr_text, seed = 3, bg.color = clr_bg, bg.r = 0.15, min.segment.length = 0, box.padding = 1.2, force = 3, segment.color = clr_text2) +
  scale_size_area(max_size = 16, breaks = c(10, 50, 100)) +
  coord_map_box(view) +
  labs(size = "Places within 400 m (about five minutes' walk)") +
  guides(size = guide_legend(override.aes = list(fill = clr_accent2, alpha = 0.8))) +
  theme_map()

save_map(p, here::here("2026/23-taste/23-taste-station-food.png"), title = "A bite before the train", subtitle = "Restaurants, fast food and cafés within 400 m \\ of each station in Uppsala County", caption = caption_src("OpenStreetMap, Trafikverket"))
