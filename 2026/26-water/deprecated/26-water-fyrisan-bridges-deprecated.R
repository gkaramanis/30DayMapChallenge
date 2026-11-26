library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")
dir.create(dir_data, showWarnings = FALSE, recursive = TRUE)

bb_kommun <- c(17.15, 59.60, 18.15, 60.20)

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

# Water and roads for base maps
osm_water <- function(bb = bb_city, name = "water-city") {
  w <- osm_get(name, bb, list(natural = "water", waterway = "river"))
  bind_rows(w$osm_polygons |> select(osm_id), w$osm_multipolygons |> select(osm_id), w$osm_lines |> filter(waterway == "river") |> st_transform(crs_se) |> st_buffer(8) |> st_transform(4326) |> select(osm_id)) |>
    st_transform(crs_se)
}

osm_roads <- function(bb = bb_city, name = "roads-city") {
  osm_get(name, bb, list(highway = c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential", "living_street", "service", "motorway_link", "trunk_link", "primary_link", "secondary_link")))$osm_lines |>
    select(osm_id, any_of(c("name", "highway", "maxspeed", "surface", "lit"))) |>
    st_transform(crs_se)
}


gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

river <- osm_get("fyrisan", bb_kommun, list(waterway = "river"))$osm_lines
river <- river[tag(river, "name") %in% "Fyrisån", ] |>
  st_transform(crs_se)

bridges <- osm_get("bridges-kommun", bb_kommun, list(bridge = "yes"))$osm_lines
bridges <- bridges |>
  mutate(highway = tag(pick(everything()), "highway"), railway = tag(pick(everything()), "railway")) |>
  filter(!is.na(highway) | !is.na(railway)) |>
  st_transform(crs_se) |>
  st_filter(st_buffer(st_union(river), 15)) |>
  mutate(mode = case_when(!is.na(railway) ~ "Rail", highway %in% c("cycleway", "footway", "path", "pedestrian", "steps") ~ "Walk & bike", TRUE ~ "Road"))

# Roads and rail drawn last, above the footways that run beside them
bridges <- st_filter(bridges, bb_sf(c(17.60, 59.835, 17.68, 59.875))) |>
  arrange(factor(mode, levels = c("Walk & bike", "Road", "Rail")))
counts <- count(st_drop_geometry(bridges), mode)
water <- osm_water(bb_kommun, "water-kommun")
roads <- osm_roads()

# View around the bridges in the map box's shape
bridges_box <- st_bbox(bridges)
view_h <- (bridges_box[["ymax"]] - bridges_box[["ymin"]]) + 800
view <- map_box_view(c(mean(bridges_box[c("xmin", "xmax")]), mean(bridges_box[c("ymin", "ymax")])), view_h * map_box$width / map_box$height)

p <- ggplot() +
  geom_sf(data = water, fill = clr_water, color = NA) +
  geom_sf(data = roads, color = clr_road, linewidth = 0.3) +
  geom_sf(data = river, color = clr_water, linewidth = 3) +
  geom_sf(data = bridges, color = clr_bg, linewidth = 3.4) +
  geom_sf(data = bridges, aes(color = mode), linewidth = 2.2) +
  scale_color_manual(values = c("Road" = clr_text, "Walk & bike" = clr_accent, "Rail" = clr_accent2), labels = function(x) paste0(x, " (", counts$n[match(x, counts$mode)], ")")) +
  coord_map_box(view) +
  labs(color = NULL) +
  theme_map() +
  theme(panel.background = element_rect(fill = clr_land, color = NA))

save_map(p, here::here("2026/26-water/26-water-fyrisan-bridges.png"), title = "Crossing Fyrisån", subtitle = "Every bridge over the river in central Uppsala, \\ by who can use it", caption = caption_src("OpenStreetMap"))
