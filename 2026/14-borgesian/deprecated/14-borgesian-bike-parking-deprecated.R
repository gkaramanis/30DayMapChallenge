library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")
dir.create(dir_data, showWarnings = FALSE, recursive = TRUE)

bb_centre <- c(17.620, 59.850, 17.660, 59.866)

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

# Features as a named list of key = values; each key is queried separately
# and the results combined. NA means any value.
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

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

# One bike takes about 0.7 m × 1.8 m of rack space
m2_per_bike <- 0.7 * 1.8

parking <- osm_get("bike-parking-centre", bb_centre, list(amenity = "bicycle_parking"))

pts <- parking$osm_points
pts <- pts[!is.na(tag(pts, "amenity")), ]

# Points drawn as squares of true area; mapped areas kept as drawn
racks <- bind_rows(
  pts |>
    mutate(capacity = suppressWarnings(as.numeric(tag(pick(everything()), "capacity")))) |>
    st_transform(crs_se) |>
    mutate(known = !is.na(capacity), geometry = st_buffer(geometry, sqrt(coalesce(capacity, 2) * m2_per_bike) / 2, endCapStyle = "SQUARE")),
  parking$osm_polygons |>
    mutate(capacity = suppressWarnings(as.numeric(tag(pick(everything()), "capacity")))) |>
    st_transform(crs_se) |>
    mutate(known = TRUE)
) |>
  select(capacity, known)

buildings <- osm_get("buildings-centre", bb_centre, list(building = NA))$osm_polygons |>
  st_transform(crs_se)
roads <- osm_get("roads-centre", bb_centre, list(highway = c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential", "living_street", "service", "motorway_link", "trunk_link", "primary_link", "secondary_link")))$osm_lines |>
  st_transform(crs_se)

total <- sum(racks$capacity, na.rm = TRUE)

p <- ggplot() +
  geom_sf(data = buildings, fill = clr_land, color = clr_bg, linewidth = 0.1) +
  geom_sf(data = roads, color = clr_road, linewidth = 0.3) +
  geom_sf(data = racks, aes(fill = known), color = NA) +
  scale_fill_manual(values = c("TRUE" = clr_accent2, "FALSE" = clr_accent), labels = c("TRUE" = "Capacity tagged", "FALSE" = "Capacity unknown, drawn as 2 bikes")) +
  coord_map_box(st_bbox(c(xmin = 17.625, ymin = 59.850, xmax = 17.655, ymax = 59.866), crs = 4326)) +
  guides(fill = guide_legend(title = NULL)) +
  theme_map()

save_map(p, here::here("2026/14-borgesian/14-borgesian-bike-parking.png"), title = "Every bike space \\ in central Uppsala", subtitle = paste0(scales::number(total, big.mark = " "), " tagged spaces at true size, \\ each drawn as 0.7 × 1.8 m of ground"), caption = caption_src("OpenStreetMap"))
