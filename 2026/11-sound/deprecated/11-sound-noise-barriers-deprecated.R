library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")
dir.create(dir_data, showWarnings = FALSE)

bb_kommun <- c(17.15, 59.60, 18.15, 60.20)

# Evaluate expr once and cache the result as rds in data/
cached <- function(name, expr) {
  path <- file.path(dir_data, paste0(name, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  out <- expr
  saveRDS(out, path)
  out
}

# features: named list of key = values. Each key is queried separately and
# the results combined (OR).
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

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

# Official noise maps are PDF only, so map where Uppsala builds walls against sound
barriers <- osm_get("noise-barriers", bb_kommun, list(wall = "noise_barrier"))$osm_lines |>
  st_transform(crs_se) |>
  mutate(length_m = as.numeric(st_length(geometry)))

kommun <- osm_boundary("Uppsala kommun", "7", bb_kommun) |>
  st_transform(crs_se)

# The cached query also holds OSM rail; keep its roads only
loud_roads <- osm_get("loud-roads", bb_kommun, list(highway = c("motorway", "trunk", "primary"), railway = "rail"))$osm_lines |>
  filter(!is.na(highway)) |>
  st_transform(crs_se)

# Trafikverket's Järnvägsnät med grundegenskaper 3.0 (Lastkajen), in SWEREF 99 TM.
# Open running track: main lines (nhsp) and Lennakatten (tågspår)

rail_gpkg <- file.path(dir_data, "trv-jarnvagsnat", "Järnvägsnät_grundegenskaper3_0_GeoPackage.gpkg")
rail_area <- st_as_text(st_as_sfc(st_bbox(st_buffer(kommun, 10000))))

rail <- read_sf(rail_gpkg, wkt_filter = rail_area) |>
  filter(SpTyp %in% c("nhsp", "tågspår"), Status == "Öppen")

area <- st_buffer(st_union(barriers), 1500) |> st_bbox()
total_km <- round(sum(barriers$length_m) / 1000, 1)

p <- ggplot() +
  geom_sf(data = kommun, fill = clr_land, color = NA) +
  geom_sf(data = loud_roads, color = clr_text2, linewidth = 0.4) +
  geom_sf(data = rail, color = clr_text2, linewidth = 0.4) +
  geom_sf(data = barriers, color = clr_accent, linewidth = 2) +
  coord_map_box(area) +
  theme_map()

save_map(p, here::here("2026/11-sound/11-sound-noise-barriers.png"), title = "Walls against noise", subtitle = paste0(total_km, " km of noise barriers (amber) along Uppsala's \\ ", "motorways, main roads and railways (grey)"), caption = caption_src("OpenStreetMap wall=noise_barrier, Trafikverket"))
