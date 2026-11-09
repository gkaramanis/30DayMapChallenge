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
  saveRDS(expr, path)
  expr
}

lan <- cached("boundary-uppsala_lan", {
  osmdata::opq(bb_lan, timeout = 300) |>
    osmdata::add_osm_feature("boundary", "administrative") |>
    osmdata::add_osm_feature("admin_level", "4") |>
    osmdata::add_osm_feature("name", "Uppsala län") |>
    osmdata::osmdata_sf() |>
    pluck("osm_multipolygons") |>
    select(name) |>
    st_make_valid()
}) |>
  st_transform(crs_se)

# Key from https://developer.trafiklab.se, stored in ~/.Renviron
ul_zip <- file.path(dir_data, "ul.zip")
if (!file.exists(ul_zip)) {
  key <- Sys.getenv("TRAFIKLAB_GTFS_REGIONAL_KEY")
  download.file(paste0("https://opendata.samtrafiken.se/gtfs/ul/ul.zip?key=", key), ul_zip, mode = "wb", headers = c("Accept-Encoding" = "gzip"))
}
gtfs <- tidytransit::read_gtfs(ul_zip)
on <- as.Date("2026-11-09")

# Stops with at least 10 departures on the day
frequent <- tidytransit::filter_feed_by_date(gtfs, on)$stop_times |>
  count(stop_id, name = "departures") |>
  filter(departures >= 10)

stops <- gtfs$stops |>
  filter(!is.na(stop_lat)) |>
  semi_join(frequent, by = join_by(stop_id)) |>
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) |>
  st_transform(crs_se)

# 1 km grid aligned with SCB's SWEREF 99 TM grid
bb <- st_bbox(lan)
grid <- st_make_grid(lan, cellsize = 1000, offset = floor(bb[c("xmin", "ymin")] / 1000) * 1000) |>
  st_as_sf() |>
  st_filter(lan) |>
  rename(geometry = x)

centroids <- st_centroid(grid)
nearest <- st_nearest_feature(centroids, stops)
grid$dist_km <- as.numeric(st_distance(centroids, stops[nearest, ], by_element = TRUE)) / 1000

# Optional: SCB population per 1 km square (open geodata, "Statistik på rutor"),
# saved as data/scb-befolkning-1km.gpkg. Keeps populated squares only.
pop_path <- file.path(dir_data, "scb-befolkning-1km.gpkg")
if (file.exists(pop_path)) {
  pop <- read_sf(pop_path) |> st_transform(crs_se)
  grid <- grid |> st_filter(st_centroid(pop))
}

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

p <- ggplot() +
  geom_sf(data = lan, fill = clr_land, color = NA) +
  geom_sf(data = grid, aes(fill = dist_km), color = NA) +
  geom_sf(data = lan, fill = NA, color = clr_text2, linewidth = 0.2) +
  scale_fill_steps(low = "#DADFF0", high = clr_accent2, breaks = c(1, 2, 5, 10, 20), limits = c(0, NA), labels = \(x) paste(x, "km"), guide = guide_coloursteps(title = NULL)) +
  coord_map_box(st_bbox(lan)) +
  theme_map() +
  theme(legend.key.width = unit(10, "mm"), legend.key.height = unit(7, "mm"), legend.text = element_text(size = 13))

save_map(p, here::here("2026/09-urban-rural/09-urban-rural-service-gap.png"), title = "How far to a bus\\ that actually comes?", subtitle = paste0("Distance from each 1 km square to the nearest \\ stop with 10+ departures on ", str_squish(format(on, "%e %B %Y"))), caption = caption_src("UL GTFS via Trafiklab, SCB, OpenStreetMap"))
