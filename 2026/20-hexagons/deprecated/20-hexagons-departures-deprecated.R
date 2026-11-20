library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")
dir.create(dir_data, showWarnings = FALSE, recursive = TRUE)

bb_kommun <- c(17.15, 59.60, 18.15, 60.20)

# Evaluate expr once and cache the result as rds in data/
cached <- function(name, expr) {
  path <- file.path(dir_data, paste0(name, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  out <- expr
  saveRDS(out, path)
  out
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

uppsala_kommun <- function() osm_boundary("Uppsala kommun", "7", bb_kommun)

# Key from https://developer.trafiklab.se, stored in ~/.Renviron
ul_zip <- file.path(dir_data, "ul.zip")
if (!file.exists(ul_zip)) {
  key <- Sys.getenv("TRAFIKLAB_GTFS_REGIONAL_KEY")
  download.file(paste0("https://opendata.samtrafiken.se/gtfs/ul/ul.zip?key=", key), ul_zip, mode = "wb", headers = c("Accept-Encoding" = "gzip"))
}

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

gtfs <- tidytransit::read_gtfs(ul_zip)
on <- as.Date("2026-11-20")

# Service day 05-24 is 19 hours
deps <- tidytransit::filter_feed_by_date(gtfs, on)$stop_times |>
  mutate(dep_s = as.numeric(departure_time)) |>
  filter(between(dep_s, 5 * 3600, 24 * 3600 - 1)) |>
  count(stop_id, name = "departures")

stops <- gtfs$stops |>
  filter(!is.na(stop_lat)) |>
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326, remove = FALSE) |>
  inner_join(deps, by = join_by(stop_id))

stops$h3 <- h3jsr::point_to_cell(stops, res = 8)

hexes <- stops |>
  st_drop_geometry() |>
  summarise(per_hour = sum(departures) / 19, .by = h3)

hex_sf <- h3jsr::cell_to_polygon(hexes$h3, simple = FALSE) |>
  rename(h3 = h3_address) |>
  left_join(hexes, by = join_by(h3)) |>
  st_transform(crs_se)

kommun <- uppsala_kommun() |> st_transform(crs_se)
hex_sf <- st_filter(hex_sf, kommun)

p <- ggplot() +
  geom_sf(data = kommun, fill = clr_land, color = NA) +
  geom_sf(data = hex_sf, aes(fill = per_hour), color = clr_bg, linewidth = 0.1) +
  scale_fill_stepsn(colors = c(clr_accent, clr_accent2), trans = "log10", breaks = c(1, 3, 10, 30, 100)) +
  coord_map_box(st_bbox(kommun)) +
  guides(fill = guide_coloursteps(title = NULL, theme = theme(legend.key.width = unit(14, "lines"), legend.key.height = unit(0.8, "lines")))) +
  theme_map()

save_map(p, here::here("2026/20-hexagons/20-hexagons-departures.png"), title = "Buses per hour, by bestagon", subtitle = paste0("Average weekday departures per hour (05-24) \\ in H3 resolution 8 cells, ", str_squish(format(on, "%A %e %B %Y"))), caption = caption_src("UL GTFS via Trafiklab, OpenStreetMap"))
