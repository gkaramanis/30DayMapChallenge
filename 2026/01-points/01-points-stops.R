library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")
dir.create(dir_data, showWarnings = FALSE)

# Evaluate expr once and cache the result as rds in data/
cached <- function(name, expr) {
  path <- file.path(dir_data, paste0(name, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  saveRDS(expr, path)
  expr
}

# City bounding box (xmin, ymin, xmax, ymax) in WGS84
bb_city <- c(17.55, 59.80, 17.76, 59.90)

kommun <- cached("boundary-uppsala_kommun", {
  osmdata::opq(c(17.15, 59.60, 18.15, 60.20), timeout = 300) |>
    osmdata::add_osm_feature("boundary", "administrative") |>
    osmdata::add_osm_feature("admin_level", "7") |>
    osmdata::add_osm_feature("name", "Uppsala kommun") |>
    osmdata::osmdata_sf() |>
    pluck("osm_multipolygons") |>
    select(name) |>
    st_make_valid()
}) |>
  st_transform(crs_se)

roads <- cached("osm-roads-city", {
  osmdata::opq(bb_city, timeout = 600) |>
    osmdata::add_osm_feature("highway", c(
      "motorway", "trunk", "primary", "secondary", "tertiary",
      "unclassified", "residential", "living_street", "service",
      "motorway_link", "trunk_link", "primary_link", "secondary_link"
    )) |>
    osmdata::osmdata_sf()
})$osm_lines |>
  st_transform(crs_se) |>
  mutate(major = highway %in% c("motorway", "trunk", "primary", "secondary"))

# Key from https://developer.trafiklab.se, stored in ~/.Renviron
ul_zip <- file.path(dir_data, "ul.zip")
if (!file.exists(ul_zip)) {
  key <- Sys.getenv("TRAFIKLAB_GTFS_REGIONAL_KEY")
  download.file(
    paste0("https://opendata.samtrafiken.se/gtfs/ul/ul.zip?key=", key),
    ul_zip, mode = "wb", headers = c("Accept-Encoding" = "gzip")
  )
}
gtfs <- tidytransit::read_gtfs(ul_zip)
on <- as.Date("2026-11-01")

deps <- tidytransit::filter_feed_by_date(gtfs, on)$stop_times |>
  count(stop_id, name = "departures")

# Group platforms into their parent station where there is one
stops <- gtfs$stops |>
  left_join(deps, by = join_by(stop_id)) |>
  mutate(group_id = coalesce(na_if(parent_station, ""), stop_id)) |>
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) |>
  group_by(group_id) |>
  summarise(departures = sum(departures, na.rm = TRUE)) |>
  filter(departures > 0) |>
  st_centroid() |>
  st_transform(crs_se) |>
  arrange(departures)

uppsala_c <- slice_max(stops, departures, n = 1)

gg_record(
  here::here("30daymap-temp"),
  width = map_box$width, height = map_box$height, units = "in", dpi = 320
)

p <- ggplot() +
  geom_sf(data = kommun, fill = "#FAFAF6", color = clr_grid, linewidth = 0.4) + # no land color here
  geom_sf(data = roads, aes(linewidth = major), color = clr_road) +
  geom_sf(
    data = stops, aes(size = departures),
    color = clr_accent2, alpha = 0.5, stroke = 0
  ) +
  geom_sf(
    data = uppsala_c, aes(size = departures),
    color = clr_accent, alpha = 0.7, stroke = 0
  ) +
  shadowtext::geom_shadowtext(
    data = uppsala_c, aes(label = "Uppsala C", geometry = geometry),
    stat = "sf_coordinates", family = f1, fontface = "bold", size = 6,
    color = clr_accent, bg.color = clr_land, hjust = 0, nudge_x = 500
  ) +
  scale_linewidth_manual(values = c(`FALSE` = 0.5, `TRUE` = 1), guide = "none") +
  scale_size_area(max_size = 14, labels = scales::label_number(big.mark = " ")) +
  coord_map_box(st_bbox(setNames(bb_city, c("xmin", "ymin", "xmax", "ymax")), crs = 4326)) +
  guides(size = guide_legend(
    title = NULL,
    override.aes = list(color = clr_accent2, alpha = 0.8)
  )) +
  theme_map() +
  theme(
    legend.text = element_text(margin = margin(l = 7, r = 20)),
    legend.key.height = unit(12, "mm"),
    legend.key.width = unit(12, "mm")
  )

save_map(
  p, here::here("2026/01-points/01-points-stops.png"),
  title = "Where Uppsala's buses stop,\\ and how many",
  subtitle = "UL stops sized by planned departures \\ on Sunday 1 November 2026",
  caption = caption_src("UL GTFS via Trafiklab")
)
