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

# Lakes and rivers, rivers buffered to 8 m so they draw as polygons
water_osm <- cached("osm-water-city", {
  q <- osmdata::opq(bb_city, timeout = 600)
  c(
    osmdata::osmdata_sf(osmdata::add_osm_feature(q, "natural", "water")),
    osmdata::osmdata_sf(osmdata::add_osm_feature(q, "waterway", "river"))
  )
})
water <- bind_rows(
  water_osm$osm_polygons |> select(osm_id) |> st_transform(crs_se),
  water_osm$osm_multipolygons |> select(osm_id) |> st_transform(crs_se),
  water_osm$osm_lines |>
    filter(waterway == "river") |>
    select(osm_id) |>
    st_transform(crs_se) |>
    st_buffer(8)
)

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
on <- as.Date("2026-11-03")

trips_per_shape <- tidytransit::filter_feed_by_date(gtfs, on)$trips |>
  filter(!is.na(shape_id)) |>
  count(shape_id, name = "trips")

shapes <- tidytransit::shapes_as_sf(gtfs$shapes) |>
  inner_join(trips_per_shape, by = join_by(shape_id)) |>
  st_transform(crs_se) |>
  arrange(trips)

# 10.6 km wide view around the city centre
centre <- st_bbox(setNames(bb_city, c("xmin", "ymin", "xmax", "ymax")), crs = 4326) |>
  st_as_sfc() |>
  st_transform(crs_se) |>
  st_centroid() |>
  st_coordinates()
view <- st_bbox(
  c(xmin = centre[1] - 5300, xmax = centre[1] + 5300,
    ymin = centre[2] - 4800, ymax = centre[2] + 4800),
  crs = st_crs(crs_se)
)

gg_record(
  here::here("30daymap-temp"),
  width = map_box$width, height = map_box$height, units = "in", dpi = 320
)

p <- ggplot() +
  geom_sf(data = water, fill = clr_water, color = NA) +
  geom_sf(data = shapes, aes(linewidth = trips, color = trips), alpha = 0.7) +
  scale_linewidth(range = c(0.15, 2.2), guide = "none") +
  scale_color_gradient(
    low = clr_accent, high = clr_accent2,
    transform = "log10", breaks = c(10, 100, 1000)
  ) +
  coord_map_box(view) +
  labs(color = "Trips per day") +
  theme_map() +
  theme(
    legend.direction = "horizontal",
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(0.35, "cm")
  )

save_map(
  p, here::here("2026/02-lines/02-lines-bus-routes.png"),
  title = "Uppsala's bus network",
  subtitle = "Line width shows trips per day \\ on Tuesday 3 November 2026",
  caption = caption_src("UL GTFS via Trafiklab")
)
