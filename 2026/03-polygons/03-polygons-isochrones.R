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

# Kommun bounding box (xmin, ymin, xmax, ymax) in WGS84
bb_kommun <- c(17.15, 59.60, 18.15, 60.20)

kommun <- cached("boundary-uppsala_kommun", {
  osmdata::opq(bb_kommun, timeout = 300) |>
    osmdata::add_osm_feature("boundary", "administrative") |>
    osmdata::add_osm_feature("admin_level", "7") |>
    osmdata::add_osm_feature("name", "Uppsala kommun") |>
    osmdata::osmdata_sf() |>
    pluck("osm_multipolygons") |>
    select(name) |>
    st_make_valid()
}) |>
  st_transform(crs_se)

origin <- data.frame(id = "centralen", lat = 59.8586, lon = 17.6459)
departure <- as.POSIXct("2026-11-03 08:00:00", tz = "Europe/Stockholm")
cutoff <- 20

# Needs Java 21 and data/r5/uppsala.osm.pbf, cut from Geofabrik's Sweden file:
# osmium extract -b 16.7,59.3,18.95,60.75 sweden-latest.osm.pbf -o uppsala.osm.pbf
# R5 fails on route_type 1501 (UL line 988, patient transport), so it is dropped.
iso <- cached(paste0("isochrones-centralen-", cutoff, "-2026-11-03"), {
  options(java.parameters = "-Xmx6G")
  dir_r5 <- file.path(dir_data, "r5")
  stopifnot(
    "Put uppsala.osm.pbf in data/r5" =
      file.exists(file.path(dir_r5, "uppsala.osm.pbf"))
  )
  feed <- tidytransit::read_gtfs(file.path(dir_data, "ul.zip"))
  feed$routes <- filter(feed$routes, route_type != 1501)
  feed$trips <- semi_join(feed$trips, feed$routes, by = join_by(route_id))
  feed$stop_times <- semi_join(feed$stop_times, feed$trips, by = join_by(trip_id))
  tidytransit::write_gtfs(feed, file.path(dir_r5, "ul.zip"))
  r5 <- r5r::build_network(dir_r5)
  on.exit(r5r::stop_r5(r5))
  modes <- list(Bike = "BICYCLE", `Bus and walk` = c("WALK", "TRANSIT"))
  imap(modes, \(mode, label) {
    r5r::isochrone(
      r5,
      origins = origin, mode = mode, departure_datetime = departure,
      cutoffs = cutoff, polygon_output = TRUE
    ) |>
      mutate(mode = label)
  }) |>
    list_rbind() |>
    st_as_sf()
}) |>
  st_transform(crs_se)

# Same road types as day 1, read from the OSM extract that r5r uses
road_types <- c(
  "motorway", "trunk", "primary", "secondary", "tertiary",
  "unclassified", "residential", "living_street", "service",
  "motorway_link", "trunk_link", "primary_link", "secondary_link"
)
roads <- cached("osm-roads-city-wide", {
  st_read(
    file.path(dir_data, "r5", "uppsala.osm.pbf"), layer = "lines",
    query = sprintf(
      "SELECT highway FROM lines WHERE highway IN (%s)",
      paste0("'", road_types, "'", collapse = ", ")
    ),
    wkt_filter = st_as_text(st_as_sfc(st_bbox(
      c(xmin = 17.42, ymin = 59.79, xmax = 17.87, ymax = 59.93), crs = 4326
    ))),
    quiet = TRUE
  )
}) |>
  st_transform(crs_se) |>
  mutate(major = highway %in% c("motorway", "trunk", "primary", "secondary"))

origin_sf <- st_as_sf(origin, coords = c("lon", "lat"), crs = 4326) |>
  st_transform(crs_se)
centre <- st_coordinates(origin_sf)

gg_record(
  here::here("30daymap-temp"),
  width = map_box$width, height = map_box$height, units = "in", dpi = 320
)

view <- st_bbox(
  c(xmin = centre[1] - 3200, ymin = centre[2] - 3200,
    xmax = centre[1] + 3200, ymax = centre[2] + 3200),
  crs = crs_se
)
clr_modes <- c("Bike" = clr_accent2, "Bus and walk" = clr_accent)

p <- ggplot() +
  geom_sf(data = filter(iso, mode == "Bike"), aes(fill = mode, color = mode), alpha = 0.55, linewidth = 0.6) +
  geom_sf(data = roads, aes(linewidth = major), color = clr_road) +
  scale_linewidth_manual(values = c(`FALSE` = 0.15, `TRUE` = 0.4), guide = "none") +
  geom_sf(data = filter(iso, mode == "Bus and walk"), aes(fill = mode, color = mode), alpha = 0.4, linewidth = 0.9) +
  geom_sf(data = origin_sf, shape = 21, fill = clr_text, color = clr_bg, size = 3.5, stroke = 1) +
  scale_fill_manual(values = clr_modes) +
  scale_color_manual(values = clr_modes) +
  coord_map_box(view) +
  labs(fill = paste(cutoff, "minutes by"), color = paste(cutoff, "minutes by")) +
  theme_map() +
  theme(
    legend.title = element_text(hjust = 0)
  )

save_map(
  p, here::here("2026/03-polygons/03-polygons-isochrones.png"),
  title = paste0("In ", cutoff, " minutes, \\ the bike beats the bus"),
  subtitle = "Area reachable from Uppsala C by bike and by \\ bus and walk, leaving at 08:00 on a weekday",
  caption = caption_src("UL GTFS via Trafiklab, OpenStreetMap")
)
