library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

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

# SCB open geodata over WFS, filtered with CQL
scb_wfs <- function(name, layer, filter) {
  url <- paste0("https://geodata.scb.se/geoserver/stat/wfs?service=WFS&version=2.0.0&request=GetFeature&typeNames=stat:", layer, "&outputFormat=application/json&CQL_FILTER=", URLencode(filter, reserved = TRUE))
  cached(name, st_read(url, quiet = TRUE)) |>
    st_transform(crs_se) |>
    st_make_valid()
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

# Every tätort in the county: Uppsala is home, the rest are the settlements
tatorter <- scb_wfs("scb-tatorter-lan-2023", "Tatorter_2023", "lan='03'")
uppsala <- filter(tatorter, tatortskod == "0380TC125")
# Sävja and Alsike sit against Uppsala and would crowd its label
towns <- filter(tatorter, bef >= 4000, !tatort %in% c("Sävja", "Alsike")) |> st_point_on_surface()

# 1 km grid aligned with SCB's SWEREF 99 TM grid, cut to the county outline
bb <- st_bbox(lan)
grid <- st_make_grid(lan, cellsize = 1000, offset = floor(bb[c("xmin", "ymin")] / 1000) * 1000) |>
  st_as_sf() |>
  rename(geometry = x) |>
  st_filter(lan) |>
  mutate(cell = row_number())

# Nine points per square on a 3 x 3 grid, 333 m apart, so that one point
# snapped to a dead-end road does not decide the whole square
offsets <- expand_grid(dx = c(-1, 0, 1) * 1000 / 3, dy = c(-1, 0, 1) * 1000 / 3)
centre_xy <- st_coordinates(st_centroid(st_geometry(grid)))

homes <- tibble(cell = grid$cell, x = centre_xy[, 1], y = centre_xy[, 2]) |>
  cross_join(offsets) |>
  mutate(id = paste(cell, row_number(), sep = "_"), x = x + dx, y = y + dy, .by = cell) |>
  st_as_sf(coords = c("x", "y"), crs = crs_se) |>
  st_transform(4326)

points_home <- tibble(id = homes$id, lon = st_coordinates(homes)[, 1], lat = st_coordinates(homes)[, 2])

origin <- data.frame(id = "centralen", lat = 59.8586, lon = 17.6459)
window_start <- as.POSIXct("2026-11-09 15:00:00", tz = "Europe/Stockholm")
walk_max <- 30

# Bus and walk from Uppsala Centralstation to each point, for every departure
# minute from 15:00 to midnight. Changes are allowed; each walk, including the
# one home, is at most 30 minutes at r5r's 3.6 km/h. Each point keeps its
# latest departure that has you home before midnight.
# Needs Java 21 and the day 3 network in data/r5: uppsala.osm.pbf, cut from
# Geofabrik's Sweden file, and ul.zip without route_type 1501, which R5 rejects.
last_home <- function(r5, dest) {
  r5r::expanded_travel_time_matrix(r5r_network = r5, origins = origin, destinations = dest, mode = c("WALK", "TRANSIT"), departure_datetime = window_start, time_window = 9 * 60, max_walk_time = walk_max, max_trip_duration = 240) |>
    filter(!is.na(total_time)) |>
    mutate(dep_h = as.numeric(hms::as_hms(departure_time)) / 3600) |>
    filter(dep_h + total_time / 60 < 24) |>
    summarise(leave = max(dep_h), .by = to_id)
}

leave_points <- cached("r5-last-bus-home-9pt-2026-11-09", {
  options(java.parameters = "-Xmx6G")
  dir_r5 <- file.path(dir_data, "r5")
  ul_zip <- file.path(dir_data, "ul.zip")
  if (!file.exists(file.path(dir_r5, "ul.zip"))) {
    feed <- tidytransit::read_gtfs(ul_zip)
    feed$routes <- filter(feed$routes, route_type != 1501)
    feed$trips <- semi_join(feed$trips, feed$routes, by = join_by(route_id))
    feed$stop_times <- semi_join(feed$stop_times, feed$trips, by = join_by(trip_id))
    tidytransit::write_gtfs(feed, file.path(dir_r5, "ul.zip"))
  }
  r5 <- r5r::build_network(dir_r5)
  on.exit(r5r::stop_r5(r5))
  # R5 takes at most 5000 destinations per call
  split(points_home, ceiling(seq_len(nrow(points_home)) / 5000)) |>
    map(\(dest) last_home(r5, dest), .progress = TRUE) |>
    list_rbind()
})

# A square counts from its best-placed point
leave_by <- leave_points |>
  mutate(cell = as.integer(str_extract(to_id, "^\\d+"))) |>
  summarise(leave = max(leave), .by = cell)

# SCB population per 1 km square, end of 2025, on the same grid. SCB lists
# inhabited squares only.
pop <- scb_wfs("scb-befolkning-1km-2025-lan", "befolkning_1km_2025", paste0("BBOX(sp_geometry,", paste(bb[c("xmin", "ymin", "xmax", "ymax")], collapse = ","), ",'EPSG:3006')")) |>
  st_drop_geometry() |>
  transmute(rutid = rutid_scb, people = beftotalt)

bins <- c("No way home by bus", "Before 20:00", "20:00 or later")

# Each inhabited square, its last bus class, and whether its centre lies in
# Uppsala, in another tätort or in the countryside
in_tatort <- st_within(st_centroid(st_geometry(grid)), st_geometry(tatorter))

homes_people <- grid |>
  mutate(
    rutid = sprintf("%.0f%.0f", round(centre_xy[, 1] - 500), round(centre_xy[, 2] - 500)),
    area = map_chr(in_tatort, \(i) if (length(i) == 0) "Countryside" else if (tatorter$tatortskod[i[1]] == "0380TC125") "Uppsala" else "Towns")
  ) |>
  inner_join(pop, by = join_by(rutid)) |>
  left_join(leave_by, by = join_by(cell)) |>
  mutate(ends = factor(if_else(is.na(leave), bins[1], as.character(cut(leave, c(0, 20, 24), labels = bins[-1], right = FALSE))), levels = bins))

# Share of people outside Uppsala in each class, towns against countryside
shares <- homes_people |>
  st_drop_geometry() |>
  filter(area != "Uppsala") |>
  count(area, ends, wt = people, name = "people") |>
  mutate(share = people / sum(people), .by = area)

stranded <- shares |>
  filter(area == "Countryside", ends == bins[1]) |>
  pull(people)

# Larger squares on the same grid, block_size metres wide, each in the class
# where most of its residents fall. Squares with no residents stay blank.
block_size <- 2000

grid_blocks <- st_make_grid(lan, cellsize = block_size, offset = floor(bb[c("xmin", "ymin")] / block_size) * block_size) |>
  st_as_sf() |>
  rename(geometry = x) |>
  mutate(block = sprintf("%.0f_%.0f", floor(st_coordinates(st_centroid(geometry))[, 1] / block_size), floor(st_coordinates(st_centroid(geometry))[, 2] / block_size)))

block_class <- homes_people |>
  mutate(block = sprintf("%.0f_%.0f", floor(st_coordinates(st_centroid(geometry))[, 1] / block_size), floor(st_coordinates(st_centroid(geometry))[, 2] / block_size))) |>
  st_drop_geometry() |>
  count(block, ends, wt = people, name = "people") |>
  slice_max(people, n = 1, with_ties = FALSE, by = block)

grid_bus <- grid_blocks |>
  inner_join(block_class, by = join_by(block)) |>
  # One shape per class, so no seams show between squares
  group_by(ends) |>
  summarise() |>
  st_intersection(st_geometry(lan))

# One hue, darker the later you can stay in town; stranded homes in warm grey,
# empty land stays plain
clr_ends <- set_names(c("#7F7C73", "#C9D0EE", "#1C2763"), bins)

p <- ggplot() +
  geom_sf(data = lan, fill = clr_land, color = NA) +
  geom_sf(data = grid_bus, aes(fill = ends), color = NA) +
  geom_sf(data = lan, fill = NA, color = clr_text2, linewidth = 0.2) +
  geom_sf(data = uppsala, fill = clr_accent, color = NA) +
  ggrepel::geom_text_repel(data = towns, aes(label = tatort, geometry = geometry), stat = "sf_coordinates", family = f1, size = 3.6, color = clr_text, bg.color = clr_bg, bg.r = 0.12, seed = 1, min.segment.length = Inf) +
  scale_fill_manual(values = clr_ends, name = "Last bus to catch", drop = FALSE) +
  coord_map_box(st_bbox(lan)) +
  theme_map() +
  theme(legend.position.inside = c(0.99, 0.02), legend.justification.inside = c(1, 0), legend.title = element_text(size = 13, hjust = 0), legend.text = element_text(size = 12), legend.key.size = unit(5, "mm"))

save_map(p, here::here("2026/09-urban-rural/09-urban-rural-last-bus.png"),
  title = "The last bus home",
  subtitle = str_glue("{scales::comma(stranded, 100)} people in the countryside have no bus home\\ from Uppsala C by midnight. Almost no one in towns") |> as.character(),
  caption = caption_src("UL GTFS via Trafiklab, SCB, OpenStreetMap")
)
