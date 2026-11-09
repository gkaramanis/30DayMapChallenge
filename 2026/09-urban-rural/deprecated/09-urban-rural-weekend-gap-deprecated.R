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

tatorter <- scb_wfs("scb-tatorter-lan-2023", "Tatorter_2023", "lan='03'")
uppsala <- filter(tatorter, tatortskod == "0380TC125")
# Sävja and Alsike sit against Uppsala and would crowd its label
towns <- filter(tatorter, bef >= 4000, !tatort %in% c("Sävja", "Alsike")) |> st_point_on_surface()

# Key from https://developer.trafiklab.se, stored in ~/.Renviron
ul_zip <- file.path(dir_data, "ul.zip")
if (!file.exists(ul_zip)) {
  key <- Sys.getenv("TRAFIKLAB_GTFS_REGIONAL_KEY")
  download.file(paste0("https://opendata.samtrafiken.se/gtfs/ul/ul.zip?key=", key), ul_zip, mode = "wb", headers = c("Accept-Encoding" = "gzip"))
}
saturday <- as.Date("2026-11-07")
gtfs <- tidytransit::read_gtfs(ul_zip) |> tidytransit::filter_feed_by_date(saturday)

# The clock hours in which each stop has at least one departure. Hours past
# midnight run on as 24, 25 … in GTFS and count as Saturday night.
stop_hours <- gtfs$stop_times |>
  filter(!is.na(departure_time)) |>
  distinct(stop_id, hour = as.numeric(departure_time) %/% 3600) |>
  summarise(hours = list(hour), .by = stop_id)

stops <- gtfs$stops |>
  filter(!is.na(stop_lat)) |>
  inner_join(stop_hours, by = join_by(stop_id)) |>
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) |>
  st_transform(crs_se)

# 1 km grid aligned with SCB's SWEREF 99 TM grid, cut to the county outline.
# Each square counts the hours with a bus from any stop within 1 km of its
# centre, about a 15-minute walk, up to 24.
bb <- st_bbox(lan)
grid <- st_make_grid(lan, cellsize = 1000, offset = floor(bb[c("xmin", "ymin")] / 1000) * 1000) |>
  st_as_sf() |>
  rename(geometry = x) |>
  st_filter(lan) |>
  mutate(cell = row_number())

centre_xy <- st_coordinates(st_centroid(st_geometry(grid)))
near <- st_is_within_distance(st_centroid(st_geometry(grid)), stops, dist = 1000)

bins <- c("No Saturday bus", "1–8 hours", "9–16 hours", "17–24 hours")

grid_hours <- grid |>
  mutate(
    hours = map_int(near, \(i) min(length(unique(unlist(stops$hours[i]))), 24L)),
    span = factor(if_else(hours == 0, bins[1], as.character(cut(hours, c(0, 8, 16, 24), labels = bins[-1]))), levels = bins)
  )

# SCB population per 1 km square, end of 2025, on the same grid. SCB lists
# inhabited squares only.
pop <- scb_wfs("scb-befolkning-1km-2025-lan", "befolkning_1km_2025", paste0("BBOX(sp_geometry,", paste(bb[c("xmin", "ymin", "xmax", "ymax")], collapse = ","), ",'EPSG:3006')")) |>
  st_drop_geometry() |>
  transmute(rutid = rutid_scb, people = beftotalt)

# Whether each square's centre lies in Uppsala, in another tätort or in the
# countryside
in_tatort <- st_within(st_centroid(st_geometry(grid)), st_geometry(tatorter))

shares <- grid_hours |>
  st_drop_geometry() |>
  mutate(
    rutid = sprintf("%.0f%.0f", round(centre_xy[, 1] - 500), round(centre_xy[, 2] - 500)),
    area = map_chr(in_tatort, \(i) if (length(i) == 0) "Countryside" else if (tatorter$tatortskod[i[1]] == "0380TC125") "Uppsala" else "Towns")
  ) |>
  inner_join(pop, by = join_by(rutid)) |>
  filter(area != "Uppsala") |>
  count(area, span, wt = people, name = "people") |>
  mutate(share = people / sum(people), .by = area)

no_bus <- shares |>
  filter(span == bins[1]) |>
  select(area, share) |>
  deframe()

grid_bus <- grid_hours |>
  # One shape per class, so no seams show between squares
  group_by(span) |>
  summarise() |>
  st_intersection(st_geometry(lan))

# One hue, darker the more hours a bus runs; no Saturday bus is plain land
clr_span <- set_names(c(clr_land, "#C9D0EE", "#6F7DC1", "#1C2763"), bins)

p <- ggplot() +
  geom_sf(data = grid_bus, aes(fill = span), color = NA) +
  geom_sf(data = lan, fill = NA, color = clr_text2, linewidth = 0.2) +
  geom_sf(data = uppsala, fill = clr_accent, color = NA) +
  ggrepel::geom_text_repel(data = towns, aes(label = tatort, geometry = geometry), stat = "sf_coordinates", family = f1, size = 3.6, color = clr_text, bg.color = clr_bg, bg.r = 0.12, seed = 1, min.segment.length = Inf) +
  scale_fill_manual(values = clr_span, name = "Hours with a bus", drop = FALSE) +
  coord_map_box(st_bbox(lan)) +
  theme_map() +
  theme(legend.position.inside = c(0.99, 0.02), legend.justification.inside = c(1, 0), legend.title = element_text(size = 13, hjust = 0), legend.text = element_text(size = 12), legend.key.size = unit(5, "mm"))

save_map(p, here::here("2026/09-urban-rural/09-urban-rural-weekend-gap.png"),
  title = "The weekend gap",
  subtitle = str_glue("On Saturdays, {scales::percent(no_bus[['Countryside']])} of countryside residents have\\ no bus within 1 km. In towns, {scales::percent(no_bus[['Towns']])}") |> as.character(),
  caption = caption_src("UL GTFS via Trafiklab, SCB, OpenStreetMap")
)
