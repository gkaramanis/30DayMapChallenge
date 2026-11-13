library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")
dir.create(dir_data, showWarnings = FALSE)

bb_lan <- c(16.70, 59.30, 18.95, 60.75)

# Evaluate expr once and cache the result as rds in data/
cached <- function(name, expr) {
  path <- file.path(dir_data, paste0(name, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  saveRDS(expr, path)
  expr
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

uppsala_lan <- function() osm_boundary("Uppsala län", "4", bb_lan)

# Keys from https://developer.trafiklab.se, stored in ~/.Renviron:
# TRAFIKLAB_GTFS_REGIONAL_KEY, TRAFIKLAB_GTFS_SWEDEN_KEY
gtfs_download <- function(url, file) {
  path <- file.path(dir_data, file)
  if (!file.exists(path)) {
    download.file(url, path, mode = "wb", headers = c("Accept-Encoding" = "gzip"))
  }
  path
}

sweden_gtfs <- function() {
  key <- Sys.getenv("TRAFIKLAB_GTFS_SWEDEN_KEY")
  stopifnot("Set TRAFIKLAB_GTFS_SWEDEN_KEY" = key != "" || file.exists(file.path(dir_data, "sweden.zip")))
  path <- gtfs_download(paste0("https://opendata.samtrafiken.se/gtfs-sweden/sweden.zip?key=", key), "sweden.zip")
  tidytransit::read_gtfs(path)
}

# service_ids running on a date, from calendar and calendar_dates
services_on <- function(gtfs, on) {
  day <- c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")[lubridate::wday(on, week_start = 1)]
  ids <- character()
  if (!is.null(gtfs$calendar) && nrow(gtfs$calendar) > 0) {
    ids <- gtfs$calendar |>
      filter(start_date <= on, end_date >= on, .data[[day]] == 1) |>
      pull(service_id)
  }
  if (!is.null(gtfs$calendar_dates)) {
    cd <- gtfs$calendar_dates |> filter(date == on)
    ids <- union(setdiff(ids, cd$service_id[cd$exception_type == 2]), cd$service_id[cd$exception_type == 1])
  }
  ids
}

# stop_times for trips on a date, with route info and departure in seconds
stop_times_on <- function(gtfs, on) {
  trips <- gtfs$trips |>
    filter(service_id %in% services_on(gtfs, on)) |>
    left_join(gtfs$routes |> select(route_id, route_short_name, route_type, any_of("agency_id")), by = "route_id")
  gtfs$stop_times |>
    inner_join(trips, by = "trip_id") |>
    mutate(dep_s = as.numeric(departure_time), arr_s = as.numeric(arrival_time))
}

stops_sf <- function(gtfs) {
  stops <- gtfs$stops
  if (!"parent_station" %in% names(stops)) stops$parent_station <- NA_character_
  stops |>
    filter(!is.na(stop_lat)) |>
    st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326, remove = FALSE)
}

is_rail <- function(route_type) route_type == 2 | between(route_type, 100, 199)


gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

# National feed: UL plus all passenger rail. Not yet every operator (e.g. some
# long-distance buses); GTFS Sverige 2 is complete but has no shapes.
# Large: needs several GB of RAM.
gtfs <- sweden_gtfs()
on <- as.Date("2026-11-13")

lan <- uppsala_lan()

stops_lan <- stops_sf(gtfs) |>
  st_filter(lan) |>
  st_transform(crs_se)

st_lan <- stop_times_on(gtfs, on) |>
  semi_join(st_drop_geometry(stops_lan), by = "stop_id") |>
  mutate(mode = if_else(is_rail(route_type), "rail", "bus"))

# Interchanges: rail stations plus every stop within 300 m of them
stations <- stops_lan |>
  semi_join(st_lan |> filter(mode == "rail"), by = "stop_id") |>
  mutate(group_id = coalesce(na_if(parent_station, ""), stop_id)) |>
  group_by(group_id) |>
  summarise(name = first(stop_name)) |>
  # Uppsala C has separate train and bus station groups; treat as one
  mutate(name = str_remove(name, " Tåg$")) |>
  group_by(name) |>
  summarise(group_id = first(group_id)) |>
  st_centroid()

catchment <- st_join(stops_lan |> select(stop_id), st_buffer(stations, 300), left = FALSE)

interchanges <- st_lan |>
  inner_join(st_drop_geometry(catchment), by = "stop_id") |>
  group_by(group_id, name) |>
  summarise(departures = n(), rail = sum(mode == "rail"), operators = n_distinct(agency_id), .groups = "drop") |>
  left_join(stations |> select(group_id), by = "group_id") |>
  st_as_sf() |>
  arrange(departures)

lan <- lan |> st_transform(crs_se)

# Trafikverket's Järnvägsnät med grundegenskaper 3.0 (Lastkajen), in SWEREF 99 TM.
# Open running track: main lines (nhsp) and Lennakatten (tågspår)

rail_gpkg <- file.path(dir_data, "trv-jarnvagsnat", "Järnvägsnät_grundegenskaper3_0_GeoPackage.gpkg")
rail_area <- st_as_text(st_as_sfc(st_bbox(lan)))

rail_lines <- read_sf(rail_gpkg, wkt_filter = rail_area) |>
  filter(SpTyp %in% c("nhsp", "tågspår"), Status == "Öppen")

p <- ggplot() +
  geom_sf(data = lan, fill = clr_land, color = NA) +
  geom_sf(data = st_intersection(rail_lines, lan), color = clr_text2, linewidth = 0.4) +
  geom_sf(data = interchanges, aes(size = departures, fill = operators), shape = 21, color = clr_bg, stroke = 0.6) +
  ggrepel::geom_text_repel(data = interchanges |> slice_max(departures, n = 10), aes(label = str_remove(name, " station.*$"), geometry = geometry), stat = "sf_coordinates", family = f1, size = 4, color = clr_text, seed = 2, bg.color = clr_bg, bg.r = 0.1) +
  scale_size_area(max_size = 14, labels = scales::label_number(big.mark = " ")) +
  scale_fill_steps(low = clr_accent, high = clr_accent2, breaks = 1:6, guide = guide_coloursteps(title = NULL, order = 2)) +
  guides(size = guide_legend(title = NULL, order = 1, override.aes = list(shape = 21, fill = clr_accent2, color = clr_bg))) +
  coord_map_box(st_bbox(lan)) +
  theme_map() +
  theme(legend.box = "horizontal", legend.key.width = unit(1.6, "lines"), legend.key.height = unit(0.8, "lines"), legend.spacing.x = unit(0.3, "lines"), legend.text = element_text(size = 12))

save_map(p, here::here("2026/13-interactions/13-interactions-interchanges.png"), title = "Where trains and \\ buses meet", subtitle = paste0("Departures within 300 m of each railway station in Uppsala County on ", str_squish(format(on, "%A %e %B %Y")), ", \\ coloured by number of operators"), caption = caption_src("GTFS Sweden 3 via Trafiklab, Trafikverket, OpenStreetMap"))
