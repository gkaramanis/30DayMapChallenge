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
  out <- expr
  saveRDS(out, path)
  out
}

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

lan_osm <- cached("boundary-uppsala_lan", {
  osmdata::opq(bb_lan, timeout = 300) |>
    osmdata::add_osm_feature("boundary", "administrative") |>
    osmdata::add_osm_feature("admin_level", "4") |>
    osmdata::add_osm_feature("name", "Uppsala län") |>
    osmdata::osmdata_sf() |>
    pluck("osm_multipolygons") |>
    select(name) |>
    st_make_valid()
})

lan <- st_transform(lan_osm, crs_se)

# GTFS Sverige 2 from Samtrafiken's open archive (CC0, no key)
gtfs_day <- "20261105"
gtfs_zip <- file.path(dir_data, "gtfs-sverige-2", paste0("sweden-", gtfs_day, ".zip"))

if (!file.exists(gtfs_zip)) {
  dir.create(dirname(gtfs_zip), showWarnings = FALSE)
  download.file(paste0("https://data.samtrafiken.se/trafiklab/gtfs-sverige-2/", substr(gtfs_day, 1, 4), "/", substr(gtfs_day, 5, 6), "/sweden-", gtfs_day, ".zip"), gtfs_zip, mode = "wb")
}

# Stops that rail trips call at (route_type 2 and 100-199)
rail_stops <- cached(paste0("gtfs-rail-stops-", gtfs_day), local({
  dir <- file.path(tempdir(), "gtfs-sverige-2")
  unzip(gtfs_zip, files = c("stops.txt", "trips.txt", "routes.txt", "stop_times.txt"), exdir = dir)
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  csv <- \(f) sprintf("read_csv('%s', all_varchar = true, quote = '\"', header = true)", file.path(dir, f))
  DBI::dbGetQuery(con, glue::glue("
    SELECT stop_id, stop_name, CAST(stop_lat AS DOUBLE) AS lat, CAST(stop_lon AS DOUBLE) AS lon
    FROM {csv('stops.txt')}
    WHERE stop_id IN (
      SELECT st.stop_id
      FROM {csv('stop_times.txt')} st JOIN {csv('trips.txt')} t USING (trip_id) JOIN {csv('routes.txt')} r USING (route_id)
      WHERE CAST(r.route_type AS INT) = 2 OR CAST(r.route_type AS INT) BETWEEN 100 AND 199)
  "))
}))

# Year-round stations only, without the Lennakatten heritage line; "Tierp station" becomes "Tierp"
stations <- st_as_sf(rail_stops, coords = c("lon", "lat"), crs = 4326) |>
  st_transform(crs_se) |>
  st_filter(lan) |>
  filter(!str_detect(stop_name, "Lennakatten")) |>
  mutate(name = str_remove(stop_name, " station$"))

rail_gpkg <- file.path(dir_data, "trv-jarnvagsnat", "Järnvägsnät_grundegenskaper3_0_GeoPackage.gpkg")
rail_area <- st_as_text(st_as_sfc(st_bbox(st_buffer(lan, 40000))))

rail <- read_sf(rail_gpkg, wkt_filter = rail_area) |>
  filter(SpTyp == "nhsp", Status == "Öppen")

p <- ggplot() +
  geom_sf(data = lan, fill = clr_land, color = clr_grid, linewidth = 0.3) +
  geom_sf(data = rail, linewidth = 0.5, color = clr_text2) +
  geom_sf(data = stations, shape = 21, fill = clr_bg, color = clr_accent2, size = 3, stroke = 1) +
  geom_sf(data = filter(stations, name == "Uppsala Centralstation"), shape = 21, fill = clr_accent, color = clr_text, size = 4.5, stroke = 0.8) +
  ggrepel::geom_text_repel(data = stations, aes(label = name, geometry = geometry), stat = "sf_coordinates", family = f1, size = 3.6, color = clr_text, seed = 1, max.overlaps = Inf, min.segment.length = Inf, bg.color = clr_land) +
  coord_map_box(st_bbox(st_buffer(st_union(stations), 12000))) +
  theme_map()

save_map(p, here::here("2026/07-10-minute/07-10-minute-stations.png"), 
  title = paste0(nrow(stations), " train stations\\ in Uppsala län"), 
  subtitle = "Lines from Stockholm, Gävle and Sala meet at Uppsala Centralstation (orange)",
  caption = paste(caption_src("Trafikverket, Samtrafiken, OpenStreetMap"), "· Made in 10 minutes")
)
