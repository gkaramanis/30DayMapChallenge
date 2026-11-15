library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")

# UL GTFS, 1 November 2026 feed (covers all of November)
gtfs <- file.path(dir_data, "ul.zip")
# osmium extract -b 16.7,59.3,18.95,60.75 sweden-latest.osm.pbf (Geofabrik)
pbf <- file.path(dir_data, "r5/uppsala.osm.pbf")

walk_m <- 400

centre <- st_point(c(17.646, 59.858)) |> st_sfc(crs = 4326) |> st_transform(crs_se) |> st_coordinates()
view <- map_box_view(centre, 11000)
view_sfc <- st_as_sfc(view)
view_wgs <- st_as_text(st_as_sfc(st_bbox(st_transform(view_sfc, 4326))))

# Evaluate expr once and cache the result as rds in data/
cached <- function(name, expr) {
  path <- file.path(dir_data, paste0(name, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  saveRDS(expr, path)
  expr
}

# Stops ------------------------------------------------------------------------

# Only stops with departures; stops.txt also lists unused and parent stops
served_ids <- read_csv(unz(gtfs, "stop_times.txt"), col_select = stop_id, col_types = cols(stop_id = "c")) |> distinct(stop_id)
stops <- read_csv(unz(gtfs, "stops.txt"), col_types = cols(.default = "c")) |> semi_join(served_ids, by = join_by(stop_id)) |> mutate(across(c(stop_lat, stop_lon), as.numeric)) |> st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) |> st_transform(crs_se)

# Stops just outside the view still cut holes inside it
reach <- stops |> st_filter(st_buffer(view_sfc, walk_m)) |> st_buffer(walk_m) |> st_union()

# Land and water ----------------------------------------------------------------

read_osm <- function(layer, where) {
  sql <- paste("SELECT * FROM", layer, "WHERE", where)
  st_read(pbf, query = sql, wkt_filter = view_wgs, quiet = TRUE) |> st_transform(crs_se) |> st_make_valid() |> st_crop(view)
}

roads <- read_osm("lines", "highway IN ('motorway', 'trunk', 'primary', 'secondary', 'tertiary', 'unclassified', 'residential', 'living_street', 'pedestrian', 'service', 'motorway_link', 'trunk_link', 'primary_link', 'secondary_link', 'tertiary_link')")
water <- bind_rows(read_osm("multipolygons", "natural = 'water'") |> select(osm_id), read_osm("lines", "waterway = 'river'") |> st_buffer(8) |> select(osm_id)) |> st_union()

# Land beyond walking distance of every stop: the view, minus water, minus the holes
unserved <- view_sfc |> st_difference(water) |> st_difference(reach)

# Share of Uppsala's urban area (SCB tätort) beyond walking distance
urban <- cached("scb-tatort-uppsala-2023", st_read(paste0("https://geodata.scb.se/geoserver/stat/wfs?service=WFS&version=2.0.0&request=GetFeature&typeNames=stat:Tatorter_2023&outputFormat=application/json&CQL_FILTER=", URLencode("tatort='Uppsala' AND kommun='0380'", reserved = TRUE)), quiet = TRUE)) |> st_transform(crs_se) |> st_make_valid() |> st_union()
urban_reach <- stops |> st_filter(st_buffer(urban, walk_m)) |> st_buffer(walk_m) |> st_union()
urban_unserved <- as.numeric(st_area(st_difference(urban, urban_reach)) / st_area(urban))

# Map -------------------------------------------------------------------------

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

p <- ggplot() +
  geom_sf(data = water, fill = clr_water, color = NA) +
  geom_sf(data = roads, color = clr_text2, linewidth = 0.15) +
  geom_sf(data = unserved, fill = clr_text, color = NA) +
  coord_sf(xlim = view[c("xmin", "xmax")], ylim = view[c("ymin", "ymax")], crs = crs_se, expand = FALSE) +
  theme_map()

save_map(p, here::here("2026/15-inside-out/15-inside-out-bus-holes.png"), title = paste0(round(urban_unserved * 100), "% of urban Uppsala is far from a bus stop"), subtitle = paste0("Land more than ", walk_m, " m from a UL stop is filled. \\ Each hole is the ", walk_m, " m around a stop"), caption = caption_src("UL GTFS via Trafiklab, SCB, OpenStreetMap"))
