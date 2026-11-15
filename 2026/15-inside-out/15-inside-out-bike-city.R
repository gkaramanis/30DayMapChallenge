library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")

# Cycle route classes cut from NVDB Uppsala län (Lastkajen, login) to the view
cycle_gpkg <- file.path(dir_data, "nvdb/cykelvagsklass-uppsala-centrum.gpkg")
nvdb_gpkg <- file.path(dir_data, "nvdb/uppsala_lan.gpkg")
# osmium extract -b 16.7,59.3,18.95,60.75 sweden-latest.osm.pbf (Geofabrik)
pbf <- file.path(dir_data, "r5/uppsala.osm.pbf")

centre <- st_point(c(17.646, 59.858)) |> st_sfc(crs = 4326) |> st_transform(crs_se) |> st_coordinates()
view <- map_box_view(centre, 6000)
view_wgs <- st_as_text(st_as_sfc(st_bbox(st_transform(st_as_sfc(view), 4326))))

# Cycle network from NVDB

if (!file.exists(cycle_gpkg)) st_read(nvdb_gpkg, layer = "NVDB_DK_O_107_CykelVgsKat", quiet = TRUE) |> st_zm() |> st_crop(view) |> select(Forbindelsekategori) |> st_write(cycle_gpkg, quiet = TRUE)

cycleways <- st_read(cycle_gpkg, quiet = TRUE)
local_cycleways <- filter(cycleways, Forbindelsekategori == "Lokal cykelväg")
# Main includes the one regional segment
main_cycleways <- filter(cycleways, Forbindelsekategori != "Lokal cykelväg")

# Buildings and water from OSM

read_osm <- function(layer, where) {
  sql <- paste("SELECT osm_id FROM", layer, "WHERE", where)
  st_read(pbf, query = sql, wkt_filter = view_wgs, quiet = TRUE) |> st_transform(crs_se) |> st_make_valid() |> st_crop(view)
}

buildings <- read_osm("multipolygons", "building IS NOT NULL")
water <- bind_rows(read_osm("multipolygons", "natural = 'water'"), read_osm("lines", "waterway = 'river'") |> st_buffer(8))

# Map

width_main <- 1
width_local <- 0.6
casing <- 0.35
clr_local <- "#F4CF8F"

cycle_line <- function(data, color, width) geom_sf(data = data, color = color, linewidth = width, lineend = "round")

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

# Casings first, so fills join at junctions
p <- ggplot() +
  geom_sf(data = water, fill = clr_water, color = NA) +
  geom_sf(data = buildings, fill = clr_grid, color = NA) +
  cycle_line(local_cycleways, clr_text, width_local + casing) +
  cycle_line(main_cycleways, clr_text, width_main + casing) +
  cycle_line(local_cycleways, clr_local, width_local) +
  cycle_line(main_cycleways, clr_accent, width_main) +
  coord_sf(xlim = view[c("xmin", "xmax")], ylim = view[c("ymin", "ymax")], crs = crs_se, expand = FALSE) +
  theme_map(bg = "white")

save_map(
  p, here::here("2026/15-inside-out/15-inside-out-bike-city.png"),
  title = "Uppsala's road map,\\ with bikes in place of cars",
  subtitle = "Main cycle routes in amber, local ones lighter. \\ Car roads are left off",
  caption = caption_src("Trafikverket, OpenStreetMap")
)
