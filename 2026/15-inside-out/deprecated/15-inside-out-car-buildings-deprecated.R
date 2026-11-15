library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")

# osmium extract -b 16.7,59.3,18.95,60.75 sweden-latest.osm.pbf (Geofabrik)
pbf <- file.path(dir_data, "r5/uppsala.osm.pbf")

centre <- st_point(c(17.646, 59.858)) |> st_sfc(crs = 4326) |> st_transform(crs_se) |> st_coordinates()
view <- map_box_view(centre, 6000)
view_sfc <- st_as_sfc(view)
view_wgs <- st_as_text(st_as_sfc(st_bbox(st_transform(view_sfc, 4326))))

# Value of one key from the pbf's other_tags column
tag <- function(other_tags, key) str_match(other_tags, paste0('"', key, '"=>"([^"]+)"'))[, 2]

# GDAL's wkt_filter drops some large multipolygons, so the view filter happens after reading
read_osm <- function(layer, where) {
  sql <- paste("SELECT * FROM", layer, "WHERE", where)
  st_read(pbf, query = sql, wkt_filter = view_wgs, quiet = TRUE) |> st_transform(crs_se) |> st_make_valid() |> st_filter(view_sfc)
}

# Buildings ----------------------------------------------------------------------

car_types <- c("garage", "garages", "carport", "parking")

buildings <- read_osm("multipolygons", "building IS NOT NULL") |> mutate(parking = tag(other_tags, "parking"), for_cars = building %in% car_types | parking %in% "multi-storey")

car_buildings <- filter(buildings, for_cars)
other_buildings <- filter(buildings, !for_cars)

roads <- read_osm("lines", "highway IN ('motorway', 'trunk', 'primary', 'secondary', 'tertiary', 'unclassified', 'residential', 'living_street', 'motorway_link', 'trunk_link', 'primary_link', 'secondary_link', 'tertiary_link')") |> st_crop(view)
water <- bind_rows(read_osm("multipolygons", "natural = 'water'") |> select(osm_id), read_osm("lines", "waterway = 'river'") |> st_buffer(8) |> select(osm_id)) |> st_crop(view)

# Map -------------------------------------------------------------------------

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

p <- ggplot() +
  geom_sf(data = water, fill = clr_water, color = NA) +
  geom_sf(data = roads, color = clr_grid, linewidth = 0.3) +
  geom_sf(data = other_buildings, fill = clr_land, color = clr_grid, linewidth = 0.1) +
  geom_sf(data = car_buildings, fill = clr_accent, color = NA) +
  coord_sf(xlim = view[c("xmin", "xmax")], ylim = view[c("ymin", "ymax")], crs = crs_se, expand = FALSE) +
  theme_map()

save_map(p, here::here("2026/15-inside-out/15-inside-out-car-buildings.png"), title = paste("At least", format(nrow(car_buildings), big.mark = ","), "buildings in central Uppsala are for cars"), subtitle = "Garages, carports and car parks in amber. \\ Every other building in grey", caption = caption_src("OpenStreetMap"))
