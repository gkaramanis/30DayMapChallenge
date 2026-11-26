library(tidyverse)
library(sf)
library(terra)
library(png)

source(here::here("2026", "R", "helpers.R"))

dir_data <- file.path(dir_2026, "data")

# MSB flood hazard maps (hotkartor_2026) are raster-only; exported once as transparent PNG
msb_export <- function(name, layer, bb, size) {
  path <- file.path(dir_data, name)
  if (!file.exists(path)) {
    url <- str_glue("https://gisapp.msb.se/arcgis/rest/services/Oversvamningskarteringar/hotkartor_2026/MapServer/export?bbox={str_c(bb, collapse = ',')}&bboxSR=3006&imageSR=3006&size={size}&layers=show:{layer}&format=png32&transparent=true&f=image")
    download.file(url, path, mode = "wb", quiet = TRUE)
  }
  path
}

# Flooded pixels (non-transparent) as polygons in SWEREF 99 TM
flood_polygons <- function(path, bb) {
  alpha <- readPNG(path)[, , 4]
  r <- rast(ifelse(alpha > 0, 1, NA), extent = ext(bb[c(1, 3, 2, 4)]), crs = "EPSG:3006")
  r |>
    as.polygons(dissolve = TRUE) |>
    st_as_sf() |>
    st_make_valid()
}

bb_sky <- c(642414, 6628773, 654277, 6644719)
bb_fyris <- c(645890, 6636747, 648878, 6640481)

sky <- flood_polygons(msb_export("msb-skyfall-100-uppsala.png", 231, bb_sky, "3048,4096"), bb_sky)
fyris <- flood_polygons(msb_export("msb-fyrisan-max-uppsala.png", 229, bb_fyris, "2988,3734"), bb_fyris)

view <- map_box_view(c(647900, 6638200), 8500)
view_sf <- st_as_sfc(view)

roads <- readRDS(file.path(dir_data, "osm-roads-city.rds"))$osm_lines |>
  select(osm_id, highway) |>
  st_transform(crs_se) |>
  st_filter(view_sf)

rail <- readRDS(file.path(dir_data, "osm-rail-lan.rds"))$osm_lines |>
  filter(railway %in% c("rail", "tram")) |>
  select(osm_id) |>
  st_transform(crs_se) |>
  st_filter(view_sf)

water_osm <- readRDS(file.path(dir_data, "osm-water-city.rds"))
water_river <- water_osm$osm_lines |>
  filter(waterway == "river") |>
  st_transform(crs_se) |>
  st_buffer(8) |>
  select(osm_id)

water <- bind_rows(select(water_osm$osm_polygons, osm_id), select(water_osm$osm_multipolygons, osm_id)) |>
  st_transform(crs_se) |>
  bind_rows(water_river) |>
  st_filter(view_sf)

p <- ggplot() +
  geom_sf(data = water, fill = clr_water, color = NA) +
  geom_sf(data = sky, fill = "#8FB8DE", color = NA) +
  geom_sf(data = fyris, fill = "#3D4F9F", color = NA) +
  geom_sf(data = roads, color = clr_text2, linewidth = 0.2) +
  geom_sf(data = rail, color = clr_accent, linewidth = 0.6) +
  coord_map_box(view) +
  theme_map()

ggsave(here::here("2026/26-water/alternatives/26-water-flood-roads-rail.png"), p, width = map_box$width, height = map_box$height, dpi = 150)
