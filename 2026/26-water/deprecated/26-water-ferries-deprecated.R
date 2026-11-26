library(tidyverse)
library(sf)

source(here::here("2026", "R", "helpers.R"))

dir_data <- file.path(dir_2026, "data")
pbf <- file.path(dir_data, "r5", "uppsala.osm.pbf")
path_ferry <- file.path(dir_data, "pbf-ferries-water-routes.rds")

# Ferry routes and canal-like waterways from the pbf, with tags parsed from other_tags
read_pbf <- function(layer, where) {
  read_sf(pbf, layer = layer, query = str_glue("SELECT * FROM {layer} WHERE {where}"))
}

if (!file.exists(path_ferry)) {
  w_lines <- "other_tags LIKE '%\"route\"=>\"ferry\"%' OR other_tags LIKE '%\"ferry\"=>%' OR waterway IN ('canal', 'lock_gate') OR other_tags LIKE '%\"seamark:type\"=>\"fairway\"%'"
  w_multi <- "other_tags LIKE '%\"route\"=>\"ferry\"%'"
  lines <- read_pbf("lines", w_lines)
  multi <- read_pbf("multilinestrings", w_multi)
  saveRDS(list(lines = lines, multi = multi), path_ferry)
}
routes <- readRDS(path_ferry)

parse_tag <- function(x, key) str_match(x, str_c('"', key, '"=>"([^"]*)"'))[, 2]

ferry <- bind_rows(routes$lines, routes$multi) |>
  mutate(route = parse_tag(other_tags, "route"), ferry = parse_tag(other_tags, "ferry"), seamark = parse_tag(other_tags, "seamark:type")) |>
  st_transform(crs_se)

county <- readRDS(file.path(dir_data, "boundary-uppsala_lan.rds")) |>
  st_transform(crs_se)

view <- map_box_view(c(662000, 6670500), 185000)
view_sf <- st_as_sfc(view)

# Ferries that touch the county or lie within a few km of it
county_near <- st_buffer(st_union(county), 3000)

ferries <- ferry |>
  filter(route == "ferry" | !is.na(ferry)) |>
  st_filter(county_near)

canals <- ferry |>
  filter(waterway %in% c("canal", "lock_gate")) |>
  st_filter(county_near)

water <- readRDS(file.path(dir_data, "pbf-water.rds")) |>
  st_transform(crs_se) |>
  st_filter(view_sf)

p <- ggplot() +
  geom_sf(data = water, fill = clr_water, color = NA) +
  geom_sf(data = county, fill = NA, color = clr_text2, linewidth = 0.2) +
  geom_sf(data = canals, color = clr_accent2, linewidth = 0.5) +
  geom_sf(data = ferries, color = clr_accent, linewidth = 0.8) +
  coord_map_box(view) +
  theme_map() +
  theme(panel.background = element_rect(fill = clr_land, color = NA))

ggsave(here::here("2026/26-water/alternatives/26-water-ferries.png"), p, width = map_box$width, height = map_box$height, dpi = 150)

print(ferries |> st_drop_geometry() |> count(name, sort = TRUE) |> head(40), n = 40)
print(nrow(canals))
