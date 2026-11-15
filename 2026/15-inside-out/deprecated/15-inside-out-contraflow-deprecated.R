library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")

# NVDB Uppsala län GeoPackage from Lastkajen (login), unzipped to data/nvdb/uppsala_lan.gpkg
nvdb_gpkg <- file.path(dir_data, "nvdb/uppsala_lan.gpkg")
# osmium extract -b 16.7,59.3,18.95,60.75 sweden-latest.osm.pbf (Geofabrik)
pbf <- file.path(dir_data, "r5/uppsala.osm.pbf")

centre <- st_point(c(17.643, 59.857)) |> st_sfc(crs = 4326) |> st_transform(crs_se) |> st_coordinates()
view <- map_box_view(centre, 4500)
view_wgs <- st_as_text(st_as_sfc(st_bbox(st_transform(st_as_sfc(view), 4326))))

# NVDB data -----------------------------------------------------------------

read_nvdb <- function(layer) st_read(nvdb_gpkg, layer = layer, quiet = TRUE) |> st_zm() |> st_crop(view)

# Rules in one direction with no time limit; DIRECTION is the prohibited direction relative to the digitised line
one_direction <- function(x) filter(x, DIRECTION %in% c("Med", "Mot"), is.na(Tidsintervall__Startdag1), is.na(Tidsintervall__Dagslag1), is.na(Tidsintervall__Klockslag__Starttid__Timme11))

# Local streets (functional class 6-9); main roads are mostly split carriageways, one-way by design
local_links <- read_nvdb("NVDB_DK_O_38_FunkVagklass") |> filter(Klass >= 6) |> pull(ELEMENT_ID) |> unique()
roundabout_links <- read_nvdb("NVDB_DK_O_11_Cirkulationsplats") |> pull(ELEMENT_ID) |> unique()

# One-way for motor vehicles only: bikes may ride both ways
contraflow <- read_nvdb("NVDB_DK_O_12_ForbudTrafik") |> one_direction() |> filter(Galler_fordon1 == "motordrivna fordon")

# One-way for everyone, on local streets
oneway <- read_nvdb("NVDB_DK_O_77_ForbjudenFardriktning") |> one_direction() |> filter(ELEMENT_ID %in% local_links, !ELEMENT_ID %in% c(roundabout_links, contraflow$ELEMENT_ID))

# Streets and arrows ----------------------------------------------------------

# Join link-by-link segments into whole streets
merge_streets <- function(geometry, directed) geometry |> st_cast("LINESTRING", warn = FALSE) |> st_union() |> st_line_merge(directed = directed) |> st_cast("LINESTRING") |> st_as_sf()

oneway_lines <- st_cast(oneway, "LINESTRING", warn = FALSE)
# Reverse "Med" lines so every line runs the way traffic may go
oneway_travel <- c(st_reverse(st_geometry(oneway_lines)[oneway_lines$DIRECTION == "Med"]), st_geometry(oneway_lines)[oneway_lines$DIRECTION == "Mot"])
oneway_streets <- merge_streets(oneway_travel, directed = TRUE)
contraflow_streets <- merge_streets(st_geometry(contraflow), directed = FALSE)

# Vertices in line order, so geom_path can put arrowheads at the street ends; short streets get none, as their heads would touch
street_path <- function(streets, min_len = 150) as_tibble(st_coordinates(streets)) |> mutate(long = as.numeric(st_length(streets))[L1] >= min_len)

oneway_path <- street_path(oneway_streets)
contraflow_path <- street_path(contraflow_streets)

# Base map from OSM -----------------------------------------------------------

read_osm <- function(layer, where) {
  sql <- paste("SELECT * FROM", layer, "WHERE", where)
  st_read(pbf, query = sql, wkt_filter = view_wgs, quiet = TRUE) |> st_transform(crs_se) |> st_crop(view)
}

roads <- read_osm("lines", "highway IN ('motorway', 'trunk', 'primary', 'secondary', 'tertiary', 'unclassified', 'residential', 'living_street', 'pedestrian', 'motorway_link', 'trunk_link', 'primary_link', 'secondary_link', 'tertiary_link')")
water <- bind_rows(read_osm("multipolygons", "natural = 'water'") |> select(osm_id), read_osm("lines", "waterway = 'river'") |> st_buffer(8) |> select(osm_id))

# Map -------------------------------------------------------------------------

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

p <- ggplot() +
  geom_sf(data = water, fill = clr_water, color = NA) +
  geom_sf(data = roads, color = clr_grid, linewidth = 0.4) +
  geom_path(data = filter(oneway_path, !long), aes(X, Y, group = L1), color = clr_text2, linewidth = 0.5) +
  geom_path(data = filter(oneway_path, long), aes(X, Y, group = L1), color = clr_text2, linewidth = 0.5, arrow = arrow(length = unit(1.5, "mm"), type = "open")) +
  geom_path(data = filter(contraflow_path, !long), aes(X, Y, group = L1), color = clr_accent2, linewidth = 0.8, lineend = "round") +
  geom_path(data = filter(contraflow_path, long), aes(X, Y, group = L1), color = clr_accent2, linewidth = 0.8, arrow = arrow(length = unit(1.5, "mm"), type = "open", ends = "both")) +
  coord_sf(xlim = view[c("xmin", "xmax")], ylim = view[c("ymin", "ymax")], crs = crs_se, expand = FALSE) +
  theme_map()

save_map(p, here::here("2026/15-inside-out/15-inside-out-contraflow.png"), title = "One way for cars, both ways for bikes", subtitle = "Bikes may ride both ways on the blue streets. \\ Grey streets are one-way for everyone", caption = caption_src("Trafikverket, OpenStreetMap"))
