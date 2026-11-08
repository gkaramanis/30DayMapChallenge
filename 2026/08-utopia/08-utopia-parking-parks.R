library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

dir_data <- file.path(dir_2026, "data")
dir.create(dir_data, showWarnings = FALSE, recursive = TRUE)

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

# Layers from the local OSM extract, cropped to the map box. GDAL's wkt_filter
# drops some large multipolygons, so the crop happens after reading.
pbf <- file.path(dir_data, "r5", "uppsala.osm.pbf")

pbf_read <- function(name, layer, where) {
  cached(name, st_read(pbf, query = paste("SELECT name, other_tags FROM", layer, "WHERE", where), quiet = TRUE)) |>
    st_transform(crs_se) |>
    st_make_valid() |>
    st_filter(st_as_sfc(view))
}

hectares <- function(x) as.numeric(st_area(x)) / 1e4

# Uppsala tätort, SCB's statistical urban area, and its public green areas
urban <- scb_wfs("scb-tatort-uppsala-2023", "Tatorter_2023", "tatort='Uppsala' AND kommun='0380'") |> st_geometry()
green_scb <- scb_wfs("scb-gronomraden-uppsala-2020", "Gronomraden_2020_tatort", "tatortskod='0380TC125'")

# Map box around the urban area, so water reaches every edge
bb_urban <- st_bbox(urban)
urban_w <- bb_urban[["xmax"]] - bb_urban[["xmin"]]
urban_h <- bb_urban[["ymax"]] - bb_urban[["ymin"]]
view <- map_box_view(c(bb_urban[["xmin"]] + urban_w / 2, bb_urban[["ymin"]] + urban_h / 2), max(urban_w, urban_h * map_box$width / map_box$height))

# Underground and rooftop car parks have no ground to plant
parking <- pbf_read("pbf-parking", "multipolygons", "amenity = 'parking'") |>
  filter(!str_detect(coalesce(other_tags, ""), '"parking"=>"(underground|rooftop)"'))

green_today <- st_intersection(st_union(green_scb), urban)

# Car parks already inside a green area add nothing new
green_new <- st_union(parking) |>
  st_difference(green_today) |>
  st_intersection(urban)

kinds <- c("Public green areas today", "Car park turned park")
green <- st_sf(kind = factor(kinds, levels = kinds), geometry = c(green_today, green_new))

# The two largest car park areas against the City Garden. Lots less than 75 m
# apart (about a road and its verges) form one area, named after the nearest
# OSM suburb.
stadstradgarden <- pbf_read("pbf-parks", "multipolygons", "leisure = 'park'") |>
  filter(name == "Stadsträdgården")

suburbs <- pbf_read("pbf-suburbs", "points", "place = 'suburb'")

lots <- st_sf(geometry = st_cast(st_collection_extract(green_new, "POLYGON"), "POLYGON"))
clusters <- st_sf(geometry = st_cast(st_union(st_buffer(lots, 75 / 2)), "POLYGON")) |> mutate(id = row_number())

largest <- st_join(lots, clusters) |>
  group_by(id) |>
  summarise() |>
  slice_max(hectares(geometry), n = 2) |>
  mutate(name = paste(suburbs$name[st_nearest_feature(st_centroid(geometry), suburbs)], "car parks"))

compare <- bind_rows(st_sf(name = "Uppsala City Garden", geometry = st_union(stadstradgarden)), select(largest, name)) |>
  mutate(label = paste0(name, "\n", round(hectares(geometry), 1), " ha"))

# Lakes, plus rivers as 8 m bands
water <- bind_rows(
  pbf_read("pbf-water", "multipolygons", "natural = 'water'"),
  pbf_read("pbf-rivers", "lines", "waterway = 'river'") |> st_buffer(8)
)

p <- ggplot() +
  geom_sf(data = water, fill = clr_water, color = NA) +
  geom_sf(data = green, aes(fill = kind), color = NA) +
  geom_sf(data = compare, fill = NA, color = clr_text, linewidth = 0.3) +
  ggrepel::geom_text_repel(data = compare, aes(geometry = geometry, label = label), stat = "sf_coordinates", family = f1, fontface = "bold", size = 5, lineheight = 1, color = clr_text, bg.color = "white", seed = 1, min.segment.length = 0, force_pull = -0.5, point.padding = 0.8, segment.color = clr_accent) +
  scale_fill_manual(values = set_names(c("#B9D3A8", "#1F5C1E"), kinds), name = NULL) +
  coord_sf(xlim = view[c("xmin", "xmax")], ylim = view[c("ymin", "ymax")], crs = crs_se, expand = FALSE) +
  theme_map(bg = "white") +
  theme(legend.background = element_rect(fill = "white", color = NA), legend.margin = margin(6, 8, 6, 8))

save_map(p, here::here("2026/08-utopia/08-utopia-parking-parks.png"),
  title = "What if the car parks \\ became parks?",
  subtitle = str_glue("If every car park turned into a park, Uppsala would \\ gain {round(hectares(green_new))} hectares, or {round(100 * hectares(green_new) / hectares(green_today))}%, more public green areas") |> as.character(),
  caption = caption_src("OpenStreetMap, SCB 2020")
)
