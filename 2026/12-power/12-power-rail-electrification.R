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

# OSM tag column, or NA when no feature carries the tag
tag <- function(df, key) {
  if (key %in% names(df)) df[[key]] else rep(NA_character_, nrow(df))
}

osm_get <- function(name, bb, key, value) {
  cached(paste0("osm-", name), osmdata::opq(bb, timeout = 600) |> osmdata::add_osm_feature(key, value) |> osmdata::osmdata_sf())
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

bb_lan <- c(16.70, 59.30, 18.95, 60.75)

lan <- osm_boundary("Uppsala län", "4", bb_lan) |> st_transform(crs_se)

# Trafikverket's Järnvägsnät med grundegenskaper 3.0 (Lastkajen), in SWEREF 99 TM.
# Open running track: main lines (nhsp) and Lennakatten (tågspår)
rail_gpkg <- file.path(dir_data, "trv-jarnvagsnat", "Järnvägsnät_grundegenskaper3_0_GeoPackage.gpkg")
rail_area <- st_as_text(st_buffer(st_as_sfc(st_bbox(lan)), 30000))

rail_all <- read_sf(rail_gpkg, wkt_filter = rail_area) |>
  filter(SpTyp %in% c("nhsp", "tågspår"), Status == "Öppen") |>
  mutate(electrified = Elektrifi != "ej el")

# Track inside the county, for the lengths
rail <- st_intersection(rail_all, lan)

# View around the county's railways, cropped at the map box ratio
rail_centre <- st_coordinates(st_centroid(st_as_sfc(st_bbox(rail))))
view <- map_box_view(rail_centre, 128000)

# All track in view, drawn past the county border
rail_view <- st_crop(rail_all, view)
wired_view <- filter(rail_view, electrified)
unwired_view <- filter(rail_view, !electrified)

# GADM counties, clipped to the coast, for land. GADM's Uppsala lacks Heby
# (moved in 2007), so the outline is the OSM county clipped to that land
counties <- cached("gadm41-swe-1", read_sf("/vsizip//vsicurl/https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_SWE_1.json.zip"))

land <- counties |>
  st_transform(crs_se) |>
  st_make_valid() |>
  st_crop(view)

lan_land <- st_intersection(lan, st_union(land))

# Everything outside the county, faded under a mask
outside <- st_difference(st_as_sfc(view), st_union(lan_land))

substations <- osm_get("substations-lan", bb_lan, "power", "substation")

traction <- bind_rows(
  substations$osm_points |> select(osm_id, any_of(c("substation", "name"))),
  substations$osm_polygons |> select(osm_id, any_of(c("substation", "name"))) |> st_centroid()
) |>
  filter(tag(pick(everything()), "substation") %in% "traction") |>
  st_transform(crs_se) |>
  st_filter(lan)

power_lines <- osm_get("power-lines-lan", bb_lan, "power", "line")$osm_lines |>
  st_transform(crs_se) |>
  st_crop(view)

# The railway's own grid runs at 16.7 Hz, the rest of the grid at 50 Hz
rail_grid <- filter(power_lines, str_detect(tag(pick(everything()), "frequency"), "^16") %in% TRUE)

# The two lines without overhead wires, labelled at a point along each.
# Hargshamnsbanan is bandel 435; Lennakatten is the narrow-gauge tågspår
unwired <- rail |>
  filter(!electrified) |>
  mutate(line = if_else(Bandel == "435", "Hargshamnsbanan", "Lennakatten")) |>
  summarise(geometry = st_union(geom), .by = line) |>
  mutate(
    km = round(as.numeric(st_length(geometry)) / 1000),
    use = if_else(line == "Lennakatten", "heritage railway", "freight"),
    label = str_glue("{line}, {use}\n{km} km without wires"),
    vjust = if_else(line == "Lennakatten", 1.7, -0.6),
    nudge = if_else(line == "Lennakatten", 12000, 0)
  )

unwired_labels <- unwired |>
  mutate(
    geometry = st_point_on_surface(geometry),
    x = st_coordinates(geometry)[, 1] + nudge,
    y = st_coordinates(geometry)[, 2]
  ) |>
  st_drop_geometry()

# Towns along the railways; hjust and vjust place each label off the track
towns <- tribble(
  ~name, ~lon, ~lat, ~hjust, ~vjust,
  "Uppsala", 17.638, 59.858, 1.15, 1.4,
  "Enköping", 17.077, 59.636, 0.5, 1.8,
  "Bålsta", 17.531, 59.567, 1.2, 1.6,
  "Knivsta", 17.787, 59.725, 1.25, 0.5,
  "Tierp", 17.517, 60.343, 1.4, 0.5,
  "Örbyhus", 17.705, 60.226, 1.2, 0.5,
  "Heby", 16.862, 59.938, 0.5, 1.8
) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(crs_se)

# Glow from stacked lines, wide and faint to narrow and bright
glow <- map(c(5, 3, 1.6, 0.6), \(w) geom_sf(data = wired_view, color = clr_accent, linewidth = w, alpha = 0.45 / w))

p <- ggplot() +
  geom_sf(data = land, fill = clr_land, color = NA) +
  geom_sf(data = power_lines, color = clr_text2, alpha = 0.35, linewidth = 0.2) +
  geom_sf(data = rail_grid, color = clr_accent2, linewidth = 0.7) +
  glow +
  geom_sf(data = unwired_view, color = clr_text, linewidth = 0.9, linetype = "22") +
  geom_sf(data = outside, fill = clr_bg, alpha = 0.7, color = NA) +
  geom_sf(data = lan_land, fill = NA, color = clr_text2, linewidth = 0.6) +
  geom_sf(data = traction, shape = 21, fill = clr_text, color = clr_land, stroke = 1, size = 3) +
  geom_text(data = unwired_labels, aes(x, y, label = label, vjust = vjust), family = f1, color = clr_text, size = 4.5, lineheight = 0.9) +
  # Along the 400 kV line from Forsmark, in the empty northeast
  annotate("text", x = 659000, y = 6698300, label = "National grid", angle = -3, vjust = -0.5, family = f1, color = clr_text2, size = 3.8) +
  geom_sf_text(data = towns, aes(label = name, hjust = hjust, vjust = vjust), family = f1, fontface = "bold", color = clr_text2, size = 4.2) +
  coord_map_box(view) +
  theme_map(bg = clr_water)

save_map(p, here::here("2026/12-power/12-power-rail-electrification.png"),
  title = "Uppsala County's passenger\\ trains all run under wires",
  subtitle = "Blue lines carry the railway's own power,\\ dots mark where it reaches the wires",
  caption = caption_src("Trafikverket, OpenStreetMap, GADM")
)
