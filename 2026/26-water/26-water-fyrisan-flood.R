library(tidyverse)
library(sf)
library(terra)
library(png)
library(patchwork)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")

# Area MSB modeled for Fyrisån in hotkartor_2026, SWEREF 99 TM, and the two flood layers in it
bb_msb <- c(xmin = 645618, ymin = 6636743, xmax = 649130, ymax = 6640697)
scenarios <- c("1 in 100 years" = 228, "1 in 10,000 years" = 229)
modes <- c("Streets", "Cycleways", "Railway")

clr_flood <- c("1 in 100 years" = "#8F9BD0", "1 in 10,000 years" = "#D2D8F0")
clr_flooded <- "#E08410"
clr_dry <- "#CFCFC7"
line_width <- c(Streets = 0.3, Cycleways = 0.2, Railway = 0.8)

# MSB's full height in the map box's shape, a little wider than the modeled area
view <- map_box_view(c(mean(bb_msb[c("xmin", "xmax")]), mean(bb_msb[c("ymin", "ymax")])), (bb_msb[["ymax"]] - bb_msb[["ymin"]]) * map_box$width / map_box$height)
view_sf <- st_as_sfc(view)

# The flood maps are raster-only, so each layer is exported once as a transparent PNG at 1 m per pixel
msb_png <- function(layer) {
  path <- file.path(dir_data, str_glue("msb-fyrisan-{layer}-1m.png"))
  if (!file.exists(path)) {
    size <- str_c(bb_msb[["xmax"]] - bb_msb[["xmin"]], ",", bb_msb[["ymax"]] - bb_msb[["ymin"]])
    url <- str_glue("https://gisapp.msb.se/arcgis/rest/services/Oversvamningskarteringar/hotkartor_2026/MapServer/export?bbox={str_c(bb_msb, collapse = ',')}&bboxSR=3006&imageSR=3006&size={size}&layers=show:{layer}&format=png32&transparent=true&f=image")
    download.file(url, path, mode = "wb", quiet = TRUE)
  }
  path
}

# Water deeper than 0.5 m as one polygon. MSB draws 0-0.5 m in rgb(187, 246, 252) and the deeper classes with red 119 or less.
flood_deep <- function(layer) {
  img <- readPNG(msb_png(layer))
  deep <- img[, , 4] > 0 & img[, , 1] < 150 / 255
  rast(ifelse(deep, 1, NA), extent = ext(bb_msb[c("xmin", "xmax", "ymin", "ymax")]), crs = "EPSG:3006") |>
    as.polygons() |>
    st_as_sf() |>
    st_make_valid() |>
    st_union()
}

# OSM ways cropped to the view. Bridges are left out, since the flood layers include the river beneath them.
osm_network <- function(file, mode, ...) {
  readRDS(file.path(dir_data, file))$osm_lines |>
    filter(..., is.na(bridge) | bridge == "no") |>
    transmute(mode = mode) |>
    st_transform(crs_se) |>
    st_intersection(view_sf)
}

# Kilometers of each mode inside a flood zone
km_by_mode <- function(zone) {
  network |>
    st_intersection(zone) |>
    mutate(km = as.numeric(st_length(geometry)) / 1000) |>
    st_drop_geometry() |>
    summarise(km = sum(km), .by = mode)
}

floods <- map(scenarios, flood_deep)

# Service ways (driveways, parking aisles) are not streets
network <- bind_rows(
  osm_network("osm-roads-city.rds", "Streets", highway != "service"),
  osm_network("osm-cycleways-city.rds", "Cycleways"),
  osm_network("osm-rail-lan.rds", "Railway", railway == "rail")
) |>
  mutate(mode = factor(mode, levels = modes), width = line_width[as.character(mode)])

flooded <- st_intersection(network, reduce(floods, st_union))

flood_km <- map(floods, km_by_mode) |>
  list_rbind(names_to = "scenario") |>
  complete(mode, scenario, fill = list(km = 0))

water_osm <- readRDS(file.path(dir_data, "osm-water-city.rds"))
river <- water_osm$osm_lines |>
  filter(waterway == "river") |>
  st_transform(crs_se) |>
  st_buffer(8) |>
  select(osm_id)
water <- bind_rows(select(water_osm$osm_polygons, osm_id), select(water_osm$osm_multipolygons, osm_id)) |>
  st_transform(crs_se) |>
  bind_rows(river) |>
  st_filter(view_sf)

# Kilometers under water, with the column headers as the flood legend
table_cells <- flood_km |>
  mutate(x = if_else(scenario == "1 in 100 years", 2.35, 3.95), y = 4 - as.integer(mode), label = if_else(km < 0.05, "–", sprintf("%.1f km", km)))
table_heads <- tibble(scenario = names(scenarios), x = c(1.75, 3.35), y = 3.85)

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

p_table <- ggplot() +
  annotation_custom(grid::roundrectGrob(r = unit(10, "pt"), gp = grid::gpar(fill = clr_bg, col = NA))) +
  geom_label(data = table_heads, aes(x, y, label = scenario, fill = scenario), family = f1, size = 4.2, color = clr_text, border.colour = NA, label.r = unit(0.4, "lines"), label.padding = unit(0.35, "lines")) +
  geom_text(data = distinct(table_cells, mode, y), aes(0, y, label = mode), family = f1, size = 4.5, hjust = 0, color = clr_text) +
  geom_text(data = table_cells, aes(x, y, label = label), family = f1, size = 4.5, hjust = 1, color = clr_text) +
  scale_fill_manual(values = clr_flood, guide = "none") +
  coord_cartesian(xlim = c(-0.15, 4.3), ylim = c(0.5, 4.3), expand = FALSE) +
  theme_void()

p_map <- ggplot() +
  geom_sf(data = floods[["1 in 10,000 years"]], fill = clr_flood[["1 in 10,000 years"]], color = NA) +
  geom_sf(data = floods[["1 in 100 years"]], fill = clr_flood[["1 in 100 years"]], color = NA) +
  geom_sf(data = water, fill = clr_accent2, color = NA) +
  geom_sf(data = network, aes(linewidth = I(width)), color = clr_dry) +
  # Light halo keeps the orange apart from the blues beneath
  geom_sf(data = flooded, aes(linewidth = I(width * 4)), color = clr_bg) +
  geom_sf(data = flooded, aes(linewidth = I(width * 2.5)), color = clr_flooded) +
  coord_map_box(view) +
  theme_map() +
  theme(panel.background = element_rect(fill = clr_land, color = NA))

p <- p_map + inset_element(p_table, left = 0.03, bottom = 0.03, right = 0.545, top = 0.2, align_to = "full") + plot_annotation(theme = theme(plot.margin = margin(0)))

save_map(p, here::here("2026/26-water/26-water-fyrisan-flood.png"),
  title = "If Fyrisån floods, \\ Uppsala mostly stays dry",
  subtitle = "Streets, cycleways and railway under more than \\ half a meter of water (orange) in two flood scenarios",
  caption = caption_src("MSB, OpenStreetMap")
)
