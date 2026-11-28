library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")

bb_city <- c(xmin = 17.55, ymin = 59.80, xmax = 17.76, ymax = 59.90)

# Download url to data/file once and return the path
download_once <- function(url, file) {
  path <- file.path(dir_data, file)
  if (!file.exists(path)) download.file(url, path, mode = "wb", quiet = TRUE)
  path
}

# One NYKO 2023 layer from Uppsala kommun's map service, in SWEREF 99 TM
nyko_layer <- function(layer, file) {
  download_once(str_glue("https://kartportal.uppsala.se/mapping/rest/services/iExternaKartan/BefolkningPolitik/MapServer/{layer}/query?where=1%3D1&outFields=*&outSR=3006&f=geojson"), file) |>
    read_sf() |>
    st_set_crs(crs_se)
}

# Trygghetsundersökning 2023, run by SCB for Uppsala kommun. Every area report repeats the indicator table for all NYKO4 areas.
survey_pdf <- download_once("https://www.uppsala.se/contentassets/b9ba367c60034376ac98b3fd90a17991/almtuna.pdf", "trygghetsundersokning-2023-almtuna.pdf")
indicators <- c("worry", "mobility", "problems", "victims", "cohesion", "trust")

survey <- pdftools::pdf_text(survey_pdf) |>
  str_split("\n") |>
  unlist() |>
  str_match("^(\\S.*?)\\s{2,}(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s*$") |>
  as_tibble(.name_repair = ~ c("line", "area", indicators)) |>
  filter(!is.na(area), area != "Totalsumma") |>
  distinct(area, .keep_all = TRUE) |>
  mutate(across(all_of(indicators), as.integer)) |>
  select(-line)

# NYKO4 2023 matches the survey's areas; only one name is spelled differently
areas <- nyko_layer(2, "nyko4-2023.geojson") |>
  left_join(mutate(survey, N4Namn23 = str_replace(area, "^Gränby alle$", "Gränby allé")), by = join_by(N4Namn23)) |>
  mutate(unaffected = 100L - mobility)

# District names for orientation, spread over the city and chosen without regard to the survey values
districts <- c("Centrum", "Luthagen", "Svartbäcken", "Gränby", "Årsta", "Stenhagen", "Flogsta Ekeby", "Eriksberg", "Gottsunda", "Sävja")

labels <- nyko_layer(3, "nyko3-2023.geojson") |>
  filter(N3Namn23 %in% districts) |>
  st_point_on_surface() |>
  mutate(label = str_remove(N3Namn23, " Ekeby"))

view <- coord_map_box(st_bbox(bb_city, crs = 4326))
view_sf <- st_as_sfc(st_bbox(c(xmin = view$limits$x[[1]], xmax = view$limits$x[[2]], ymin = view$limits$y[[1]], ymax = view$limits$y[[2]]), crs = crs_se))

roads <- readRDS(file.path(dir_data, "osm-roads-city.rds"))$osm_lines |>
  st_transform(crs_se) |>
  mutate(width = if_else(highway %in% c("motorway", "trunk", "primary", "secondary"), 0.35, 0.12))

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

clr_unaffected <- c("#F6E7C8", "#EFC77A", clr_accent, "#B5541A", "#8A3A16")

# Legend background with rounded corners; element_rect has no radius
element_rect_round <- S7::new_class("element_rect_round", parent = element_rect, properties = list(r = S7::class_any))
S7::method(element_grob, element_rect_round) <- function(element, x = 0.5, y = 0.5, width = 1, height = 1, ...) grid::roundrectGrob(x, y, width, height, r = element@r, gp = grid::gpar(fill = element@fill, col = NA))

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

p <- ggplot() +
  geom_sf(data = filter(areas, !is.na(unaffected)), aes(fill = unaffected), color = clr_bg, linewidth = 0.3) +
  geom_sf(data = water, fill = clr_water, color = NA) +
  geom_sf(data = roads, aes(linewidth = I(width)), color = clr_text2, alpha = 0.35) +
  geom_sf(data = filter(areas, is.na(unaffected)), fill = NA, color = clr_grid, linewidth = 0.3) +
  shadowtext::geom_shadowtext(data = labels, aes(geometry = geometry, label = label), stat = "sf_coordinates", family = f1, size = 4.2, color = clr_text, bg.color = clr_bg, bg.r = 0.15) +
  scale_fill_stepsn(colors = clr_unaffected, breaks = c(80, 85, 90, 95), limits = c(75, 100), labels = \(x) str_c(x, "%"), name = "Everyone else") +
  guides(fill = guide_colorsteps(barwidth = unit(14, "lines"), barheight = unit(0.6, "lines"), direction = "horizontal", even.steps = TRUE)) +
  view +
  theme_map() +
  theme(panel.background = element_rect(fill = clr_land, color = NA), legend.position.inside = c(0.98, 0.98), legend.justification.inside = c(1, 1), legend.background = element_rect_round(fill = alpha(clr_bg, 0.9), r = unit(10, "pt")), legend.margin = margin(8, 12, 8, 12))

save_map(p, here::here("2026/28-feeling/28-feeling-mobility-reverse.png"),
  title = "Most Uppsala residents \\ move freely in every area",
  subtitle = "Only a few often stay home or avoid certain streets \\ because they feel unsafe, 2023",
  caption = caption_src("Uppsala kommun, SCB, OpenStreetMap")
)
