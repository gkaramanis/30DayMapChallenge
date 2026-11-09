library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

dir_data <- file.path(dir_2026, "data")
dir.create(dir_data, showWarnings = FALSE, recursive = TRUE)

bb_lan <- c(16.70, 59.30, 18.95, 60.75)

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

lan <- cached("boundary-uppsala_lan", {
  osmdata::opq(bb_lan, timeout = 300) |>
    osmdata::add_osm_feature("boundary", "administrative") |>
    osmdata::add_osm_feature("admin_level", "4") |>
    osmdata::add_osm_feature("name", "Uppsala län") |>
    osmdata::osmdata_sf() |>
    pluck("osm_multipolygons") |>
    select(name) |>
    st_make_valid()
}) |>
  st_transform(crs_se)

bb <- st_bbox(lan)

tatorter <- scb_wfs("scb-tatorter-lan-2023", "Tatorter_2023", "lan='03'")

# Sävja and Alsike would crowd Uppsala's label
towns <- filter(tatorter, bef >= 4000, !tatort %in% c("Sävja", "Alsike")) |> st_point_on_surface()

# Key from https://developer.trafiklab.se, stored in ~/.Renviron
ul_zip <- file.path(dir_data, "ul.zip")
if (!file.exists(ul_zip)) {
  key <- Sys.getenv("TRAFIKLAB_GTFS_REGIONAL_KEY")
  download.file(paste0("https://opendata.samtrafiken.se/gtfs/ul/ul.zip?key=", key), ul_zip, mode = "wb", headers = c("Accept-Encoding" = "gzip"))
}
gtfs <- tidytransit::read_gtfs(ul_zip) |> tidytransit::filter_feed_by_date(as.Date("2026-11-05"))

kinds <- c("School bus", "Other bus lines")

# UL buses on a school day, typed by route_desc; no SL, ferry or hospital buses
ul_agency <- gtfs$agency |> filter(agency_name == "UL") |> pull(agency_id)

routes <- gtfs$routes |>
  filter(agency_id %in% ul_agency, route_type == 700, route_desc != "Sjukresebuss", route_id %in% gtfs$trips$route_id) |>
  mutate(kind = factor(if_else(route_desc == "Skolbuss", kinds[1], kinds[2]), levels = kinds)) |>
  select(route_id, kind)

# Map-box view around the county; lines fade outside it
lan_w <- bb[["xmax"]] - bb[["xmin"]]
lan_h <- bb[["ymax"]] - bb[["ymin"]]
view <- map_box_view(c(bb[["xmin"]] + lan_w / 2, bb[["ymin"]] + lan_h / 2), max(lan_w, lan_h * map_box$width / map_box$height))

# School buses first, so amber shows only where no other line runs
lines <- tidytransit::shapes_as_sf(gtfs$shapes) |>
  inner_join(distinct(gtfs$trips, shape_id, route_id), by = join_by(shape_id)) |>
  inner_join(routes, by = join_by(route_id)) |>
  arrange(kind) |>
  st_transform(crs_se) |>
  st_intersection(st_as_sfc(view))

lines_in <- st_intersection(lines, st_geometry(lan))
lines_out <- st_difference(lines, st_union(lan))

# Whether only school buses call at each stop
stop_kinds <- gtfs$stop_times |>
  distinct(trip_id, stop_id) |>
  inner_join(distinct(gtfs$trips, trip_id, route_id), by = join_by(trip_id)) |>
  inner_join(routes, by = join_by(route_id)) |>
  summarise(school_only = all(kind == kinds[1]), .by = stop_id)

stops <- gtfs$stops |>
  filter(!is.na(stop_lat)) |>
  inner_join(stop_kinds, by = join_by(stop_id)) |>
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) |>
  st_transform(crs_se)

# Rural population; counted where every stop within 1 km is school-bus only
pop <- scb_wfs("scb-befolkning-1km-2025-lan", "befolkning_1km_2025", paste0("BBOX(sp_geometry,", paste(bb[c("xmin", "ymin", "xmax", "ymax")], collapse = ","), ",'EPSG:3006')")) |>
  st_point_on_surface() |>
  st_filter(lan) |>
  st_filter(st_union(tatorter), .predicate = st_disjoint)

near <- st_is_within_distance(pop, stops, dist = 1000)

school_only_people <- pop |>
  st_drop_geometry() |>
  filter(map_lgl(near, \(i) length(i) > 0 && all(stops$school_only[i]))) |>
  summarise(people = sum(beftotalt)) |>
  pull(people)

clr_kinds <- set_names(c(clr_accent, clr_accent2), kinds)

p <- ggplot() +
  geom_sf(data = lan, fill = clr_land, color = NA) +
  geom_sf(data = lines_out, aes(color = kind), linewidth = 0.45, alpha = 0.1, show.legend = FALSE) +
  geom_sf(data = lines_in, aes(color = kind), linewidth = 0.45, key_glyph = "path") +
  geom_sf(data = lan, fill = NA, color = clr_text2, linewidth = 0.2) +
  ggrepel::geom_text_repel(data = towns, aes(label = tatort, geometry = geometry), stat = "sf_coordinates", family = f1, size = 3.6, color = clr_text, bg.color = clr_bg, bg.r = 0.12, seed = 1, min.segment.length = Inf) +
  scale_color_manual(values = clr_kinds, name = NULL, guide = guide_legend(override.aes = list(linewidth = 2))) +
  coord_sf(xlim = view[c("xmin", "xmax")], ylim = view[c("ymin", "ymax")], crs = crs_se, expand = FALSE) +
  theme_map() +
  theme(legend.position.inside = c(0.99, 0.02), legend.justification.inside = c(1, 0), legend.text = element_text(size = 12), legend.key.width = unit(8, "mm"))

save_map(p, here::here("2026/09-urban-rural/09-urban-rural-school-buses.png"),
  title = "When school's out,\\ so is the bus",
  subtitle = str_glue("For about {scales::comma(school_only_people, 1000)} people in rural Uppsala County,\\ the only bus within 1 km is a school bus") |> as.character(),
  caption = caption_src("UL GTFS via Trafiklab, SCB, OpenStreetMap")
)
