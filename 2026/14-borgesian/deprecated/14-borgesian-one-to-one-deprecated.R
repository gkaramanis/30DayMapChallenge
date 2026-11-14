library(tidyverse)
library(sf)
library(camcorder)
library(ggmapinset)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")
dir.create(dir_data, showWarnings = FALSE)

bb_centre <- c(17.620, 59.850, 17.660, 59.866)

# Cache expr as rds in data/
cached <- function(name, expr) {
  path <- file.path(dir_data, paste0(name, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  saveRDS(expr, path)
  expr
}

# features: named list of key = values, each key queried separately; NA means any value
osm_get <- function(name, bb, features) {
  cached(paste0("osm-", name), {
    res <- imap(features, \(value, key) {
      query <- osmdata::opq(bb, timeout = 600)
      osmdata::osmdata_sf(if (all(is.na(value))) osmdata::add_osm_feature(query, key) else osmdata::add_osm_feature(query, key, value))
    })
    do.call(c, unname(res))
  })
}

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

# The bike parking by Uppsala C, 2 500 stands, way 210282837
site <- osm_get("bike-parking-centre", bb_centre, list(amenity = "bicycle_parking"))$osm_polygons |> filter(osm_id == "210282837") |> st_transform(crs_se) |> st_union()

centre <- st_coordinates(st_centroid(site))[1, ]
site_bb <- st_bbox(site)
view <- map_box_view(centre, max(site_bb[["xmax"]] - site_bb[["xmin"]], (site_bb[["ymax"]] - site_bb[["ymin"]]) * map_box$width / map_box$height) * 1.1)

# Layers are cropped 20 m beyond the view so thick lines run cleanly to the edge
pad <- st_bbox(st_buffer(st_as_sfc(view), 20))

buildings <- osm_get("buildings-centre", bb_centre, list(building = NA))$osm_polygons |> st_transform(crs_se) |> st_crop(pad)
roads <- osm_get("roads-centre", bb_centre, list(highway = c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential", "living_street", "service", "motorway_link", "trunk_link", "primary_link", "secondary_link")))$osm_lines |> st_transform(crs_se) |> st_crop(pad)
paths <- osm_get("paths-centre", bb_centre, list(highway = c("pedestrian", "footway", "cycleway", "path", "steps")))$osm_lines |> st_transform(crs_se) |> st_crop(pad)

# A4 sheets at 1:1, keeping those whose centre is on the site
sheets <- st_sf(geometry = st_make_grid(site, cellsize = c(0.210, 0.297))) |>
  st_filter(site, .predicate = \(x, y) st_intersects(st_centroid(x), y))

# 500 sheets to a pack
packs <- ceiling(nrow(sheets) / 500)

# Inset: a 5 × 5 m square 1.5 m inside the site's western corner, magnified
# into the open top right of the map at inset_size of the map's width
inset_m <- 5
inset_size <- 0.25
site_xy <- st_coordinates(site)
west <- site_xy[which.min(site_xy[, 1]), 1:2]
inset_centre <- west + (centre - west) / sqrt(sum((centre - west)^2)) * 1.5

view_w <- view[["xmax"]] - view[["xmin"]]
to_side <- view_w * inset_size
margin_m <- view_w * 0.02
to_centre <- c(view[["xmax"]] - margin_m - to_side / 2, view[["ymax"]] - margin_m - to_side / 2)

inset <- configure_inset(shape_rectangle(centre = st_sfc(st_point(inset_centre), crs = crs_se), hwidth = inset_m / 2), scale = to_side / inset_m, translation = to_centre - inset_centre, units = "m", crs_working = crs_se)

frame_aes <- list(colour = clr_accent, linewidth = 0.8)

p <- ggplot() +
  # Main map, then the inset frame filled to hide what lies under it, then
  # the magnified copies; no building or road falls in the inset
  geom_sf(data = buildings, fill = clr_land, color = clr_text2, linewidth = 0.15) +
  geom_sf(data = roads, color = clr_text2, linewidth = 10) +
  geom_sf(data = paths, color = clr_grid, linewidth = 3) +
  geom_sf(data = sheets, fill = "white", color = clr_text2, linewidth = 0.07, alpha = 0.5) +
  geom_inset_frame(source.aes = c(frame_aes, fill = NA), target.aes = c(frame_aes, fill = clr_bg), lines.aes = list(linetype = 0)) +
  # Paths as 1 m wide polygons in the inset, about the 3 mm line's width on
  # the main map; polygons are cut cleanly at the frame, thick strokes are not
  geom_sf_inset(data = st_buffer(paths, 0.5), fill = clr_grid, color = NA, map_base = "none") +
  geom_sf_inset(data = sheets, fill = "white", color = clr_text2, linewidth = 0.07, alpha = 0.5, map_base = "none") +
  # Frame outline again, over the magnified copies
  geom_inset_frame(source.aes = c(frame_aes, fill = NA), target.aes = c(frame_aes, fill = NA), lines.aes = list(linetype = 0)) +
  annotate("label", x = to_centre[[1]] - to_side / 2 + margin_m * 2, y = to_centre[[2]] + to_side / 2 - margin_m / 2, label = paste(inset_m, "m across"), hjust = 0, vjust = 1, family = f1, size = 3.4, color = clr_text, fill = clr_bg, border.colour = NA) +
  coord_sf_inset(inset, xlim = view[c("xmin", "xmax")], ylim = view[c("ymin", "ymax")], crs = crs_se, expand = FALSE) +
  theme_map()

save_map(p, here::here("2026/14-borgesian/14-borgesian-one-to-one.png"),
  title = "A map as big \\ as the place",
  subtitle = paste0("Uppsala C's bike parking drawn at 1:1 \\ would take ", packs, " packs of printer paper"),
  caption = caption_src("OpenStreetMap")
)
