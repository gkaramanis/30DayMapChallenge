library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")
dir.create(dir_data, showWarnings = FALSE)

# Evaluate expr once and cache the result as rds in data/
cached <- function(name, expr) {
  path <- file.path(dir_data, paste0(name, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  saveRDS(expr, path)
  expr
}

# City bounding box (xmin, ymin, xmax, ymax) in WGS84
bb_city <- c(17.55, 59.80, 17.76, 59.90)

# Inner city: the station, the river and the shopping streets between them
bb_centre <- c(17.618, 59.8515, 17.660, 59.8655)

roads <- cached("osm-roads-city", {
  osmdata::opq(bb_city, timeout = 600) |>
    osmdata::add_osm_feature("highway", c(
      "motorway", "trunk", "primary", "secondary", "tertiary",
      "unclassified", "residential", "living_street", "service",
      "motorway_link", "trunk_link", "primary_link", "secondary_link"
    )) |>
    osmdata::osmdata_sf()
})$osm_lines |>
  st_transform(crs_se) |>
  mutate(major = highway %in% c("motorway", "trunk", "primary", "secondary"))

# Buildings and river skipped until they're downloaded on a faster connection
# buildings <- cached("osm-buildings-bike-parking-centre", {
#   osmdata::opq(bb_centre, timeout = 300) |>
#     osmdata::add_osm_feature("building") |>
#     osmdata::osmdata_sf()
# })$osm_polygons |>
#   select(osm_id) |>
#   st_transform(crs_se)
#
# river <- cached("osm-water-bike-parking-centre", {
#   osmdata::opq(bb_centre, timeout = 300) |>
#     osmdata::add_osm_feature("natural", "water") |>
#     osmdata::osmdata_sf()
# })[c("osm_polygons", "osm_multipolygons")] |>
#   compact() |>
#   map(\(layer) select(layer, osm_id) |> st_make_valid()) |>
#   bind_rows() |>
#   st_transform(crs_se)

# Uppsala kommun's bike parking areas (polygons, no attributes)
kommun_parking_url <- paste0(
  "https://kartportal.uppsala.se/mapping/rest/services/iKommunkartan/",
  "GOT_Cykelparkeringar/FeatureServer/2/query",
  "?where=1%3D1&outFields=OBJECTID&outSR=4326&f=geojson"
)

bike_parking <- cached("kommun-bicycle-parking", {
  st_read(kommun_parking_url, quiet = TRUE)
})

# One point per parking area, inside its polygon
sites <- bike_parking |>
  st_transform(crs_se) |>
  mutate(area = as.numeric(st_area(geometry))) |>
  st_point_on_surface()

# Sites within 50 m of each other chain into a cluster of at least 5
eps <- 50
sites_clustered <- sites |>
  mutate(cluster = dbscan::dbscan(st_coordinates(sites), eps = eps, minPts = 5)$cluster)

uppsala_c <- st_sfc(st_point(c(17.6460, 59.8586)), crs = 4326) |>
  st_transform(crs_se)
station_cluster <- sites_clustered$cluster[st_nearest_feature(uppsala_c, sites_clustered)]

# Rounded cluster shapes: each site buffered by half of eps, then merged
clusters <- sites_clustered |>
  filter(cluster > 0) |>
  summarise(
    n_sites = n(),
    area = sum(area),
    geometry = st_union(st_buffer(geometry, eps / 2)),
    .by = cluster
  ) |>
  st_as_sf() |>
  mutate(station = cluster == station_cluster)

# Place names from Nominatim reverse lookups of the cluster centres
place_names <- tribble(
  ~name,              ~lon,     ~lat,
  "Uppsala C",        17.64510, 59.85931,
  "Sankt Persgatan",  17.63755, 59.86009,
  "Forumgallerian",   17.64126, 59.85850,
  "Stadsbiblioteket", 17.63569, 59.86100,
  "Östra Ågatan",     17.63924, 59.85736
) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(crs_se)

labels <- clusters[st_nearest_feature(place_names, clusters), ] |>
  mutate(name = place_names$name) |>
  st_point_on_surface()

gg_record(
  here::here("30daymap-temp"),
  width = map_box$width, height = map_box$height, units = "in", dpi = 320
)

p <- ggplot() +
  # geom_sf(data = river, fill = clr_water, color = NA) +
  # geom_sf(data = buildings, fill = clr_grid, color = NA) +
  geom_sf(data = roads, aes(linewidth = major), color = clr_road) +
  geom_sf(
    data = clusters, aes(fill = station),
    color = NA, alpha = 0.35
  ) +
  geom_sf(
    data = filter(sites_clustered, cluster == 0),
    aes(size = area),
    color = clr_text2, alpha = 0.5, stroke = 0
  ) +
  geom_sf(
    data = filter(sites_clustered, cluster > 0),
    aes(size = area),
    color = clr_accent2, stroke = 0
  ) +
  ggrepel::geom_text_repel(
    data = labels,
    aes(label = name, color = station, geometry = geometry),
    stat = "sf_coordinates",
    family = f1, fontface = "bold", size = 5,
    bg.color = clr_bg, bg.r = 0.15,
    min.segment.length = Inf, box.padding = 1, seed = 4
  ) +
  scale_size_area(max_size = 2.5, guide = "none") +
  scale_color_manual(
    values = c(`FALSE` = clr_accent2, `TRUE` = clr_accent),
    guide = "none"
  ) +
  scale_linewidth_manual(values = c(`FALSE` = 0.4, `TRUE` = 0.9), guide = "none") +
  scale_fill_manual(
    values = c(`FALSE` = clr_accent2, `TRUE` = clr_accent),
    guide = "none"
  ) +
  coord_map_box(st_bbox(setNames(bb_centre, c("xmin", "ymin", "xmax", "ymax")), crs = 4326)) +
  theme_map()

save_map(
  p, here::here("2026/04-clusters/04-clusters-bike-parking.png"),
  title = "Where Uppsala\\ parks its bikes",
  subtitle = str_glue(
    "{n_distinct(clusters$cluster)} clusters of the kommun's bike parking\\ ",
    "areas within {eps} m of each other"
  ),
  caption = caption_src("Uppsala kommun, OpenStreetMap")
)
