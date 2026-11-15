library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")

# osmium extract -b 16.7,59.3,18.95,60.75 sweden-latest.osm.pbf (Geofabrik)
pbf <- file.path(dir_data, "r5/uppsala.osm.pbf")

centre <- st_point(c(17.646, 59.858)) |> st_sfc(crs = 4326) |> st_transform(crs_se) |> st_coordinates()
view <- map_box_view(centre, 11000)
view_wgs <- st_as_text(st_as_sfc(st_bbox(st_transform(st_as_sfc(view), 4326))))

# Trafikverket's GCM crossings from NVDB; Passa_85 = 2 is an underpass (planskild passage underfart)
gcm_url <- "https://vektor-pr.trafikverket.se/gis/rest/services/GCM/Trafiksäkerhetsklass_Passager_GCM/FeatureServer/0/query"
underpasses_file <- file.path(dir_data, "trv-gcm-underpasses-uppsala.geojson")
if (!file.exists(underpasses_file)) {
  req <- httr2::request(gcm_url) |> httr2::req_url_query(where = "Passa_85 = 2", geometry = paste(view, collapse = ","), geometryType = "esriGeometryEnvelope", inSR = crs_se, outSR = crs_se, outFields = "*", f = "geojson")
  httr2::req_perform(req, path = underpasses_file)
}
underpasses <- st_read(underpasses_file, quiet = TRUE) |> st_set_crs(crs_se) |> st_crop(view)

read_osm <- function(layer, where) {
  sql <- paste("SELECT * FROM", layer, "WHERE", where)
  st_read(pbf, query = sql, wkt_filter = view_wgs, quiet = TRUE) |> st_transform(crs_se) |> st_crop(view)
}

roads <- read_osm("lines", "highway IN ('motorway', 'trunk', 'primary', 'secondary', 'tertiary', 'unclassified', 'residential', 'living_street', 'motorway_link', 'trunk_link', 'primary_link', 'secondary_link', 'tertiary_link')")
rail <- read_osm("lines", "railway = 'rail'")
water <- bind_rows(read_osm("multipolygons", "natural = 'water'") |> select(osm_id), read_osm("lines", "waterway = 'river'") |> st_buffer(8) |> select(osm_id))

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

p <- ggplot() +
  geom_sf(data = water, fill = clr_water, color = NA) +
  geom_sf(data = roads, color = clr_text2, linewidth = 0.15, linetype = "22") +
  geom_sf(data = rail, color = clr_text2, linewidth = 0.3, linetype = "22") +
  geom_sf(data = underpasses, shape = 21, size = 3.2, stroke = 0.8, color = clr_accent2, fill = NA) +
  geom_sf(data = underpasses, size = 0.8, color = clr_accent2) +
  coord_sf(xlim = view[c("xmin", "xmax")], ylim = view[c("ymin", "ymax")], crs = crs_se, expand = FALSE) +
  theme_map()

save_map(p, here::here("2026/15-inside-out/15-inside-out-underpasses.png"), title = paste(nrow(underpasses), "ways under the traffic"), subtitle = "Cycle and foot underpasses in Uppsala. Streets are \\ dashed, the way maps usually draw tunnels", caption = caption_src("Trafikverket, OpenStreetMap"))
