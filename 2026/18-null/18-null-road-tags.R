library(tidyverse)
library(sf)

source(here::here("2026/R/helpers.R"))

bb_city <- c(xmin = 17.55, ymin = 59.80, xmax = 17.76, ymax = 59.90)

# No service roads: parking aisles and driveways rarely get a speed limit
road_types <- c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential", "living_street", "motorway_link", "trunk_link", "primary_link", "secondary_link")

# Cached OSM query, shared with other days
roads_file <- file.path(dir_2026, "data", "osm-roads-city.rds")
if (!file.exists(roads_file)) osmdata::opq(unname(bb_city), timeout = 600) |> osmdata::add_osm_feature("highway", road_types) |> osmdata::osmdata_sf() |> saveRDS(roads_file)

city <- st_bbox(bb_city, crs = 4326) |> st_as_sfc() |> st_transform(crs_se)

roads <- readRDS(roads_file)$osm_lines |> filter(highway %in% road_types) |> st_transform(crs_se) |> st_filter(city) |> mutate(is_null = is.na(maxspeed), len = as.numeric(st_length(geometry))) |> arrange(is_null)

share_null <- weighted.mean(roads$is_null, roads$len)

# As tall as the city, cropped at the sides to the map box shape
city_bb <- st_bbox(city)
view <- map_box_view(c(mean(city_bb[c("xmin", "xmax")]), mean(city_bb[c("ymin", "ymax")])), (city_bb[["ymax"]] - city_bb[["ymin"]]) * map_box$width / map_box$height)

p <- ggplot(roads) +
  geom_sf(aes(color = is_null, linewidth = is_null)) +
  scale_color_manual(values = c("TRUE" = clr_accent, "FALSE" = colorspace::lighten(clr_text2, 0.55))) +
  scale_linewidth_manual(values = c("TRUE" = 0.6, "FALSE" = 0.2)) +
  coord_sf(xlim = view[c("xmin", "xmax")], ylim = view[c("ymin", "ymax")], crs = crs_se, expand = FALSE) +
  theme_map() +
  theme(legend.position = "none")

save_map(p, here::here("2026/18-null/18-null-road-tags.png"),
  title = "A quarter of Uppsala's roads\\ have a NULL speed limit",
  subtitle = paste0("Amber roads have no speed limit tagged\\ in OpenStreetMap, ", scales::percent(share_null, accuracy = 1), " of the road length"),
  caption = caption_src("OpenStreetMap")
)
