library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = 8, height = 9, dpi = 320)

gtfs <- ul_gtfs()
on <- typical_weekday(gtfs)

# Bus stops in service according to UL
served <- stop_times_on(gtfs, on) |>
  filter(is_bus(route_type)) |>
  distinct(stop_id)

lan <- uppsala_lan()

gtfs_stops <- stops_sf(gtfs) |>
  semi_join(served, by = "stop_id") |>
  st_filter(lan) |>
  st_transform(crs_se)

osm <- osm_get("bus-stops-lan", bb_lan, list(highway = "bus_stop", public_transport = "platform"))
osm_stops <- bind_rows(
  osm$osm_points |> select(osm_id, any_of(c("highway", "public_transport", "bus"))),
  osm$osm_polygons |> select(osm_id, any_of(c("highway", "public_transport", "bus"))) |> st_centroid(),
  osm$osm_lines |> select(osm_id, any_of(c("highway", "public_transport", "bus"))) |> st_centroid()
)
osm_stops <- osm_stops[tag(osm_stops, "highway") %in% "bus_stop" | tag(osm_stops, "bus") %in% "yes", ] |>
  st_transform(crs_se)

# A GTFS stop counts as mapped if an OSM stop lies within 50 m
nearest <- st_nearest_feature(gtfs_stops, osm_stops)
gtfs_stops <- gtfs_stops |>
  mutate(
    dist_m = as.numeric(st_distance(gtfs_stops, osm_stops[nearest, ], by_element = TRUE)),
    in_osm = if_else(dist_m <= 50, "In OSM", "Missing from OSM")
  ) |>
  arrange(in_osm)

share <- mean(gtfs_stops$in_osm == "In OSM")
lan <- st_transform(lan, crs_se)

ggplot() +
  geom_sf(data = lan, fill = "#f2efe9", color = NA) +
  geom_sf(data = gtfs_stops, aes(color = in_osm), size = 0.7) +
  scale_color_manual(values = c("In OSM" = "#7eb77f", "Missing from OSM" = "#d7263d")) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Is every bus stop on the map?",
    subtitle = paste0(scales::percent(share, accuracy = 0.1), " of UL's ", nrow(gtfs_stops), " served bus stops have an OpenStreetMap stop within 50 m"),
    caption = caption_src("UL GTFS via Trafiklab, OpenStreetMap"),
    color = NULL
  ) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "top",
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(family = f2, size = 18, face = "bold"),
    plot.subtitle = element_text(size = 10, margin = margin(5, 0, 10, 0)),
    plot.caption = element_text(size = 8),
    plot.margin = margin(10, 10, 10, 10)
  )

record_polaroid()

# Missing stops, for fixing in OSM
gtfs_stops |>
  filter(in_osm == "Missing from OSM") |>
  st_transform(4326) |>
  write_sf(file.path(dir_data, "bus-stops-missing-in-osm.geojson"), delete_dsn = TRUE)
