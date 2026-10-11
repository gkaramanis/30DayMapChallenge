library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = map_width, height = map_height, units = "in", dpi = 320)

# The tram is under construction (service planned 2029). OSM maps it as
# railway=construction + construction:railway=tram, or railway=tram once opened.
tram <- osm_get("tram-construction", bb_city, list(railway = c("construction", "tram")))

tram_lines <- tram$osm_lines[tram$osm_lines$railway %in% "tram" | tag(tram$osm_lines, "construction:railway") %in% "tram", ] |>
  st_transform(crs_se)

tram_stops <- tram$osm_points[tag(tram$osm_points, "station") %in% "tram" | tag(tram$osm_points, "railway") %in% "tram_stop", ] |>
  st_transform(crs_se)

roads <- osm_roads()
water <- osm_water()
area <- st_buffer(st_union(tram_lines), 800) |> st_bbox()
centre <- c((area[1] + area[3]) / 2, (area[2] + area[4]) / 2)
half_height <- max((area[4] - area[2]) / 2, (area[3] - area[1]) / 2 / 1.1)
half_width <- half_height * 1.1

key_stops <- tram_stops |>
  filter(name %in% c("Uppsala C", "Akademiska sjukhuset", "Gottsunda torg", "Ultuna", "Bergsbrunna station"))

ggplot() +
  geom_sf(data = water, fill = clr_water, color = NA) +
  geom_sf(data = roads, color = clr_road, linewidth = 0.25) +
  geom_sf(data = tram_lines, color = clr_accent2, linewidth = 1.8) +
  geom_sf(data = tram_stops, shape = 21, fill = clr_bg, color = clr_accent2, size = 3, stroke = 1) +
  ggrepel::geom_text_repel(data = filter(key_stops, name == "Bergsbrunna station"), aes(label = name, geometry = geometry), stat = "sf_coordinates", family = f1, fontface = "bold", size = 5.5, color = clr_text, seed = 1, nudge_x = -900, nudge_y = -600, min.segment.length = 0.2, segment.color = clr_text2) +
  ggrepel::geom_text_repel(data = filter(key_stops, name != "Bergsbrunna station"), aes(label = name, geometry = geometry), stat = "sf_coordinates", family = f1, fontface = "bold", size = 5.5, color = clr_text, seed = 1, nudge_x = 600, min.segment.length = 0.2, segment.color = clr_text2) +
  coord_sf(xlim = centre[1] + c(-1, 1) * half_width, ylim = centre[2] + c(-1, 1) * half_height, expand = FALSE) +
  labs(
    title = "Uppsala spårväg, opening day",
    subtitle = "The tram from Uppsala C to Gottsunda\nand Bergsbrunna, drawn as if already running",
    caption = caption_src("OpenStreetMap (lines under construction)")
  ) +
  theme_map()

record_polaroid()
