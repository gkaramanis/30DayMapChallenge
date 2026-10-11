library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = 8, height = 10, dpi = 320)

# The tram is under construction (service planned 2029). OSM maps it as
# railway=construction + construction=tram, or railway=tram once opened.
tram <- osm_get("tram-construction", bb_city, list(railway = c("construction", "tram")))$osm_lines

tram_lines <- tram[tram$railway %in% "tram" | tag(tram, "construction") %in% "tram", ] |>
  st_transform(crs_se)

tram_stops <- osm_get("tram-stops", bb_city, list(railway = "tram_stop", public_transport = "stop_position"))$osm_points
tram_stops <- tram_stops[tag(tram_stops, "railway") %in% "tram_stop" | tag(tram_stops, "tram") %in% "yes", ] |>
  st_transform(crs_se) |>
  st_filter(st_buffer(st_union(tram_lines), 50))

roads <- osm_roads()
water <- osm_water()
area <- st_buffer(st_union(tram_lines), 800) |> st_bbox()

col_bg <- "#f6f4ef"
col_tram <- "#0f7b6c"

ggplot() +
  geom_sf(data = water, fill = "#d6e6ee", color = NA) +
  geom_sf(data = roads, color = "grey80", linewidth = 0.2) +
  geom_sf(data = tram_lines, color = col_tram, linewidth = 2) +
  geom_sf(data = tram_stops, shape = 21, fill = "white", color = col_tram, size = 2.5, stroke = 1) +
  coord_sf(xlim = area[c(1, 3)], ylim = area[c(2, 4)], expand = FALSE) +
  labs(
    title = "Uppsala spårväg, opening day",
    subtitle = "The tram from Uppsala C to Gottsunda and Bergsbrunna, drawn as if already running",
    caption = caption_src("OpenStreetMap (lines under construction)")
  ) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = col_bg, color = NA),
    plot.title = element_text(family = f2, size = 20, face = "bold", color = col_tram),
    plot.subtitle = element_text(size = 11, margin = margin(5, 0, 10, 0)),
    plot.caption = element_text(size = 8),
    plot.margin = margin(10, 10, 10, 10)
  )

record_polaroid()
