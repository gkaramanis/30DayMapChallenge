library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = map_width, height = map_height, units = "in", dpi = 320)

gtfs <- ul_gtfs()

# Friday night has the most late service
on <- typical_weekday(gtfs) + 3

night <- stop_times_on(gtfs, on) |>
  filter(dep_s >= 23 * 3600 | dep_s < 5 * 3600)

night_trips <- night |>
  distinct(trip_id) |>
  left_join(gtfs$trips |> select(trip_id, shape_id), by = "trip_id") |>
  count(shape_id, name = "trips")

shapes <- tidytransit::shapes_as_sf(gtfs$shapes) |>
  inner_join(night_trips, by = "shape_id") |>
  st_transform(crs_se)

night_stops <- stops_sf(gtfs) |>
  semi_join(night, by = "stop_id") |>
  st_transform(crs_se)

city <- bb_sf(bb_city)
roads <- osm_roads() |> st_filter(city)

# Glow from stacked lines, wide and faint to narrow and bright
glow <- map(c(4, 2.5, 1.2, 0.4), function(w) {
  geom_sf(data = shapes, color = clr_accent, linewidth = w, alpha = 0.6 / w)
})

ggplot() +
  geom_sf(data = roads, color = colorspace::darken(clr_text2, 0.55), linewidth = 0.2) +
  glow +
  geom_sf(data = night_stops, color = clr_bg, size = 0.5) +
  coord_sf(xlim = st_bbox(city)[c(1, 3)], ylim = st_bbox(city)[c(2, 4)], expand = FALSE) +
  labs(
    title = "Uppsala after 23:00",
    subtitle = paste0("Bus routes and stops served\nbetween 23:00 and 05:00,\nnight of ", str_squish(format(on, "%e %B %Y"))),
    caption = caption_src("UL GTFS via Trafiklab, OpenStreetMap")
  ) +
  theme_map(bg = clr_text, text = clr_bg) +
  theme(plot.title = element_text(color = clr_accent))

record_polaroid()
