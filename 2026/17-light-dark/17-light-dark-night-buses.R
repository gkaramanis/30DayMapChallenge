library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = 9, height = 9, dpi = 320)

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

col_bg <- "#05060a"
col_glow <- "#ffd166"

# Glow from stacked lines, wide and faint to narrow and bright
glow <- map(c(4, 2.5, 1.2, 0.4), function(w) {
  geom_sf(data = shapes, color = col_glow, linewidth = w, alpha = 0.6 / w)
})

ggplot() +
  geom_sf(data = roads, color = "#1a1d26", linewidth = 0.15) +
  glow +
  geom_sf(data = night_stops, color = "white", size = 0.4) +
  coord_sf(xlim = st_bbox(city)[c(1, 3)], ylim = st_bbox(city)[c(2, 4)], expand = FALSE) +
  labs(
    title = "Uppsala after 23:00",
    subtitle = paste0("Bus routes and stops served between 23:00 and 05:00, night of ", format(on, "%e %B %Y")),
    caption = caption_src("UL GTFS via Trafiklab, OpenStreetMap")
  ) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = col_bg, color = NA),
    plot.title = element_text(family = f2, size = 20, face = "bold", color = col_glow),
    plot.subtitle = element_text(size = 10, color = "grey75", margin = margin(5, 0, 10, 0)),
    plot.caption = element_text(size = 8, color = "grey60"),
    plot.margin = margin(10, 10, 10, 10)
  )

record_polaroid()
