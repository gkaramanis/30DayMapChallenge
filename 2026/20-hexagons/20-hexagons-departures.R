library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = map_width, height = map_height, units = "in", dpi = 320)

gtfs <- ul_gtfs()
on <- typical_weekday(gtfs)

# Service day 05-24 is 19 hours
deps <- stop_times_on(gtfs, on) |>
  filter(between(dep_s, 5 * 3600, 24 * 3600 - 1)) |>
  count(stop_id, name = "departures")

stops <- stops_sf(gtfs) |>
  inner_join(deps, by = "stop_id")

stops$h3 <- h3jsr::point_to_cell(stops, res = 8)

hexes <- stops |>
  st_drop_geometry() |>
  group_by(h3) |>
  summarise(per_hour = sum(departures) / 19)

hex_sf <- h3jsr::cell_to_polygon(hexes$h3, simple = FALSE) |>
  rename(h3 = h3_address) |>
  left_join(hexes, by = "h3") |>
  st_transform(crs_se)

kommun <- uppsala_kommun() |> st_transform(crs_se)
hex_sf <- st_filter(hex_sf, kommun)

ggplot() +
  geom_sf(data = kommun, fill = clr_land, color = NA) +
  geom_sf(data = hex_sf, aes(fill = per_hour), color = clr_bg, linewidth = 0.1) +
  scale_fill_stepsn(colors = c(clr_accent, clr_accent2), trans = "log10", breaks = c(1, 3, 10, 30, 100)) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Buses per hour, by bestagon",
    subtitle = paste0("Average weekday departures per hour (05-24)\nin H3 resolution 8 cells, ", str_squish(format(on, "%e %B %Y"))),
    caption = caption_src("UL GTFS via Trafiklab, Uber H3"),
    fill = "Departures per hour"
  ) +
  guides(fill = guide_coloursteps(theme = theme(legend.key.width = unit(14, "lines"), legend.key.height = unit(0.8, "lines")))) +
  theme_map()

record_polaroid()
