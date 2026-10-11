library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = map_width, height = map_height, units = "in", dpi = 320)

gtfs <- ul_gtfs()
on <- typical_weekday(gtfs)

deps <- stop_times_on(gtfs, on) |>
  count(stop_id, name = "departures")

# Group platforms into their parent station where there is one
stops <- stops_sf(gtfs) |>
  left_join(deps, by = "stop_id") |>
  mutate(group_id = coalesce(na_if(parent_station, ""), stop_id)) |>
  group_by(group_id) |>
  summarise(departures = sum(departures, na.rm = TRUE)) |>
  filter(departures > 0) |>
  st_centroid() |>
  st_transform(crs_se) |>
  arrange(departures)

lan <- uppsala_lan() |> st_transform(crs_se)

ggplot() +
  geom_sf(data = lan, fill = clr_land, color = clr_grid, linewidth = 0.4) +
  geom_sf(data = stops, aes(size = departures), color = clr_accent2, alpha = 0.55, stroke = 0) +
  geom_sf(data = slice_max(stops, departures, n = 1), aes(size = departures), color = clr_accent, stroke = 0) +
  geom_sf_text(data = slice_max(stops, departures, n = 1), aes(label = "Uppsala C"), family = f1, fontface = "bold", size = 6, color = clr_accent, hjust = 0, nudge_x = 9000) +
  scale_size_area(max_size = 14, breaks = c(10, 100, 1000), labels = scales::label_number(big.mark = " ")) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Every stop in Uppsala län",
    subtitle = paste0("UL stops sized by departures,\n", str_squish(format(on, "%A %e %B %Y"))),
    caption = caption_src("UL GTFS via Trafiklab"),
    size = "Departures per day"
  ) +
  guides(size = guide_legend(override.aes = list(color = clr_accent2, alpha = 0.8))) +
  theme_map()

record_polaroid()
