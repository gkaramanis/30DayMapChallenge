library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = 8, height = 9, dpi = 320)

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

col_bg <- "#f7f5f0"
col_txt <- "#22223b"

ggplot() +
  geom_sf(data = lan, fill = "white", color = "grey75", linewidth = 0.2) +
  geom_sf(data = stops, aes(size = departures, color = departures), alpha = 0.7) +
  scale_size_area(max_size = 9, breaks = c(10, 100, 1000)) +
  scale_color_gradientn(colors = MetBrewer::met.brewer("Hokusai2"), trans = "log10", guide = "none") +
  coord_sf(expand = FALSE) +
  labs(
    title = "Every stop in Uppsala län",
    subtitle = paste0("UL bus stops and stations sized by departures on ", format(on, "%A %e %B %Y")),
    caption = caption_src("UL GTFS via Trafiklab"),
    size = "Departures"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.12, 0.15),
    plot.background = element_rect(fill = col_bg, color = NA),
    plot.title = element_text(family = f2, size = 18, face = "bold", color = col_txt),
    plot.subtitle = element_text(size = 11, color = col_txt, margin = margin(5, 0, 10, 0)),
    plot.caption = element_text(size = 8, color = col_txt),
    plot.margin = margin(10, 10, 10, 10)
  )

record_polaroid()
