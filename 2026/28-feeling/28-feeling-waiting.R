library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = 9, height = 9, dpi = 320)

gtfs <- ul_gtfs()
on <- typical_weekday(gtfs)

# Expected wait for someone who turns up at random is half the mean headway, 07-19
waits <- stop_times_on(gtfs, on) |>
  filter(between(dep_s, 7 * 3600, 19 * 3600)) |>
  arrange(stop_id, dep_s) |>
  group_by(stop_id) |>
  summarise(
    departures = n(),
    headway_min = if (n() > 1) mean(diff(dep_s)) / 60 else NA_real_
  ) |>
  filter(departures >= 2) |>
  mutate(wait_min = headway_min / 2)

city <- bb_sf(bb_city)

stops <- stops_sf(gtfs) |>
  inner_join(waits, by = "stop_id") |>
  st_transform(crs_se) |>
  st_filter(city) |>
  arrange(desc(wait_min))

roads <- osm_roads() |> st_filter(city)

feelings <- c("Relaxed" = "#2a9d8f", "Fine" = "#8ab17d", "Restless" = "#e9c46a", "Impatient" = "#f4a261", "Fuming" = "#e76f51")

stops <- stops |>
  mutate(feeling = cut(wait_min, c(0, 3, 7.5, 15, 30, Inf), labels = names(feelings)))

ggplot() +
  geom_sf(data = roads, color = "grey85", linewidth = 0.15) +
  geom_sf(data = stops, aes(color = feeling), size = 1.6) +
  scale_color_manual(values = feelings, labels = paste0(names(feelings), " (", c("under 3", "3-7.5", "7.5-15", "15-30", "over 30"), " min)")) +
  coord_sf(xlim = st_bbox(city)[c(1, 3)], ylim = st_bbox(city)[c(2, 4)], expand = FALSE) +
  labs(
    title = "How waiting feels",
    subtitle = "Average wait at each bus stop for someone arriving at random, 07-19 on a weekday",
    caption = caption_src("UL GTFS via Trafiklab, OpenStreetMap"),
    color = NULL
  ) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "right",
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(family = f2, size = 20, face = "bold"),
    plot.subtitle = element_text(size = 10, margin = margin(5, 0, 10, 0)),
    plot.caption = element_text(size = 8),
    plot.margin = margin(10, 10, 10, 10)
  )

record_polaroid()
