library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = map_width, height = map_height, units = "in", dpi = 320)

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

feelings <- c("Relaxed" = clr_accent2, "Fine" = colorspace::lighten(clr_accent2, 0.45), "Restless" = clr_text2, "Impatient" = colorspace::lighten(clr_accent, 0.2), "Fuming" = clr_accent)

stops <- stops |>
  mutate(feeling = cut(wait_min, c(0, 3, 7.5, 15, 30, Inf), labels = names(feelings)))

ggplot() +
  geom_sf(data = roads, color = clr_road, linewidth = 0.2) +
  geom_sf(data = stops, aes(color = feeling), size = 1.8) +
  scale_color_manual(values = feelings, labels = paste0(names(feelings), " (", c("under 3", "3-7.5", "7.5-15", "15-30", "over 30"), " min)")) +
  coord_sf(xlim = mean(st_bbox(city)[c(1, 3)]) + c(-1, 1) * 0.45 * diff(st_bbox(city)[c(2, 4)]), ylim = st_bbox(city)[c(2, 4)], expand = FALSE) +
  labs(
    title = "How waiting feels",
    subtitle = "Average wait at each bus stop for someone\narriving at random, 07-19 on a weekday",
    caption = caption_src("UL GTFS via Trafiklab, OpenStreetMap"),
    color = NULL
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 4))) +
  theme_map() +
  theme(panel.background = element_rect(fill = clr_land, color = NA))

record_polaroid()
