library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")

gtfs <- tidytransit::read_gtfs(file.path(dir_data, "ul.zip"))
feed <- tidytransit::filter_feed_by_date(gtfs, as.Date("2026-11-04"))

window <- c(7, 18) * 3600
window_hours <- diff(window) / 3600

# Boardable departures per stop point, one per route and minute
departures <- feed$stop_times |>
  filter(is.na(pickup_type) | pickup_type != 1) |>
  mutate(t = as.numeric(departure_time)) |>
  filter(t >= window[1], t < window[2]) |>
  left_join(
    select(feed$trips, trip_id, route_id),
    by = join_by(trip_id), relationship = "many-to-one"
  ) |>
  distinct(stop_id, t, route_id)

# Extra wait: mean wait for someone arriving at random, minus the wait if the
# same departures were evenly spaced. sum(h^2) / (2 * sum(h)) - mean(h) / 2
bunching <- departures |>
  arrange(stop_id, t) |>
  mutate(h = t - lag(t), .by = stop_id) |>
  filter(!is.na(h)) |>
  summarise(
    n_dep = n() + 1,
    n_routes = n_distinct(route_id),
    extra_wait = (sum(h^2) / (2 * sum(h)) - mean(h) / 2) / 60,
    .by = stop_id
  ) |>
  mutate(per_hour = n_dep / window_hours) |>
  filter(per_hour >= 3)

bb_city <- st_bbox(
  c(xmin = 17.53, ymin = 59.81, xmax = 17.77, ymax = 59.90), crs = 4326
)
city <- st_transform(st_as_sfc(bb_city), crs_se)

stops <- gtfs$stops |>
  inner_join(bunching, by = join_by(stop_id)) |>
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) |>
  st_transform(crs_se) |>
  st_filter(city) |>
  arrange(extra_wait)

cat(nrow(stops), "stop points in the city with >= 3 departures/hour\n")

stops_label <- stops |>
  slice_max(extra_wait, n = 1, by = stop_name) |>
  slice_max(extra_wait, n = 4) |>
  mutate(label = str_remove(stop_name, "^Uppsala "))

kommun <- readRDS(file.path(dir_data, "boundary-uppsala_kommun.rds")) |>
  st_transform(crs_se)

roads <- readRDS(file.path(dir_data, "osm-roads-city-wide.rds")) |>
  st_transform(crs_se) |>
  mutate(major = highway %in% c("motorway", "trunk", "primary", "secondary"))

gg_record(
  here::here("30daymap-temp"),
  width = map_box$width, height = map_box$height, units = "in", dpi = 320
)

p <- ggplot() +
  geom_sf(data = kommun, fill = clr_land, color = NA) +
  geom_sf(data = roads, aes(linewidth = major), color = clr_road) +
  scale_linewidth_manual(values = c(0.15, 0.4), guide = "none") +
  geom_sf(
    data = stops,
    aes(size = per_hour, fill = extra_wait),
    shape = 21, color = clr_bg, stroke = 0.4
  ) +
  ggrepel::geom_text_repel(
    data = stops_label,
    aes(label = label, geometry = geometry),
    stat = "sf_coordinates",
    family = f1, size = 4, color = clr_text,
    min.segment.length = 0, segment.color = clr_text2, segment.size = 0.3,
    box.padding = 0.6, seed = 4
  ) +
  scale_size_area(
    max_size = 6, name = "Departures per hour",
    guide = guide_legend(override.aes = list(fill = clr_text2))
  ) +
  scale_fill_gradient(
    low = colorspace::lighten(clr_accent, 0.45),
    high = colorspace::darken(clr_accent, 0.45),
    name = "Extra wait, minutes",
    guide = guide_colorbar(
      barwidth = unit(0.6, "cm"), barheight = unit(3, "cm")
    )
  ) +
  coord_map_box(bb_city) +
  theme_map()

save_map(
  p,
  file.path(dir_2026, "04-clusters", "04-clusters-bunching.png"),
  title = "Two at once, then nothing",
  subtitle = "Extra minutes of waiting at each bus stop because\\
 departures come in clumps, weekday 07:00–18:00",
  caption = caption_src("UL GTFS via Trafiklab")
)
