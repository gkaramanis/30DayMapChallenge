library(tidyverse)
library(sf)
library(igraph)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = map_width, height = map_height, units = "in", dpi = 320)

gtfs <- ul_gtfs()
on <- typical_weekday(gtfs)

stops <- stops_sf(gtfs) |>
  mutate(node = coalesce(na_if(parent_station, ""), stop_id))

kommun <- uppsala_kommun()
in_kommun <- stops |> st_filter(kommun) |> pull(node) |> unique()

# Edges between consecutive stops of each trip, weighted by mean travel time
edges <- stop_times_on(gtfs, on) |>
  filter(is_bus(route_type)) |>
  left_join(st_drop_geometry(stops) |> select(stop_id, node), by = "stop_id") |>
  arrange(trip_id, stop_sequence) |>
  group_by(trip_id) |>
  mutate(to = lead(node), minutes = (lead(arr_s) - dep_s) / 60) |>
  ungroup() |>
  filter(!is.na(to), node != to, node %in% in_kommun, to %in% in_kommun) |>
  group_by(from = node, to) |>
  summarise(trips = n(), minutes = pmax(mean(minutes, na.rm = TRUE), 0.5), .groups = "drop")

g <- graph_from_data_frame(edges, directed = TRUE)
btw <- betweenness(g, weights = E(g)$minutes, normalized = TRUE)

nodes <- stops |>
  group_by(node) |>
  summarise(name = first(stop_name)) |>
  st_centroid() |>
  filter(node %in% names(btw)) |>
  mutate(betweenness = btw[node]) |>
  st_transform(crs_se) |>
  arrange(betweenness)

xy <- nodes |>
  mutate(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]) |>
  st_drop_geometry() |>
  select(node, x, y)

segments <- edges |>
  left_join(xy, by = c("from" = "node")) |>
  left_join(xy |> rename(xend = x, yend = y), by = c("to" = "node"))

ggplot() +
  geom_segment(data = segments, aes(x, y, xend = xend, yend = yend, linewidth = trips), color = clr_text2, alpha = 0.3) +
  geom_sf(data = nodes, aes(size = betweenness, color = betweenness), alpha = 0.85) +
  ggrepel::geom_text_repel(data = nodes |> slice_max(betweenness, n = 8), aes(label = str_remove(name, " \\(Uppsala\\)"), geometry = geometry), stat = "sf_coordinates", family = f1, size = 5, color = clr_text, seed = 4, bg.color = clr_bg, bg.r = 0.15, segment.color = clr_text2) +
  scale_linewidth(range = c(0.05, 1.2), guide = "none") +
  scale_size_area(max_size = 8, breaks = c(0.05, 0.15, 0.3)) +
  scale_color_steps(low = clr_accent2, high = clr_accent, n.breaks = 6, guide = "none") +
  coord_sf(crs = crs_se, expand = FALSE) +
  labs(
    title = "The stops everything\npasses through",
    subtitle = "UL bus network in Uppsala kommun as a graph,\nstops sized by betweenness on\ntravel-time-weighted shortest paths",
    caption = caption_src("UL GTFS via Trafiklab, igraph"),
    size = "Betweenness"
  ) +
  guides(size = guide_legend(override.aes = list(color = clr_accent2))) +
  theme_map()

record_polaroid()
