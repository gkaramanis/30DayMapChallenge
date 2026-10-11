library(tidyverse)
library(sf)
library(igraph)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = 9, height = 9, dpi = 320)

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
  geom_segment(data = segments, aes(x, y, xend = xend, yend = yend, linewidth = trips), color = "grey70", alpha = 0.5) +
  geom_sf(data = nodes, aes(size = betweenness, color = betweenness)) +
  ggrepel::geom_text_repel(data = nodes |> slice_max(betweenness, n = 8), aes(label = name, geometry = geometry), stat = "sf_coordinates", family = f1, size = 3, seed = 4, bg.color = "white", bg.r = 0.1) +
  scale_linewidth(range = c(0.05, 1.2)) +
  scale_size_area(max_size = 8) +
  scale_color_gradientn(colors = MetBrewer::met.brewer("Demuth", direction = -1)) +
  coord_sf(crs = crs_se, expand = FALSE) +
  labs(
    title = "The stops everything passes through",
    subtitle = "UL bus network in Uppsala kommun as a graph. Stops sized by betweenness on travel-time-weighted shortest paths",
    caption = caption_src("UL GTFS via Trafiklab, igraph")
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(family = f2, size = 18, face = "bold"),
    plot.subtitle = element_text(size = 10, margin = margin(5, 0, 10, 0)),
    plot.caption = element_text(size = 8),
    plot.margin = margin(10, 10, 10, 10)
  )

record_polaroid()
