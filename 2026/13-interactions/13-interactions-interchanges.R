library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = map_width, height = map_height, units = "in", dpi = 320)

# National feed: UL plus all passenger rail. Not yet every operator (e.g. some
# long-distance buses); GTFS Sverige 2 is complete but has no shapes.
# Large: needs several GB of RAM.
gtfs <- sweden_gtfs()
on <- typical_weekday(gtfs)

lan <- uppsala_lan()

stops_lan <- stops_sf(gtfs) |>
  st_filter(lan) |>
  st_transform(crs_se)

st_lan <- stop_times_on(gtfs, on) |>
  semi_join(st_drop_geometry(stops_lan), by = "stop_id") |>
  mutate(mode = if_else(is_rail(route_type), "rail", "bus"))

# Interchanges: rail stations plus every stop within 300 m of them
stations <- stops_lan |>
  semi_join(st_lan |> filter(mode == "rail"), by = "stop_id") |>
  mutate(group_id = coalesce(na_if(parent_station, ""), stop_id)) |>
  group_by(group_id) |>
  summarise(name = first(stop_name)) |>
  st_centroid()

catchment <- st_join(stops_lan |> select(stop_id), st_buffer(stations, 300), left = FALSE)

interchanges <- st_lan |>
  inner_join(st_drop_geometry(catchment), by = "stop_id") |>
  group_by(group_id, name) |>
  summarise(
    departures = n(),
    rail = sum(mode == "rail"),
    operators = n_distinct(agency_id),
    .groups = "drop"
  ) |>
  left_join(stations |> select(group_id), by = "group_id") |>
  st_as_sf() |>
  arrange(departures)

rail_lines <- osm_get("rail-lan", bb_lan, list(railway = "rail"))$osm_lines
rail_lines <- rail_lines[is.na(tag(rail_lines, "service")), ] |>
  st_transform(crs_se)

lan <- lan |> st_transform(crs_se)

ggplot() +
  geom_sf(data = lan, fill = clr_land, color = NA) +
  geom_sf(data = st_intersection(rail_lines, lan), color = clr_text2, linewidth = 0.4) +
  geom_sf(data = interchanges, aes(size = departures, fill = operators), shape = 21, color = clr_bg, stroke = 0.6) +
  ggrepel::geom_text_repel(data = interchanges |> slice_max(departures, n = 10), aes(label = str_remove(name, " station.*$"), geometry = geometry), stat = "sf_coordinates", family = f1, size = 4, color = clr_text, seed = 2, bg.color = clr_bg, bg.r = 0.1) +
  scale_size_area(max_size = 14, labels = scales::label_number(big.mark = " ")) +
  scale_fill_steps(low = clr_accent, high = clr_accent2, breaks = 1:6, guide = guide_coloursteps(title.position = "top", title.hjust = 0.5, order = 2)) +
  guides(size = guide_legend(title.position = "top", title.hjust = 0.5, order = 1, override.aes = list(shape = 21, fill = clr_accent2, color = clr_bg))) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Where trains and buses meet",
    subtitle = paste0("Departures within 300 m of each railway station\nin Uppsala län on ", str_squish(format(on, "%e %B %Y")), ",\ncoloured by number of operators"),
    caption = caption_src("GTFS Sweden 3 via Trafiklab, OpenStreetMap"),
    size = "Departures", fill = "Operators"
  ) +
  theme_map() +
  theme(legend.box = "horizontal", legend.key.width = unit(1.6, "lines"), legend.spacing.x = unit(0.3, "lines"), legend.text = element_text(size = 12), legend.key.height = unit(0.8, "lines"))

record_polaroid()
