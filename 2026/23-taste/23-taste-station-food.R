library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = map_width, height = map_height, units = "in", dpi = 320)

lan <- uppsala_lan() |> st_transform(crs_se)

stations <- osm_get("stations-lan", bb_lan, list(railway = "station"))$osm_points |>
  select(name) |>
  filter(!is.na(name)) |>
  st_transform(crs_se) |>
  st_filter(lan)

food <- osm_get("food-lan", bb_lan, list(amenity = c("restaurant", "fast_food", "cafe")))
places <- bind_rows(
  food$osm_points |> select(osm_id, any_of("amenity")),
  food$osm_polygons |> select(osm_id, any_of("amenity")) |> st_centroid()
)
places <- places[tag(places, "amenity") %in% c("restaurant", "fast_food", "cafe"), ] |>
  st_transform(crs_se)

# 400 m is about a five-minute walk
counts <- st_join(st_buffer(stations, 400), places, join = st_intersects) |>
  st_drop_geometry() |>
  group_by(name) |>
  summarise(
    total = sum(!is.na(amenity)),
    restaurant = sum(amenity %in% "restaurant"),
    fast_food = sum(amenity %in% "fast_food"),
    cafe = sum(amenity %in% "cafe")
  )

stations <- stations |>
  left_join(counts, by = "name") |>
  arrange(total)

rail <- osm_get("rail-lan", bb_lan, list(railway = "rail"))$osm_lines
rail <- rail[is.na(tag(rail, "service")), ] |>
  st_transform(crs_se)

view <- st_bbox(st_buffer(st_union(stations), 6000))

ggplot() +
  geom_sf(data = lan, fill = clr_land, color = clr_grid, linewidth = 0.4) +
  geom_sf(data = st_intersection(rail, lan), color = clr_text2, linewidth = 0.3, alpha = 0.6) +
  geom_sf(data = stations, aes(size = total), shape = 21, fill = clr_accent2, color = clr_bg, alpha = 0.8, stroke = 0.4) +
  geom_sf(data = slice_max(stations, total, n = 1, with_ties = FALSE), aes(size = total), shape = 21, fill = clr_accent, color = clr_bg, stroke = 0.4) +
  ggrepel::geom_text_repel(data = slice_max(stations, total, n = 7, with_ties = FALSE), aes(label = paste0(name, " ", total), geometry = geometry), stat = "sf_coordinates", family = f1, size = 5, color = clr_text, seed = 3, bg.color = clr_bg, bg.r = 0.15, min.segment.length = 0, box.padding = 1.2, force = 3, segment.color = clr_text2) +
  scale_size_area(max_size = 16, breaks = c(10, 50, 100)) +
  coord_sf(xlim = view[c(1, 3)], ylim = view[c(2, 4)], expand = FALSE) +
  labs(
    title = "A bite before the train",
    subtitle = "Restaurants, fast food and cafés within 400 m\nof each station in Uppsala län",
    caption = caption_src("OpenStreetMap"),
    size = "Places within 400 m (about five minutes' walk)"
  ) +
  guides(size = guide_legend(override.aes = list(fill = clr_accent2, alpha = 0.8))) +
  theme_map()

record_polaroid()
