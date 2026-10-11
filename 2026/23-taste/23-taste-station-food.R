library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = 9, height = 9, dpi = 320)

lan <- uppsala_lan() |> st_transform(crs_se)

stations <- osm_get("stations-lan", bb_lan, list(railway = "station"))$osm_points |>
  select(name) |>
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

ggplot() +
  geom_sf(data = lan, fill = "#fdf6ec", color = NA) +
  geom_sf(data = st_intersection(rail, lan), color = "grey60", linewidth = 0.4) +
  geom_sf(data = stations, aes(size = total, fill = total), shape = 21, color = "white") +
  ggrepel::geom_text_repel(data = stations, aes(label = paste0(name, " ", total), geometry = geometry), stat = "sf_coordinates", family = f1, size = 2.8, seed = 3, bg.color = "white", bg.r = 0.1) +
  scale_size_area(max_size = 16) +
  scale_fill_gradientn(colors = MetBrewer::met.brewer("Tam")) +
  coord_sf(expand = FALSE) +
  labs(
    title = "A bite before the train",
    subtitle = "Restaurants, fast food and cafés within 400 m (about five minutes' walk) of each station in Uppsala län",
    caption = caption_src("OpenStreetMap")
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
