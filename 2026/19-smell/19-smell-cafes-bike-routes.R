library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = 9, height = 9, dpi = 320)

city <- bb_sf(bb_city)

food <- osm_get("cafes-bakeries-city", bb_city, list(amenity = "cafe", shop = "bakery"))
smells <- bind_rows(
  food$osm_points |> select(osm_id, any_of(c("name", "amenity", "shop"))),
  food$osm_polygons |> select(osm_id, any_of(c("name", "amenity", "shop"))) |> st_centroid()
)
smells <- smells[tag(smells, "amenity") %in% "cafe" | tag(smells, "shop") %in% "bakery", ] |>
  mutate(kind = if_else(tag(pick(everything()), "shop") %in% "bakery", "Bakery", "Café")) |>
  st_transform(crs_se) |>
  st_filter(city)

cycleways <- osm_cycleways() |> st_filter(city)

nearest <- st_nearest_feature(smells, cycleways)
smells$dist_m <- as.numeric(st_distance(smells, cycleways[nearest, ], by_element = TRUE))

within_reach <- mean(smells$dist_m <= 50)

ggplot() +
  geom_sf(data = cycleways, color = "#7f9c96", linewidth = 0.3) +
  geom_sf(data = smells |> arrange(desc(dist_m)), aes(color = dist_m, shape = kind), size = 2) +
  scale_color_gradient(low = "#9c4a1a", high = "#f1d4a8", limits = c(0, 200), oob = scales::squish) +
  scale_shape_manual(values = c("Café" = 16, "Bakery" = 17)) +
  coord_sf(xlim = st_bbox(city)[c(1, 3)], ylim = st_bbox(city)[c(2, 4)], expand = FALSE) +
  labs(
    title = "Follow your nose",
    subtitle = paste0(scales::percent(within_reach), " of Uppsala's cafés and bakeries are within 50 m of a cycleway"),
    caption = caption_src("OpenStreetMap"),
    color = "Metres to\ncycleway", shape = NULL
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "right",
    plot.background = element_rect(fill = "#fffaf3", color = NA),
    plot.title = element_text(family = f2, size = 20, face = "bold", color = "#9c4a1a"),
    plot.subtitle = element_text(size = 11, margin = margin(5, 0, 10, 0)),
    plot.caption = element_text(size = 8),
    plot.margin = margin(10, 10, 10, 10)
  )

record_polaroid()
