library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = map_width, height = map_height, units = "in", dpi = 320)

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
  geom_sf(data = cycleways, color = colorspace::darken(clr_grid, 0.08), linewidth = 0.3) +
  geom_sf(data = smells |> arrange(desc(dist_m)), aes(color = dist_m, shape = kind), size = 2.6) +
  scale_color_gradient(low = clr_accent, high = clr_accent2, limits = c(0, 200), oob = scales::squish) +
  scale_shape_manual(values = c("Café" = 16, "Bakery" = 17)) +
  coord_sf(xlim = st_bbox(city)[c(1, 3)], ylim = st_bbox(city)[c(2, 4)], expand = FALSE) +
  labs(
    title = "Follow your nose",
    subtitle = paste0(scales::percent(within_reach), " of Uppsala's cafés and bakeries\nare within 50 m of a cycleway"),
    caption = caption_src("OpenStreetMap"),
    color = "Metres to cycleway", shape = NULL
  ) +
  guides(
    color = guide_colourbar(order = 1, theme = theme(legend.key.width = unit(10, "lines"), legend.key.height = unit(0.8, "lines"), legend.title.position = "top")),
    shape = guide_legend(order = 2, override.aes = list(color = clr_text))
  ) +
  theme_map() +
  theme(legend.box = "vertical", legend.spacing.y = unit(4, "pt"))

record_polaroid()
