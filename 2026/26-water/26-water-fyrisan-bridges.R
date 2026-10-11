library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = 7, height = 10, dpi = 320)

river <- osm_get("fyrisan", bb_kommun, list(waterway = "river"))$osm_lines
river <- river[tag(river, "name") %in% "Fyrisån", ] |>
  st_transform(crs_se)

bridges <- osm_get("bridges-kommun", bb_kommun, list(bridge = "yes"))$osm_lines
bridges <- bridges |>
  mutate(
    highway = tag(pick(everything()), "highway"),
    railway = tag(pick(everything()), "railway")
  ) |>
  filter(!is.na(highway) | !is.na(railway)) |>
  st_transform(crs_se) |>
  st_filter(st_buffer(st_union(river), 15)) |>
  mutate(
    mode = case_when(
      !is.na(railway) ~ "Rail",
      highway %in% c("cycleway", "footway", "path", "pedestrian", "steps") ~ "Walk & bike",
      TRUE ~ "Road"
    )
  )

counts <- count(st_drop_geometry(bridges), mode)
river_area <- st_buffer(st_union(bridges), 1000) |> st_bbox()
water <- osm_water(bb_kommun, "water-kommun")

ggplot() +
  geom_sf(data = water, fill = "#bcd9e8", color = NA) +
  geom_sf(data = river, color = "#4f8fba", linewidth = 1.5) +
  geom_sf(data = bridges, aes(color = mode), linewidth = 2.2) +
  scale_color_manual(
    values = c("Road" = "#3d405b", "Walk & bike" = "#e07a5f", "Rail" = "#81b29a"),
    labels = function(x) paste0(x, " (", counts$n[match(x, counts$mode)], ")")
  ) +
  coord_sf(xlim = river_area[c(1, 3)], ylim = river_area[c(2, 4)], expand = FALSE) +
  labs(
    title = "Crossing Fyrisån",
    subtitle = "Every bridge over the river, by who can use it",
    caption = caption_src("OpenStreetMap"),
    color = NULL
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "top",
    plot.background = element_rect(fill = "#f7fbfd", color = NA),
    plot.title = element_text(family = f2, size = 20, face = "bold", color = "#1d3557"),
    plot.subtitle = element_text(size = 11, margin = margin(5, 0, 10, 0)),
    plot.caption = element_text(size = 8),
    plot.margin = margin(10, 10, 10, 10)
  )

record_polaroid()
