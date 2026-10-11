library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = map_width, height = map_height, units = "in", dpi = 320)

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

# Roads and rail drawn last, above the footways that run beside them
bridges <- st_filter(bridges, bb_sf(c(17.60, 59.835, 17.68, 59.875))) |>
  arrange(factor(mode, levels = c("Walk & bike", "Road", "Rail")))
counts <- count(st_drop_geometry(bridges), mode)
water <- osm_water(bb_kommun, "water-kommun")
roads <- osm_roads()

# Portrait frame around the bridges, matching the panel shape
bridges_box <- st_bbox(bridges)
frame_h <- (bridges_box[["ymax"]] - bridges_box[["ymin"]]) + 800
frame_w <- frame_h * 0.85
frame_x <- mean(bridges_box[c("xmin", "xmax")]) + c(-1, 1) * frame_w / 2
frame_y <- mean(bridges_box[c("ymin", "ymax")]) + c(-1, 1) * frame_h / 2

ggplot() +
  geom_sf(data = water, fill = clr_water, color = NA) +
  geom_sf(data = roads, color = clr_road, linewidth = 0.3) +
  geom_sf(data = river, color = clr_water, linewidth = 3) +
  geom_sf(data = bridges, color = clr_bg, linewidth = 3.4) +
  geom_sf(data = bridges, aes(color = mode), linewidth = 2.2) +
  scale_color_manual(
    values = c("Road" = clr_text, "Walk & bike" = clr_accent, "Rail" = clr_accent2),
    labels = function(x) paste0(x, " (", counts$n[match(x, counts$mode)], ")")
  ) +
  coord_sf(xlim = frame_x, ylim = frame_y, expand = FALSE) +
  labs(
    title = "Crossing Fyrisån",
    subtitle = "Every bridge over the river in central Uppsala,\nby who can use it",
    caption = caption_src("OpenStreetMap"),
    color = NULL
  ) +
  theme_map() +
  theme(panel.background = element_rect(fill = clr_land, color = NA))

record_polaroid()
