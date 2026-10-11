library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = map_width, height = map_height, units = "in", dpi = 320)

rail <- osm_get("rail-lan", bb_lan, list(railway = "rail"))$osm_lines
rail <- rail[is.na(tag(rail, "service")), ] |>
  mutate(electrified = if_else(tag(pick(everything()), "electrified") %in% c("contact_line", "rail"), "Electrified", "Not electrified")) |>
  st_transform(crs_se)

substations <- osm_get("substations-lan", bb_lan, list(power = "substation"))
traction <- bind_rows(
  substations$osm_points |> select(osm_id, any_of(c("substation", "name"))),
  substations$osm_polygons |> select(osm_id, any_of(c("substation", "name"))) |> st_centroid()
)
traction <- traction[tag(traction, "substation") %in% "traction", ] |>
  st_transform(crs_se)

power_lines <- osm_get("power-lines-lan", bb_lan, list(power = "line"))$osm_lines |>
  st_transform(crs_se)

lan <- uppsala_lan() |> st_transform(crs_se)

ggplot() +
  geom_sf(data = lan, fill = clr_land, color = clr_grid, linewidth = 0.3) +
  geom_sf(data = st_intersection(power_lines, lan), color = clr_accent2, alpha = 0.5, linewidth = 0.2) +
  geom_sf(data = st_intersection(rail, lan), aes(color = electrified), linewidth = 0.8) +
  geom_sf(data = st_filter(traction, lan), shape = 23, fill = clr_accent2, color = clr_bg, size = 3.5) +
  scale_color_manual(values = c("Electrified" = clr_accent, "Not electrified" = clr_text2)) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Power to the trains",
    subtitle = "Railways in Uppsala län by electrification,\ntraction substations (diamonds)\nand the high-voltage grid (blue)",
    caption = caption_src("OpenStreetMap"),
    color = NULL
  ) +
  guides(color = guide_legend(override.aes = list(linewidth = 1.5))) +
  theme_map()

record_polaroid()
