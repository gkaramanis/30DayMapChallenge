library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = 8, height = 9, dpi = 320)

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

col_bg <- "#101820"

ggplot() +
  geom_sf(data = lan, fill = "#18232e", color = NA) +
  geom_sf(data = st_intersection(power_lines, lan), color = "#3c5a74", linewidth = 0.2) +
  geom_sf(data = st_intersection(rail, lan), aes(color = electrified), linewidth = 0.8) +
  geom_sf(data = st_filter(traction, lan), shape = 23, fill = "#fee715", color = col_bg, size = 3) +
  scale_color_manual(values = c("Electrified" = "#fee715", "Not electrified" = "grey55")) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Power to the trains",
    subtitle = "Railways in Uppsala län by electrification, traction substations (diamonds) and the high-voltage grid (blue)",
    caption = caption_src("OpenStreetMap"),
    color = NULL
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "top",
    legend.text = element_text(color = "white"),
    plot.background = element_rect(fill = col_bg, color = NA),
    plot.title = element_text(family = f2, size = 18, face = "bold", color = "white"),
    plot.subtitle = element_text(size = 10, color = "grey80", margin = margin(5, 0, 10, 0)),
    plot.caption = element_text(size = 8, color = "grey70"),
    plot.margin = margin(10, 10, 10, 10)
  )

record_polaroid()
