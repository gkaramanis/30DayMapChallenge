library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = 7, height = 8, dpi = 320)

stations <- osm_get("stations-lan", bb_lan, list(railway = "station"))$osm_points |>
  select(name) |>
  st_transform(crs_se)

rail <- osm_get("rail-lan", bb_lan, list(railway = "rail"))$osm_lines
rail <- rail[is.na(tag(rail, "service")), ] |>
  st_transform(crs_se)

lan <- uppsala_lan() |> st_transform(crs_se)

ggplot() +
  geom_sf(data = lan, fill = "#f2efe9", color = NA) +
  geom_sf(data = st_intersection(rail, lan), linewidth = 0.4, color = "grey30") +
  geom_sf(data = st_filter(stations, lan), shape = 21, fill = "white", size = 2.5, stroke = 0.8) +
  ggrepel::geom_text_repel(data = st_filter(stations, lan), aes(label = name, geometry = geometry), stat = "sf_coordinates", family = f1, size = 2.8, seed = 1) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Train stations in Uppsala län",
    caption = caption_src("OpenStreetMap") |> paste("· Made in 10 minutes")
  ) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(family = f2, size = 16, face = "bold"),
    plot.margin = margin(10, 10, 10, 10)
  )

record_polaroid()
