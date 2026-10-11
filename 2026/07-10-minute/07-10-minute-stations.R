library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = map_width, height = map_height, units = "in", dpi = 320)

stations <- osm_get("stations-lan", bb_lan, list(railway = "station"))$osm_points |>
  select(name) |>
  st_transform(crs_se)

rail <- osm_get("rail-lan", bb_lan, list(railway = "rail"))$osm_lines
rail <- rail[is.na(tag(rail, "service")), ] |>
  st_transform(crs_se)

lan <- uppsala_lan() |> st_transform(crs_se)
stations_lan <- st_filter(stations, lan)

ggplot() +
  geom_sf(data = lan, fill = clr_land, color = clr_grid, linewidth = 0.3) +
  geom_sf(data = st_intersection(rail, lan), linewidth = 0.5, color = clr_text2) +
  geom_sf(data = stations_lan, shape = 21, fill = clr_bg, color = clr_accent2, size = 3, stroke = 1) +
  geom_sf(data = filter(stations_lan, name == "Uppsala central"), shape = 21, fill = clr_accent, color = clr_text, size = 4.5, stroke = 0.8) +
  ggrepel::geom_text_repel(data = stations_lan, aes(label = name, geometry = geometry), stat = "sf_coordinates", family = f1, size = 3.6, color = clr_text, seed = 1, max.overlaps = Inf, min.segment.length = 0.2, segment.color = clr_text2) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Train stations in Uppsala län",
    subtitle = paste0(nrow(stations_lan), " stations on the railways of the county"),
    caption = caption_src("OpenStreetMap") |> paste("· Made in 10 minutes")
  ) +
  theme_map()

record_polaroid()
