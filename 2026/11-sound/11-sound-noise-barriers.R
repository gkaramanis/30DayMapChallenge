library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = 8, height = 9, dpi = 320)

# Official noise maps are PDF only, so map where Uppsala builds walls against sound
barriers <- osm_get("noise-barriers", bb_kommun, list(wall = "noise_barrier"))$osm_lines |>
  st_transform(crs_se) |>
  mutate(length_m = as.numeric(st_length(geometry)))

loud <- osm_get("loud-roads", bb_kommun, list(highway = c("motorway", "trunk", "primary"), railway = "rail"))$osm_lines |>
  st_transform(crs_se)
loud <- loud[is.na(tag(loud, "service")), ]

kommun <- uppsala_kommun() |> st_transform(crs_se)
area <- st_buffer(st_union(barriers), 1500) |> st_bbox()

total_km <- round(sum(barriers$length_m) / 1000, 1)

ggplot() +
  geom_sf(data = kommun, fill = "#f4f2ee", color = NA) +
  geom_sf(data = loud, color = "grey65", linewidth = 0.4) +
  geom_sf(data = barriers, color = "#d1495b", linewidth = 1.2) +
  coord_sf(xlim = area[c(1, 3)], ylim = area[c(2, 4)], expand = FALSE) +
  labs(
    title = "Walls against noise",
    subtitle = paste0(total_km, " km of noise barriers (red) along Uppsala's motorways, main roads and railways (grey)"),
    caption = caption_src("OpenStreetMap wall=noise_barrier")
  ) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(family = f2, size = 18, face = "bold"),
    plot.subtitle = element_text(size = 10, margin = margin(5, 0, 10, 0)),
    plot.caption = element_text(size = 8),
    plot.margin = margin(10, 10, 10, 10)
  )

record_polaroid()
