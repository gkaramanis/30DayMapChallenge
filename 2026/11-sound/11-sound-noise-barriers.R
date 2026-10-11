library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = map_width, height = map_height, units = "in", dpi = 320)

# Official noise maps are PDF only, so map where Uppsala builds walls against sound
barriers <- osm_get("noise-barriers", bb_kommun, list(wall = "noise_barrier"))$osm_lines |>
  st_transform(crs_se) |>
  mutate(length_m = as.numeric(st_length(geometry)))

loud <- osm_get("loud-roads", bb_kommun, list(highway = c("motorway", "trunk", "primary"), railway = "rail"))$osm_lines |>
  st_transform(crs_se)
loud <- loud[is.na(tag(loud, "service")), ]

kommun <- uppsala_kommun() |> st_transform(crs_se)
area <- st_buffer(st_union(barriers), 1500) |> st_bbox()
# Square frame centred on the barriers, so the map fills the page
half <- max(area[["xmax"]] - area[["xmin"]], area[["ymax"]] - area[["ymin"]]) / 2
mid <- c(mean(area[c("xmin", "xmax")]), mean(area[c("ymin", "ymax")]))
area <- c(mid[1] - half, mid[2] - half, mid[1] + half, mid[2] + half)

total_km <- round(sum(barriers$length_m) / 1000, 1)

ggplot() +
  geom_sf(data = kommun, fill = clr_land, color = NA) +
  geom_sf(data = loud, color = clr_text2, linewidth = 0.4) +
  geom_sf(data = barriers, color = clr_accent, linewidth = 2) +
  coord_sf(xlim = area[c(1, 3)], ylim = area[c(2, 4)], expand = FALSE) +
  labs(
    title = "Walls against noise",
    subtitle = paste0(total_km, " km of noise barriers (amber) along Uppsala's\nmotorways, main roads and railways (grey)"),
    caption = caption_src("OpenStreetMap wall=noise_barrier")
  ) +
  theme_map()

record_polaroid()
