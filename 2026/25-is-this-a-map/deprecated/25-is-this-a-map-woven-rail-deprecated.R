library(tidyverse)
library(sf)

source(here::here("2026", "R", "helpers.R"))

# Delay data is not cached (no Trafikverket key), so the weave uses trains per day per section from GTFS
sections <- readRDS(file.path(dir_2026, "data", "rail-sections-operator-uppsala-20261103.rds")) |>
  filter(stop_lat_a > 59.6, stop_lat_b > 59.6, stop_lon_a > 17.2, stop_lon_b > 17.2)

section_lines <- sections |>
  mutate(id = row_number(), x1 = stop_lon_a, y1 = stop_lat_a, x2 = stop_lon_b, y2 = stop_lat_b) |>
  rowwise() |>
  mutate(geometry = list(st_linestring(matrix(c(x1, x2, y1, y2), 2)))) |>
  ungroup() |>
  st_as_sf(crs = 4326) |>
  st_transform(crs_se)

# Strands per section as offset sine waves, alternating phase so they read as a weave
weave_strands <- function(geom, trains, id) {
  xy <- st_coordinates(geom)[, 1:2]
  len <- sqrt(sum((xy[2, ] - xy[1, ])^2))
  dir <- (xy[2, ] - xy[1, ]) / len
  nrm <- c(-dir[2], dir[1])
  n <- max(1, round(trains / 12))
  t <- seq(0, len, length.out = 120)
  map_dfr(seq_len(n), function(k) {
    off <- (k - (n + 1) / 2) * 450
    wave <- 300 * sin(2 * pi * t / 4000 + k * pi)
    tibble(id = id, strand = k, trains = trains, x = xy[1, 1] + dir[1] * t + nrm[1] * (off + wave), y = xy[1, 2] + dir[2] * t + nrm[2] * (off + wave))
  })
}

strands <- pmap_dfr(list(st_geometry(section_lines), section_lines$trains, section_lines$id), weave_strands) |>
  mutate(group = paste(id, strand))

p <- ggplot(strands, aes(x, y, group = group, color = trains)) +
  geom_path(linewidth = 0.8) +
  scale_color_viridis_c(option = "magma", end = 0.9) +
  coord_equal() +
  theme_void() +
  theme(legend.position = "none", plot.background = element_rect(fill = clr_bg, color = NA))

ggsave(here::here("2026/25-is-this-a-map/alternatives/25-is-this-a-map-woven-rail.png"), p, width = map_box$width, height = map_box$height, dpi = 150)
