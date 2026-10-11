library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = map_width, height = map_height, units = "in", dpi = 320)

# Tram lines (1906-1953) from OpenHistoricalMap's Overpass
tram <- cached("ohm-tram", {
  osmdata::set_overpass_url("https://overpass-api.openhistoricalmap.org/api/interpreter")
  res <- osmdata::opq(bb_city, timeout = 300) |>
    osmdata::add_osm_feature("railway", "tram") |>
    osmdata::osmdata_sf()
  osmdata::set_overpass_url("https://overpass-api.de/api/interpreter")
  res
})

tram_lines <- tram$osm_lines |>
  select(osm_id, any_of(c("name", "start_date", "end_date"))) |>
  st_transform(crs_se)

roads <- osm_roads()
water <- osm_water()
area <- st_buffer(st_union(tram_lines), 600) |> st_bbox()
centre <- c((area[1] + area[3]) / 2, (area[2] + area[4]) / 2)
half_height <- max((area[4] - area[2]) / 2, (area[3] - area[1]) / 2 / 1.1)
half_width <- half_height * 1.1

ggplot() +
  geom_sf(data = water, fill = clr_water, color = NA) +
  geom_sf(data = roads, color = clr_road, linewidth = 0.25) +
  geom_sf(data = tram_lines, color = clr_bg, linewidth = 2.6) +
  geom_sf(data = tram_lines, color = clr_accent2, linewidth = 1.1) +
  coord_sf(xlim = centre[1] + c(-1, 1) * half_width, ylim = centre[2] + c(-1, 1) * half_height, expand = FALSE) +
  labs(
    title = "UPSALA SPÅRVÄGAR",
    subtitle = "Where the trams ran, 1906–1953,\nover the streets of today",
    caption = caption_src("OpenHistoricalMap, OpenStreetMap")
  ) +
  theme_map()

record_polaroid()
