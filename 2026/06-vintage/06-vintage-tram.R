library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = 8, height = 9, dpi = 320)

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
centre <- st_buffer(st_union(tram_lines), 600) |> st_bbox()

col_paper <- "#eadfc8"
col_ink <- "#3b2f2f"
col_tram <- "#a23b2a"

ggplot() +
  geom_sf(data = water, fill = "#c9cdb8", color = col_ink, linewidth = 0.15) +
  geom_sf(data = roads, color = col_ink, linewidth = 0.15, alpha = 0.6) +
  geom_sf(data = tram_lines, color = col_paper, linewidth = 2.6) +
  geom_sf(data = tram_lines, color = col_tram, linewidth = 1.1) +
  coord_sf(xlim = centre[c(1, 3)], ylim = centre[c(2, 4)], expand = FALSE) +
  labs(
    title = "UPSALA SPÅRVÄGAR",
    subtitle = "Where the trams ran, 1906–1953, over the streets of today",
    caption = caption_src("OpenHistoricalMap, OpenStreetMap")
  ) +
  theme_void(base_family = f2) +
  theme(
    plot.background = element_rect(fill = col_paper, color = col_ink, linewidth = 2),
    panel.border = element_rect(fill = NA, color = col_ink, linewidth = 0.6),
    plot.title = element_text(size = 24, hjust = 0.5, color = col_ink, margin = margin(5, 0, 0, 0)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, face = "italic", color = col_ink, margin = margin(5, 0, 10, 0)),
    plot.caption = element_text(family = f1, size = 7, hjust = 0.5, color = col_ink),
    plot.margin = margin(15, 15, 15, 15)
  )

record_polaroid()
