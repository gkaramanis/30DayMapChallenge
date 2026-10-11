library(tidyverse)
library(sf)
library(patchwork)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = 11, height = 7, dpi = 320)

kommun <- uppsala_kommun()

trip <- st_sfc(
  st_linestring(rbind(c(17.6459, 59.8586), c(18.0575, 59.3300))), # Uppsala C to Stockholm C
  crs = 4326
)

measure <- function(crs) {
  k <- st_transform(kommun, crs)
  t <- st_transform(trip, crs)
  list(
    kommun = k,
    trip = t,
    area_km2 = as.numeric(st_area(st_set_crs(k, NA))) / 1e6, # planar area in projected units
    length_km = as.numeric(st_length(st_set_crs(t, NA))) / 1e3
  )
}

panel <- function(crs, name, col) {
  m <- measure(crs)
  ggplot() +
    geom_sf(data = m$kommun, fill = col, color = NA, alpha = 0.8) +
    geom_sf(data = m$trip, linewidth = 0.8, linetype = "dashed") +
    coord_sf(crs = crs, datum = NA) +
    labs(
      title = name,
      subtitle = paste0(
        "Uppsala kommun: ", scales::number(m$area_km2, big.mark = " "), " km²\n",
        "Uppsala C to Stockholm C: ", round(m$length_km), " km"
      )
    ) +
    theme_void(base_family = f1) +
    theme(
      plot.title = element_text(family = f2, size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5, margin = margin(5, 0, 10, 0))
    )
}

panel(3006, "SWEREF 99 TM", "#3a86ff") +
  panel(3857, "Web Mercator", "#ff006e") +
  plot_annotation(
    title = "Same kommun, four times the size",
    subtitle = "At 60°N Web Mercator stretches distances by about 2× and areas by about 4×",
    caption = caption_src("OpenStreetMap"),
    theme = theme_void(base_family = f1) +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(family = f2, size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(5, 0, 10, 0)),
        plot.caption = element_text(size = 8),
        plot.margin = margin(10, 10, 10, 10)
      )
  )

record_polaroid()
