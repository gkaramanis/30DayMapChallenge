library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = 10, height = 10, dpi = 320)

# One bike takes about 0.7 m × 1.8 m of rack space
m2_per_bike <- 0.7 * 1.8

parking <- osm_get("bike-parking-centre", bb_centre, list(amenity = "bicycle_parking"))

pts <- parking$osm_points
pts <- pts[!is.na(tag(pts, "amenity")), ]

# Points drawn as squares of true area; mapped areas kept as drawn
racks <- bind_rows(
  pts |>
    mutate(capacity = suppressWarnings(as.numeric(tag(pick(everything()), "capacity")))) |>
    st_transform(crs_se) |>
    mutate(
      known = !is.na(capacity),
      geometry = st_buffer(geometry, sqrt(coalesce(capacity, 2) * m2_per_bike) / 2, endCapStyle = "SQUARE")
    ),
  parking$osm_polygons |>
    mutate(capacity = suppressWarnings(as.numeric(tag(pick(everything()), "capacity")))) |>
    st_transform(crs_se) |>
    mutate(known = TRUE)
) |>
  select(capacity, known)

buildings <- osm_get("buildings-centre", bb_centre, list(building = NA))$osm_polygons |>
  st_transform(crs_se)
roads <- osm_roads(bb_centre, "roads-centre")
centre <- bb_sf(bb_centre)

total <- sum(racks$capacity, na.rm = TRUE)

ggplot() +
  geom_sf(data = buildings, fill = "grey88", color = "white", linewidth = 0.1) +
  geom_sf(data = roads, color = "grey80", linewidth = 0.3) +
  geom_sf(data = racks, aes(fill = known), color = NA) +
  scale_fill_manual(values = c("TRUE" = "#1b4965", "FALSE" = "#ee964b"), labels = c("TRUE" = "Capacity tagged", "FALSE" = "Capacity unknown, drawn as 2 bikes")) +
  coord_sf(xlim = st_bbox(centre)[c(1, 3)], ylim = st_bbox(centre)[c(2, 4)], expand = FALSE) +
  labs(
    title = "Every bike space in central Uppsala, at true size",
    subtitle = paste0(scales::number(total, big.mark = " "), " tagged spaces, each drawn as 0.7 × 1.8 m of ground"),
    caption = caption_src("OpenStreetMap amenity=bicycle_parking"),
    fill = NULL
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "top",
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(family = f2, size = 18, face = "bold"),
    plot.subtitle = element_text(size = 10, margin = margin(5, 0, 10, 0)),
    plot.caption = element_text(size = 8),
    plot.margin = margin(10, 10, 10, 10)
  )

record_polaroid()
