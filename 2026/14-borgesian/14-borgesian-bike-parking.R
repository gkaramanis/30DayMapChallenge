library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = map_width, height = map_height, units = "in", dpi = 320)

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

frame <- st_bbox(centre)
frame_height <- (frame[["xmax"]] - frame[["xmin"]]) * map_height / map_width * 0.72
frame_mid <- mean(frame[c("ymin", "ymax")])

total <- sum(racks$capacity, na.rm = TRUE)

ggplot() +
  geom_sf(data = buildings, fill = clr_land, color = clr_bg, linewidth = 0.1) +
  geom_sf(data = roads, color = clr_road, linewidth = 0.3) +
  geom_sf(data = racks, aes(fill = known), color = NA) +
  scale_fill_manual(values = c("TRUE" = clr_accent2, "FALSE" = clr_accent), labels = c("TRUE" = "Capacity tagged", "FALSE" = "Capacity unknown, drawn as 2 bikes")) +
  coord_sf(xlim = frame[c("xmin", "xmax")], ylim = frame_mid + c(-1, 1) * frame_height / 2, expand = FALSE) +
  labs(
    title = "Every bike space\nin central Uppsala",
    subtitle = paste0(scales::number(total, big.mark = " "), " tagged spaces at true size,\neach drawn as 0.7 × 1.8 m of ground"),
    caption = caption_src("OpenStreetMap amenity=bicycle_parking"),
    fill = NULL
  ) +
  theme_map()

record_polaroid()
