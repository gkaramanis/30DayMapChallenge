library(tidyverse)
library(sf)
library(terra)
library(camcorder)

options(java.parameters = "-Xmx6G")
library(r5r)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = 9, height = 9, dpi = 320)

# Same r5r folder as day 3: data/r5 with uppsala.osm.pbf and ul.zip
dir_r5 <- file.path(dir_data, "r5")
file.copy(ul_gtfs_path(), file.path(dir_r5, "ul.zip"), overwrite = FALSE)
stopifnot("Put uppsala.osm.pbf in data/r5" = file.exists(file.path(dir_r5, "uppsala.osm.pbf")))
r5 <- build_network(dir_r5)

kommun <- uppsala_kommun() |> st_transform(crs_se)

# 250 m pixels over the kommun
template <- rast(ext(vect(kommun)), resolution = 250, crs = paste0("EPSG:", crs_se))
cells <- as_tibble(xyFromCell(template, seq_len(ncell(template)))) |>
  mutate(id = as.character(row_number())) |>
  st_as_sf(coords = c("x", "y"), crs = crs_se, remove = FALSE) |>
  st_filter(kommun)

dest <- cells |>
  st_transform(4326) |>
  mutate(lon = st_coordinates(geometry)[, 1], lat = st_coordinates(geometry)[, 2]) |>
  st_drop_geometry() |>
  select(id, lon, lat)

origin <- data.frame(id = "centralen", lat = 59.8586, lon = 17.6459)
departure <- as.POSIXct(paste(typical_weekday(ul_gtfs()), "08:00:00"), tz = "Europe/Stockholm")

tt <- travel_time_matrix(
  r5,
  origins = origin,
  destinations = dest,
  mode = c("WALK", "TRANSIT"),
  departure_datetime = departure,
  max_trip_duration = 120
)

cells <- cells |>
  st_drop_geometry() |>
  left_join(tt |> select(id = to_id, minutes = travel_time_p50), by = "id")

water <- osm_water(bb_kommun, "water-kommun")

ggplot() +
  geom_raster(data = cells |> filter(!is.na(minutes)), aes(x, y, fill = minutes)) +
  geom_sf(data = st_crop(water, kommun), fill = "white", color = NA) +
  geom_sf(data = kommun, fill = NA, color = "grey30", linewidth = 0.2) +
  scale_fill_stepsn(colors = MetBrewer::met.brewer("Hiroshige", direction = -1), breaks = seq(0, 120, 15), limits = c(0, 120)) +
  coord_sf(crs = crs_se, expand = FALSE) +
  labs(
    title = "Minutes from Uppsala C",
    subtitle = "Travel time by bus and on foot, leaving at 08:00 on a weekday, in 250 m pixels",
    caption = caption_src("UL GTFS via Trafiklab, OpenStreetMap, r5r"),
    fill = "Minutes"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "right",
    legend.key.height = unit(3, "lines"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(family = f2, size = 20, face = "bold"),
    plot.subtitle = element_text(size = 10, margin = margin(5, 0, 10, 0)),
    plot.caption = element_text(size = 8),
    plot.margin = margin(10, 10, 10, 10)
  )

record_polaroid()

stop_r5(r5)
