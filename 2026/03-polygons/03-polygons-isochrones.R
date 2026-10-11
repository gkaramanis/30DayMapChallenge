library(tidyverse)
library(sf)
library(camcorder)

# r5r needs Java 21 and a folder with one .osm.pbf and the GTFS zip(s)
options(java.parameters = "-Xmx6G")
library(r5r)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = map_width, height = map_height, units = "in", dpi = 320)

# Uppsala län extract, cut from Geofabrik's sweden-latest.osm.pbf with osmium:
# osmium extract -b 16.7,59.3,18.95,60.75 sweden-latest.osm.pbf -o uppsala.osm.pbf
dir_r5 <- file.path(dir_data, "r5")
dir.create(dir_r5, showWarnings = FALSE)
file.copy(ul_gtfs_path(), file.path(dir_r5, "ul.zip"), overwrite = FALSE)
stopifnot("Put uppsala.osm.pbf in data/r5" = file.exists(file.path(dir_r5, "uppsala.osm.pbf")))

r5 <- build_network(dir_r5)

origin <- data.frame(id = "centralen", lat = 59.8586, lon = 17.6459)
departure <- as.POSIXct(paste(typical_weekday(ul_gtfs()), "08:00:00"), tz = "Europe/Stockholm")

iso <- map(c("BICYCLE", "TRANSIT"), function(m) {
  isochrone(
    r5,
    origins = origin,
    mode = if (m == "TRANSIT") c("WALK", "TRANSIT") else m,
    departure_datetime = departure,
    cutoffs = c(15, 30, 45),
    polygon_output = TRUE
  ) |>
    mutate(mode = if (m == "TRANSIT") "Bus and walk" else "Bike")
}) |>
  bind_rows() |>
  st_transform(crs_se) |>
  arrange(desc(isochrone))

water <- osm_water(bb_kommun, "water-kommun")
kommun <- uppsala_kommun() |> st_transform(crs_se)
origin_sf <- st_as_sf(origin, coords = c("lon", "lat"), crs = 4326) |> st_transform(crs_se)
centre <- st_coordinates(origin_sf)

ggplot() +
  geom_sf(data = kommun, fill = clr_land, color = clr_grid, linewidth = 0.3) +
  geom_sf(data = iso, aes(fill = factor(isochrone)), color = clr_bg, linewidth = 0.2) +
  geom_sf(data = st_crop(water, kommun), fill = clr_water, color = NA) +
  geom_sf(data = origin_sf, shape = 21, fill = clr_accent, color = clr_text, size = 3, stroke = 0.8) +
  scale_fill_manual(values = c(
    "15" = clr_accent2,
    "30" = colorspace::lighten(clr_accent2, 0.45),
    "45" = colorspace::lighten(clr_accent2, 0.8)
  )) +
  facet_wrap(vars(mode), ncol = 1) +
  coord_sf(xlim = centre[1] + c(-24000, 24000), ylim = centre[2] + c(-11500, 11500), expand = FALSE) +
  labs(
    title = "How far you get\nfrom Uppsala C",
    subtitle = "Area reachable in 15, 30 and 45 minutes,\nleaving at 08:00 on a weekday",
    caption = caption_src("UL GTFS via Trafiklab, OpenStreetMap, r5r"),
    fill = "Minutes"
  ) +
  theme_map() +
  theme(legend.margin = margin(b = 14), strip.text = element_text(family = f2, face = "bold", size = 20, margin = margin(b = 4)))

record_polaroid()

stop_r5(r5)
