library(tidyverse)
library(sf)
library(camcorder)

# r5r needs Java 21 and a folder with one .osm.pbf and the GTFS zip(s)
options(java.parameters = "-Xmx6G")
library(r5r)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = 10, height = 6, dpi = 320)

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

ggplot() +
  geom_sf(data = kommun, fill = "white", color = "grey70", linewidth = 0.2) +
  geom_sf(data = iso, aes(fill = factor(isochrone)), color = "white", linewidth = 0.2) +
  geom_sf(data = st_crop(water, kommun), fill = "#cfe3ee", color = NA) +
  scale_fill_manual(values = MetBrewer::met.brewer("Hiroshige", 3, direction = -1)) +
  facet_wrap(vars(mode)) +
  coord_sf(expand = FALSE) +
  labs(
    title = "How far you get from Uppsala C",
    subtitle = "Area reachable in 15, 30 and 45 minutes, leaving at 08:00 on a weekday",
    caption = caption_src("UL GTFS via Trafiklab, OpenStreetMap, r5r"),
    fill = "Minutes"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "top",
    plot.background = element_rect(fill = "#f7f5f0", color = NA),
    strip.text = element_text(size = 12, face = "bold", margin = margin(0, 0, 5, 0)),
    plot.title = element_text(family = f2, size = 18, face = "bold"),
    plot.subtitle = element_text(size = 11, margin = margin(5, 0, 10, 0)),
    plot.caption = element_text(size = 8),
    plot.margin = margin(10, 10, 10, 10)
  )

record_polaroid()

stop_r5(r5)
