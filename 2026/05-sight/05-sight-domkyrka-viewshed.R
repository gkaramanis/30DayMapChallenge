library(tidyverse)
library(sf)
library(terra)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = map_width, height = map_height, units = "in", dpi = 320)

# Lantmäteriet Markhöjdmodell Nedladdning, grid 1 (open data, order via geotorget.lantmateriet.se).
# Unzip the GeoTIFF tiles covering Uppsala into data/dem/
dem_files <- list.files(file.path(dir_data, "dem"), pattern = "\\.tif$", full.names = TRUE)
stopifnot("Put Lantmäteriet grid 1 tiles in data/dem" = length(dem_files) > 0)

# 5 m cells keep the viewshed fast enough over ~15 km
dem <- vrt(dem_files) |>
  aggregate(fact = 5, fun = "mean")

domkyrka <- st_sfc(st_point(c(17.6336, 59.8582)), crs = 4326) |>
  st_transform(crs(dem))

# Towers are 118.7 m; observer just below the spire tops
vis <- viewshed(dem, loc = st_coordinates(domkyrka)[1, ], observer = 100, target = 1.5)

city <- bb_sf(bb_city)
cycleways <- osm_cycleways() |> st_filter(city)
water <- osm_water()

frame <- st_bbox(city)
frame_sf <- st_as_sfc(frame)

# Share of each cycleway visible from the towers
cycleways$visible <- terra::extract(vis, vect(cycleways), fun = mean, na.rm = TRUE)[, 2]

vis_df <- vis |>
  crop(vect(frame_sf)) |>
  as.data.frame(xy = TRUE) |>
  setNames(c("x", "y", "visible")) |>
  filter(visible > 0)

ggplot() +
  geom_sf(data = frame_sf, fill = clr_land, color = NA) +
  geom_raster(data = vis_df, aes(x, y), fill = clr_accent, alpha = 0.4) +
  geom_sf(data = st_crop(water, frame_sf), fill = clr_water, color = NA) +
  geom_sf(data = cycleways, aes(color = visible), linewidth = 0.4) +
  geom_sf(data = domkyrka, shape = 24, size = 4, fill = clr_text, color = clr_bg, stroke = 0.8) +
  scale_color_gradient(low = colorspace::darken(clr_grid, 0.15), high = clr_accent2, labels = scales::percent) +
  coord_sf(xlim = frame[c(1, 3)], ylim = frame[c(2, 4)], expand = FALSE) +
  labs(
    title = "Seen from the cathedral",
    subtitle = "Ground visible from 100 m up the domkyrka towers\n(bare-earth model, no buildings),\nwith the share of each cycleway in view",
    caption = caption_src("Lantmäteriet Markhöjdmodell grid 1, OpenStreetMap"),
    color = "Share of cycleway in view"
  ) +
  theme_map() +
  theme(legend.key.width = unit(2, "cm"), legend.key.height = unit(0.35, "cm"))

record_polaroid()
