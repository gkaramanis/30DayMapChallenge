library(tidyverse)
library(sf)
library(terra)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = 9, height = 9, dpi = 320)

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

# Share of each cycleway visible from the towers
cycleways$visible <- terra::extract(vis, vect(cycleways), fun = mean, na.rm = TRUE)[, 2]

vis_df <- vis |>
  crop(vect(city)) |>
  as.data.frame(xy = TRUE) |>
  setNames(c("x", "y", "visible")) |>
  filter(visible > 0)

col_bg <- "#f4f1ea"

ggplot() +
  geom_raster(data = vis_df, aes(x, y), fill = "#f2c14e", alpha = 0.35) +
  geom_sf(data = cycleways, aes(color = visible), linewidth = 0.3) +
  geom_sf(data = domkyrka, shape = 24, size = 3, fill = "#7a1f3d", color = "white") +
  scale_color_gradient(low = "grey75", high = "#7a1f3d", labels = scales::percent) +
  coord_sf(xlim = st_bbox(city)[c(1, 3)], ylim = st_bbox(city)[c(2, 4)], expand = FALSE) +
  labs(
    title = "Seen from the cathedral",
    subtitle = "Ground visible from 100 m up the domkyrka towers (bare-earth model, no buildings), with the share of each cycleway in view",
    caption = caption_src("Lantmäteriet Markhöjdmodell grid 1, OpenStreetMap"),
    color = "Visible"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "top",
    plot.background = element_rect(fill = col_bg, color = NA),
    plot.title = element_text(family = f2, size = 18, face = "bold"),
    plot.subtitle = element_text(size = 10, margin = margin(5, 0, 10, 0)),
    plot.caption = element_text(size = 8),
    plot.margin = margin(10, 10, 10, 10)
  )

record_polaroid()
