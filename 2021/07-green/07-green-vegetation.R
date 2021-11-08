library(tidyverse)
library(camcorder)
library(sf)
library(raster)
# library(MODIStsp)
library(rayshader)

gg_record(dir = "2021/temp", device = "png", width = 10, height = 10, units = "in", dpi = 320)

# The script is modified from:
# https://rspatialdata.github.io/vegetation.html

file <- "2021/data/gadm36_GRC_shp/gadm36_GRC_0.shp"
greece_sf <- read_sf(file)

# Download data, need to do it once
# MODIStsp(
#   gui = FALSE,
#   out_folder = "2021/data/VegetationData",
#   out_folder_mod = "VegetationData",
#   selprod = "Vegetation_Indexes_16Days_1Km (M*D13A2)",
#   bandsel = "NDVI",
#   user = "XXX",
#   password = "XXX",
#   start_date = "2020.06.01",
#   end_date = "2020.06.01",
#   verbose = FALSE,
#   spatmeth = "file",
#   spafile = file,
#   out_format = "GTiff"
# )

# Read in data
NDVI_raster <- raster(here::here("2021/data/VegetationData/gadm36_GRC_0/VI_16Days_1Km_v6/NDVI/MYD13A2_NDVI_2020_153.tif"))

# Transform data
NDVI_raster <- projectRaster(NDVI_raster, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Crop data
NDVI_raster <- raster::mask(NDVI_raster, as_Spatial(greece_sf))

# To get NDVI values from -1 to 1
gain(NDVI_raster) <- 0.0001

# Convert to data frame
NDVI_df <- as.data.frame(NDVI_raster, xy = TRUE, na.rm = TRUE)

f1 = "Porpora"

green_plot <- ggplot() +
  geom_sf(data = greece_sf, fill = NA, color = "darkgreen", size = 0.25) +
  geom_tile(data = NDVI_df, aes(x, y, fill = MYD13A2_NDVI_2020_153)) +
  labs(
    title = "Normalized difference vegetation index (NDVI)",
    # subtitle = "1970-2000",
    caption = "Source: NASA MODIS (2020-06-01) · Graphic: Georgios Karamanis"
  ) +
  scale_fill_viridis_c(name = "NDVI") +
  # guides(fill = guide_colorbar(label.position = "left")) +
  coord_sf(clip = "off") +
  theme_void(base_family = f1) +
  theme(
    legend.position = c(0.9, 0.65),
    legend.title = element_text(margin = margin(0, 0, 7, 0), size = 13),
    plot.background = element_rect(fill = "#F4F5FA", color = NA),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(hjust = 0.5, size = 12)
  )


plot_gg(green_plot, width = 10, height = 10, multicore = TRUE, preview = TRUE, anglebreaks = seq(60, 70, 0.1), zoom = 0.6, scale = 120)
