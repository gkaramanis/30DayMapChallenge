library(tidyverse)
library(raster)
library(camcorder)
library(rgeoboundaries)
library(sf)

gg_record(dir = "2021/temp", device = "png", width = 10, height = 10, units = "in", dpi = 320)

# Modified the script at:
# https://rspatialdata.github.io/temperature.html

# Download data first:
# bio_data <- getData(name = "worldclim", var = "bio", res = 2.5)

# Read in data
bio_data <- raster("2021/data/wc2-5/bio5.bil")

# Temperature to Celcius
gain(bio_data) <- 0.1

# Max Temperature of Warmest Month
warmest <- bio_data$bio5

# Greece sf
greece_sf <- geoboundaries("Greece")

# Mask data
warmest_gr <- raster::mask(warmest, as_Spatial(greece_sf))

# Raster object to dataframe
warmest_gr_df <- as.data.frame(warmest_gr, xy = TRUE, na.rm = TRUE)

f1 = "Futura"

ggplot() +
  geom_sf(data = greece_sf, fill = "moccasin", color = "moccasin", size = 1) +
  geom_tile(data = warmest_gr_df, aes(x, y, fill = bio5)) +
  labs(
    title = "Max temperature of warmest month",
    subtitle = "1970-2000",
    caption = "Source: Wordclim · Graphic: Georgios Karamanis"
    ) +
  scale_fill_gradient(name = "Temperature (°C)", low = "khaki1", high = "orangered2") +
  guides(fill = guide_colorbar(label.position = "left")) +
  coord_sf(clip = "off") +
  theme_void(base_family = f1) +
  theme(
    legend.position = c(0.9, 0.65),
    legend.title = element_text(margin = margin(0, 0, 7, -45), size = 13),
    plot.background = element_rect(fill = "#F4F5FA", color = NA),
    plot.title = element_text(hjust = 0.5, size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(hjust = 0.5, size = 12)
  )
