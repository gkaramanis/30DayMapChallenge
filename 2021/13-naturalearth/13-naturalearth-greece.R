library(rnaturalearth)
library(dplyr)
library(ggplot2)
library(sf)
library(rgeoboundaries)
library(rcartocolor)
library(camcorder)

gg_record(dir = "2021/temp", device = "png", width = 10, height = 10, units = "in", dpi = 320)

lat <- 30
lon <- 30
ortho <- paste0('+proj=ortho +lat_0=', lat, ' +lon_0=', lon,
                ' +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs')

greece <- gb_adm0(c("greece"))

relief_raw <- raster::raster("2021/Data/GRAY_50M_SR_OB.tif")

# Higher fact for fewer points = faster
relief <- raster::aggregate(relief_raw, fact = 3, fun = "mean")

greece_masked <- raster::mask(relief, greece)
greece_cropped <- raster::crop(greece_masked, greece)
raster::plot(greece_cropped)

greece_ortho <- raster::projectRaster(greece_cropped, crs = ortho, over = TRUE)

greece_ortho_df <- greece_ortho %>% 
  raster::rasterToPoints() %>% 
  as_tibble()

rel_ortho <- raster::projectRaster(relief, crs = ortho, over = TRUE)

rel_ortho_df <- rel_ortho %>% 
  raster::rasterToPoints() %>% 
  as_tibble()


ggplot() +
  geom_raster(data = rel_ortho_df, aes(x, y, color = GRAY_50M_SR_OB, fill = after_scale(colorspace::desaturate(color, 0.9)))) +
  geom_raster(data = greece_ortho_df, aes(x, y, fill = GRAY_50M_SR_OB)) +
  scale_color_carto_c(palette = "Sunset", direction = -1) +
  coord_fixed(clip = "off", expand = FALSE) +
  labs(
    caption = "Data: Natural Earth · Graphic: Georgios Karamanis"
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#201C33"),
    plot.margin = margin(15, 15, 15, 15),
    plot.caption = element_text(hjust = 0.5, family = "Porpora", color = "grey70", size = 6)
  )
