library(elevatr)
library(rgeoboundaries)
library(tidyverse)
library(raster)
library(camcorder)
library(rayshader)

gg_record(dir = "2021/temp", device = "png", width = 10, height = 5, units = "in", dpi = 320)

greece <- gb_adm1("greece")

crete <- greece %>% 
  filter(shapeName == "Crete")

elev <- get_elev_raster(locations = crete, z = 9, clip = "locations")

cropped_elev <- crop(elev, crete)
cropped_aggr <- raster::aggregate(cropped_elev, fact = 5, fun = "mean") 

elev_df <- as.data.frame(cropped_aggr, xy = TRUE) %>% 
  rename(elevation = 3)

p <- ggplot(elev_df) +
  geom_sf(data = crete, color = NA, fill = NA) +
  geom_tile(aes(x, y, fill = elevation)) +
  scale_fill_viridis_c() +
  theme_void() +
  theme(
    legend.position = "none"
  )


plot_gg(p)
save_obj("2021/data/crete blender files/crete.obj")