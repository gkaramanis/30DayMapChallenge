library(marmap)
library(tidyverse)
library(camcorder)
library(sf)
library(rgeoboundaries)

gg_record(dir = "2021/temp", device = "png", width = 11.64, height = 12, units = "in", dpi = 320)

gr_sf <- geoboundaries("Greece")
tr_sf <- geoboundaries("Turkey")
al_sf <- geoboundaries("Albania")
mk_sf <- geoboundaries("North Macedonia")
bg_sf <- geoboundaries("Bulgaria")

countries <- rbind(gr_sf, tr_sf, al_sf, mk_sf, bg_sf)

bathy <- getNOAA.bathy(lon1 = 19, lon2 = 30, lat1 = 33, lat2 = 42, resolution = 1)

bathy_df <- fortify.bathy(bathy) %>% 
  filter(z < 0)

pal <- colorRampPalette(c("purple4", "navy", "blue", "skyblue", "lightblue", "white"))

f1 = "Porpora"

ggplot(bathy_df) +
  geom_tile(aes(x = x, y = y, fill = z)) +
  geom_sf(data = countries, fill = "#FFFBF8", color = "coral4", size = 0.25) +
  # Calypso Deep annotation
  geom_mark_circle(data = NULL, aes(21.133333, 36.566667, label = str_wrap("Calypso Deep is the deepest part of the Mediterranean Sea, with a maximum depth of 5 267 m (17 280 ft)", 22)), color = "chocolate1", con.colour = "chocolate1", con.size = 1, size = 1, n = 10, label.family = f1, label.fill = "#FFFBF8") +
  coord_sf(xlim = c(19, 30), ylim = c(33, 42), expand = FALSE) +
  annotate("text", 27.7, 33.3, label = "Data: NOAA · Graphic: Georgios Karamanis", family = f1, size = 5, color = "#FFFBF8") +
  scale_fill_gradientn(colors = pal(10), labels = function(x) format(-x, big.mark = " ", trim = TRUE)) +
  guides(fill = guide_colorbar(label.position = "left", title = "Depth (m)")) +
  theme_void(base_family = f1, base_size = 20) +
  theme(
    legend.position = c(0.9, 0.65),
    legend.key.height = unit(2.5, "line"),
    legend.key.width = unit(0.75, "line"),
    plot.background = element_rect(fill = "#FFFBF8", color = NA)
  )
  