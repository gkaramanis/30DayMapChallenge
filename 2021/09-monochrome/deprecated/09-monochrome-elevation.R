library(tidyverse)
library(raster)
library(camcorder)
library(rayshader)

gg_record(dir = "2021/temp", device = "png", width = 10.95, height = 9, units = "in", dpi = 320)

elevation_data <- raster("2021/data/GRC_alt/GRC_alt.gri") %>% 
  as.data.frame(xy = TRUE) %>% 
  filter(!is.na(GRC_alt))

p <- ggplot(elevation_data) +
  geom_tile(aes(x, y, fill = GRC_alt)) +
  scale_fill_gradient(low = "grey95", high = "black") +
  coord_fixed(xlim = c(19, 28.5), ylim = c(34, 41.8), expand = FALSE) +
  theme_void() +
  theme(
    legend.position = c(0.15, 0.2),
    plot.background = element_rect(fill = "white", color = NA)
  )

plot_gg(p, width = 10, height = 10, multicore = TRUE, preview = FALSE, scale = 200)

save_obj("~/Desktop/mono.obj")
