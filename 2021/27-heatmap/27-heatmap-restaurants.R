library(tidyverse)
library(camcorder)

greece <- rgeoboundaries::gb_adm0("greece")
  
gr_restaurants <- oe_get(
  "Greece",
  layer = "points",
  query = "
  SELECT *, hstore_get_value(other_tags, 'amenity') AS amenity
  FROM points
  WHERE amenity = 'restaurant'
  "
)

gr_rest_df <- sfheaders::sf_to_df(gr_restaurants, fill = TRUE)

gg_record(dir = "2021/temp", device = "png", width = 10, height = 8.6, units = "in", dpi = 320)

f1 = "Porpora"
f2 = "General Sans"

ggplot(gr_rest_df) +
  stat_density2d(aes(x, y, fill = ..density..), geom = "tile", contour = FALSE, n = 250) +
  geom_sf(data = greece, fill = NA, color = "grey97", size = 0.1) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, size = 6, color = "black", fill = NA) +
  annotate("text", 29.2, 39.8, label = "Restaurants\nin\nGreece", family = f2, fontface = "bold", size = 12, color = "grey97", hjust = 1, lineheight = 0.85) +
  annotate("text", 29.2, 39.05, label = "Source: OpenStreetMap\nGraphic: Georgios Karamanis", family = f2, size = 3, color = "grey97", hjust = 1, lineheight = 0.95) +
  scale_fill_viridis_c(option = "turbo") +
  coord_sf(expand = FALSE) +
  labs(fill = "Density of\nrestaurants") +
  theme_void() +
  theme(
    legend.position = c(0.07, 0.13),
    legend.title = element_text(family = f1, color = "grey97"),
    legend.text = element_text(family = f1, color = "grey97"),
    plot.background = element_rect(fill = "#2C1439", color = NA),
    plot.caption = element_text(hjust = 0.5, color = "grey97", family = f1)
  )

