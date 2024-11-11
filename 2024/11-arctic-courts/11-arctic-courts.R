library(tidyverse)
library(sf)
library(osmdata)
library(grid)
library(ggpp)
library(camcorder)

gg_record(here::here("30daymap-temp/"), width = 8, height = 8, dpi = 320)

arctic_bb <- opq(bbox = c(-180, 66.5, 180, 90)) %>% 
  add_osm_feature(key = "sport", value = "basketball") %>% 
  osmdata_sf()

world <- rnaturalearth::ne_countries(scale = 50)

arctic_circle <- tibble(
  lon = seq(-180, 180, length.out = 60),
  lat = rep(66.5, 60)
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("LINESTRING")

graticules <- st_graticule(
  lat = seq(60, 90, by = 10),
  lon = seq(-180, 180, by = 20),
  crs = 4326
)

f1 <- "Charter"

shadow_grob <- grid.circle(gp = gpar(col = NA, fill = radialGradient(colours = c(NA, "#7D94B0"), stops = c(0.15, 1))))

shadow_df <- data.frame(x = 0, y = 0) %>% 
  mutate(grob = list(shadow_grob))

ggplot() +
  geom_sf(data = graticules, color = "#E2E6ED", linewidth = 0.35) +
  geom_grob(data = shadow_df, aes(x, y, label = grob), vp.height = 1.42, vp.width = 1.42) +
  geom_sf(data = world, fill = "#EEF2F7", color = "#52687D", linewidth = 0.1) +
  geom_sf(data = arctic_bb$osm_points, color = "#2C5784", size = 2, alpha = 0.5) +
  geom_sf(data = arctic_circle, fill = NA, color = "#7D94B0", linetype = "dashed") +
  ggtext::geom_richtext(data = NULL, aes(0, 0, label = "Arctic hoops:<br>Basketball courts beyond the circle<br><span style='font-size:13px'>Source: OpenStreetMap · Graphic: Georgios Karamanis</span>"), family = f1, color = "#C36D3E", size = 7, fontface = "bold", lineheight = 1, fill = NA, label.size = 0) +
  coord_sf(
    crs = "+proj=ortho +lat_0=90 +lon_0=0 +x_0=0 +y_0=0",
    xlim = c(-3000000, 3000000),
    ylim = c(-3000000, 3000000)
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.background = element_rect(fill = "#F7FAFF", color = NA)
  )

