library(tidyverse)
library(sf)
library(camcorder)
library(patchwork)

gg_record(here::here("30daymap-temp"), width = 24, height = 9, dpi = 320)

world <- read_sf(here::here("2022/data/countries.geojson")) %>% 
  janitor::clean_names() %>% 
  rmapshaper::ms_simplify(keep = 0.2)

# Data: https://ourworldindata.org/grapher/tomato-production
tomato_prod <- read_csv("2022/data/tomato-production.csv") %>% 
  janitor::clean_names() %>% 
  rename("tonnes" = last_col()) %>% 
  filter(year == 2020)

tomato_world <- world %>% 
  left_join(tomato_prod, by = c("iso_a3" = "code"))

# Code from https://wilkelab.org/practicalgg/articles/Winkel_tripel.html
crs_wintri <- "+proj=wintri +datum=WGS84 +no_defs +over"

tomato_world_wintri <- lwgeom::st_transform_proj(tomato_world, crs = crs_wintri)

grat_wintri <- st_graticule(lat = c(-89.9, seq(-80, 80, 20), 89.9)) %>%
  lwgeom::st_transform_proj(crs = crs_wintri)

lats <- c(90:-90, -90:90, 90)
longs <- c(rep(c(180, -180), each = 181), 180)

wintri_outline <- 
  list(cbind(longs, lats)) %>%
  st_polygon() %>%
  st_sfc(
    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  ) %>% 
  st_sf() %>%
  lwgeom::st_transform_proj(crs = crs_wintri) # transform to Winkel tripel

pal <- RColorBrewer::brewer.pal(9, "OrRd")[1:8]

# Plot code
p <- ggplot(tomato_world_wintri) +
  geom_sf(data = wintri_outline, fill = "#56B4E950", color = NA) +
  geom_sf(data = grat_wintri, color = "darkslateblue", linewidth = 0.15) +
  geom_sf(aes(fill = tonnes)) +
  coord_sf(datum = NULL) +
  guides(fill = guide_colorsteps(title.position = "top", title.hjust = 0.5, title = "Tomato production in tonnes, 2020")) +
  labs(caption = "Source: Our World in Data · Graphic: Georgios Karamanis") +
  theme_void(base_family = "Outfit") +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 16),
    legend.key.width = unit(3.5, "lines"),
    legend.key.height = unit(0.5, "lines"),
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.margin = margin(0, 0, 0, 0),
    plot.caption = element_text(hjust = 0.5, size = 11)
  )

# Log color scale
m1 <- p +
  scale_fill_stepsn(colors = pal, na.value = "grey97", breaks = c(0, 1e5, 5e5, 1e6, 2.5e6, 5e6, 65e6), labels = scales::label_number(suffix = " M", scale = 1e-6), trans = "log") 
  
# Non-log scale
m2 <- p +
  scale_fill_stepsn(colors = pal, na.value = "grey97", n.breaks = 6, labels = scales::label_number(suffix = " M", scale = 1e-6)) 

m1 + m2 +
  plot_annotation(
    theme = theme_void()
  )
