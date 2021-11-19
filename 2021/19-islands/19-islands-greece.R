library(sf)
library(tidyverse)
library(camcorder)
library(shadowtext)

gg_record(dir = "2021/temp", device = "png", width = 10, height = 8.5, units = "in", dpi = 320)

greece <- rgeoboundaries::gb_adm0("greece")
greece_poly <- st_cast(greece, "POLYGON")

neighbors <- rgeoboundaries::gb_adm0(c("turkey", "albania"))
  
greece_poly_area <- greece_poly %>% 
  mutate(
    area = st_area(greece_poly),
    area = as.numeric(str_remove(area, " [m^2]"))
  )  

islands <- greece_poly_area %>% 
  filter(area < 2e+10) %>% 
  mutate(i = row_number()) %>% 
  filter(i != 125)

mainland <- greece_poly_area %>% 
  filter(area > 2e+10)

mainland_buffer <- st_buffer(mainland, 500)
neighbors_buffer <- st_buffer(neighbors, 500)

greece_ferries <- read_sf(here::here("2021", "data", "greece_ferries", "greece_ferries.shp")) %>% 
  mutate(
    l = st_length(geometry),
    l = as.numeric(str_remove(l, " [m]"))
    ) %>% 
  filter(str_detect(name, "Bari|Ancona|Venice|Venezia|Brindisi", negate = TRUE)) %>% 
  filter(l < 5e+05) %>%
  mutate(mainland_intersect = lengths(st_intersects(., neighbors_buffer)) > 0) %>% 
  mutate(neighbors_intersect = lengths(st_intersects(., mainland_buffer)) > 0) %>% 
  filter(!mainland_intersect) %>% 
  filter(!neighbors_intersect)

f1 = "General Sans"

ggplot() +
  geom_sf(data = islands, size = 0.3, fill = "linen", color = colorspace::darken("azure3", 0.5)) +
  geom_sf(data = greece_ferries, size = 0.35, color = "darkblue", alpha = 0.7) +
  labs(
    title = "    Ferry routes\n    between\n    Greek islands",
    caption = "Data: geoBoundaries & OpenStreetMap · Graphic: Georgios Karamanis"
    ) +
  coord_sf(expand = FALSE) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "azure3", color = NA),
    plot.title = element_shadowtext(size = 28, color = "white", face = "bold", margin = margin(0, 0, 20, 0)),
    plot.caption = element_shadowtext(size = 10, color = "white", face = "bold", hjust = 0.95, margin = margin(15, 0, 0 , 0))
  )

