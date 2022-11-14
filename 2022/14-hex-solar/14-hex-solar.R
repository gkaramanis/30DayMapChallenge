library(tidyverse)
library(sf)
library(scico)
library(camcorder)

gg_record(here::here("30daymap-temp"), width = 12, height = 6, dpi = 320)

solar <- terra::rast(here::here("2022/data/World_TEMP_GISdata_LTAy_GlobalSolarAtlas-v2_GEOTIFF/TEMP.tif")) %>% 
  terra::aggregate(fact = 10)

solar_grid <- solar %>% 
  st_make_grid(c(1.5, 1.5), what = "polygons", square = FALSE) %>% 
  st_sf() %>% 
  mutate(id = row_number())

solar_extr <- terra::extract(solar, solar_grid) %>% 
  group_by(ID) %>% 
  summarise(value = median(TEMP, na.rm = TRUE))

solar_hex <- solar_grid %>% 
  left_join(solar_extr, by = c("id" = "ID")) %>% 
  filter(!is.na(value))

ggplot(solar_hex) +
  geom_sf(aes(fill = value), color = "brown4", linewidth = 0.025) +
  scale_fill_scico(palette = "roma", midpoint = 0, direction = -1, guide = guide_colorbar(title.position = "top", title.hjust = 0.5, title = "°C")) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Longterm yearly average of air temperature, 1994-2021",
    caption = "Source: Solargis · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = "Outfit") +
  theme(
    legend.position = "top",
    legend.key.height = unit(0.5, "lines"),
    legend.key.width = unit(2.5, "lines"),
    legend.margin = margin(0, 0, 10, 0),
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.title = element_text(hjust = 0.5, margin = margin(0, 0, 10, 0), face = "bold"),
    plot.caption = element_text(hjust = 0.5, margin = margin(20, 0, 0, 0)),
    plot.margin = margin(0, 10, 0, 10)
  )
  
