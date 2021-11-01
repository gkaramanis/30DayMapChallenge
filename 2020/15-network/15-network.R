library(sf)
library(tidyverse)
library(janitor)
library(maps)
library(sfheaders)


sweden <- map_data(map = "world") %>% 
  filter(region == "Sweden") %>% 
  sf_polygon(x = "long", y = "lat", polygon_id = "subregion" , keep = TRUE) %>% 
  st_set_crs(4326)

railroads <- read_sf(here::here("2020", "data", "sve_1milj_Sweref_99_TM_shape", "svk", "riks", "jl_riks.shp"), options = "ENCODING=WINDOWS-1252") %>% 
  clean_names() %>% 
  st_transform(4326) %>% 
  mutate(
    track_size = case_when(
      str_detect(kategori, "enkel") ~ 0.1,
      str_detect(kategori, "dubbel") ~ 0.6,
      TRUE ~ 0.125
    )
  )

railroads <- st_intersection(sweden, railroads)

ggplot(railroads) +
  geom_sf(data = sweden, fill = "grey25", color = NA) +
  geom_sf(aes(size = track_size), color = "goldenrod1") +
	geom_sf(data = subset(railroads, track_size == 0.6), size = 0.4, color = "grey25") +
  scale_size_identity() +
  theme_void() +
	theme(
	  plot.background = element_rect(fill = "grey15", color = NA)
	) 

ggsave(here::here("2020", "15-network", "15-network.png"), dpi = 320, height = 8, width = 3.546)
