library(tidyverse)
library(sf)
library(janitor)

box <- c(xmin = 17.52, xmax = 17.75, ymin = 59.78, ymax = 59.9)

street_names <- read_sf(here::here("2020", "2-lines", "data", "Uppsala\ kommun\ Shape_GeoDatabas__3359", "Uppsala_kommun_ShapeNVDB_DKGatunamn.shp")) %>% 
  clean_names() %>% 
  st_transform(4326) %>% 
  st_crop(st_bbox(box))

street_endings <- street_names %>% 
  mutate(
    ending = case_when(
      endsWith(namn, "gatan") | endsWith(namn, "gata") ~ "-gatan",
      endsWith(namn, "vägen") |  endsWith(namn, "väg") ~ "-väg(en)",
      endsWith(namn, "leden") ~ "-leden",
      endsWith(namn, "splanaden") ~ "-splanaden",
      endsWith(namn, "stigen") ~ "-stigen",
      endsWith(namn, "leden") ~ "-leden",
      endsWith(namn, "resan") ~ "-resan",
      endsWith(namn, "plan") ~ "-plan",
      endsWith(namn, "backen") | endsWith(tolower(namn), "backe") ~ "backe",
      endsWith(namn, "allé") | endsWith(namn, "allén") ~ "allé",
      endsWith(namn, "torg")| endsWith(namn, "torgen") | endsWith(namn, "orget") ~ "torg",
      endsWith(namn, "gränd") ~ "gränd"
    )
  ) 

street_endings %>% 
  st_coordinates()

  ggplot(street_endings) +
  geom_sf(aes(color = ending)) +
  coord_sf(xlim = c(17.52, 17.82), ylim = c(59.75, 59.91))

