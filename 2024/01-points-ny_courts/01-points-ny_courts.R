library(tidyverse)
library(sf)
library(camcorder)

gg_record(here::here("30daymap-temp/"), width = 8, height = 8, dpi = 320)

# https://catalog.data.gov/dataset/directory-of-basketball-courts
bbnyc <- jsonlite::fromJSON("https://www.nycgovparks.org/bigapps/DPR_Basketball_001.json") %>% 
  mutate(
    # there is one positive lon, all should be negative
    lon = -abs(as.numeric(lon)), 
    lat = as.numeric(lat)
  )

# https://github.com/codeforgermany/click_that_hood/tree/main/public/data
nyc <- read_sf(here::here("2024/data/new-york-city-boroughs-ny_.geojson"))

# https://www.nyc.gov/site/planning/data-maps/open-data/dwn-digital-city-map.page
nyc_streets <- read_sf("https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/DCM_Arterials_and_Major_Streets/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=pgeojson")

us <- rnaturalearth::ne_countries(country = "United States of America", scale = 10) %>% 
  st_crop(nyc)

f1 <- "Phosphate"
f2 <- "Futura"

ggplot() +
  geom_sf(data = us, color = "grey30", fill = "grey12") +
  geom_sf(data = nyc, color = "grey50", fill = "grey20") +
  geom_sf(data = nyc_streets, color = "grey65", linewidth = 0.1) +
  geom_point(data = bbnyc, aes(lon, lat), color = "grey5", size = 4, shape = "★", alpha = 0.9) +
  geom_point(data = bbnyc, aes(lon, lat), color = "#FF4B4B", size = 2.5, shape = "★", alpha = 0.9) +
  ggtext::geom_textbox(data = NULL, aes(label = "New York\nBasketball\nC<span style='color:#FF4B4B'>✪</span>urts", x = -74.24, y = 40.9), vjust = 1, hjust = 0, lineheight = 0.9, text.color = "white", color = NA, family = f1, size = 18, fill = NA, width = 0.5) +
  coord_sf(expand = FALSE) +
  labs(
    caption = "Sources: DATA.GOV, NYC.GOV & Click that 'hood · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "grey10", color = NA),
    plot.caption = element_text(size = 9, color = "white", family = f2),
    plot.margin = margin(10, 10, 10, 10)
  )
  
