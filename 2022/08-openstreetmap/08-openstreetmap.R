library(tidyverse)
library(sf)
library(camcorder)

gg_record(here::here("30daymap-temp"), width = 16, height = 9, dpi = 320)

# # Download Shop from https://download.osmdata.xyz 
# shop <- read_sf("shop_EPSG4326.gpkg")
# 
# # Filter shops with Twitter or Facebook contact
# shop %>% 
#   mutate(
#     contact = case_when(
#       str_detect(other_tags, "contact:twitter") ~ "twitter",
#       str_detect(other_tags, "contact:facebook") ~ "facebook"
#     )
#   ) %>% 
#   filter(!is.na(contact)) %>% 
#   # Write file
#   write_sf(here::here("2022/data/osm_shop_contact.gpkg"))


# Data created with the commented code above
shop_contact <- read_sf(here::here("2022/data/osm_shop_contact.gpkg"))

globe <- st_point(x = c(0,0)) %>%
  st_buffer(dist = 6371000) %>%
  st_sfc(crs = "+proj=ortho +lat_0=53 +lon_0=14")

world <- read_sf(here::here("2022/data/ne_10m_coastline/ne_10m_coastline.shp"))

ggplot() +
  geom_sf(data = globe, fill = "white") +
  geom_sf(data = world, linewidth = 0.1) +
  geom_sf(data = shop_contact, aes(color = contact), size = 0.1) +
  scale_color_manual(values = c("#3c5a99", "#1DA1F2")) +
  coord_sf(crs = "+proj=ortho +lat_0=53 +lon_0=14") +
  facet_wrap(vars(contact)) +
  labs(
    caption = "Source: GeoNames · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = "Outfit") +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 14, face = "bold"),
    plot.background = element_rect(fill = "#EEF2F6", color = NA),
    plot.caption = element_text(hjust = 0.5, color = "darkblue"),
    plot.margin = margin(10, 0, 10, 0)
  )
