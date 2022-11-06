library(tidyverse)
library(sf)
library(camcorder)

gg_record(here::here("30daymap-temp"), width = 10, height = 6.2, dpi = 320)

# https://www.ifw-kiel.de/publications/data-sets/ukraine-support-tracker-data-17410/
colnames <- xlsx::read.xlsx(here::here("2022/data/Ukraine_Support_Tracker.xlsx"), sheetName = "Country Summary")

ukr_aid <- readxl::read_xlsx(here::here("2022/data/Ukraine_Support_Tracker.xlsx"), sheet = "Country Summary", skip = 2) %>% 
  janitor::clean_names() %>% 
  filter(!is.na(country)) %>% 
  select(1, 3:5) %>% 
  mutate(across(everything(), ~na_if(., 0))) %>% 
  mutate(country = if_else(country == "United States", "United States of America", country))

# https://datahub.io/core/geo-countries#resource-geo-countries_zip
world <- read_sf(here::here("2022/data/countries.geojson")) %>% 
  rmapshaper::ms_simplify(keep = 0.2)

world_ukr_aid <- world %>% 
  left_join(ukr_aid, by = c("ADMIN" = "country")) %>% 
  mutate(
    fill = case_when(
      !is.na(humanitarian_commitments) ~ "#0057b7",
      ADMIN == "Ukraine" ~ "#ffd700",
      TRUE ~ "grey97"
    ),
    color = case_when(
      !is.na(humanitarian_commitments) ~ "grey97",
      TRUE ~ "grey10"
    ),
  )

ggplot(world_ukr_aid) +
  geom_sf(aes(fill = fill, color = color), size = 0.05) +
  scale_fill_identity() +
  scale_color_identity() +
  coord_sf(crs = "+proj=moll") +
  labs(
    title = "Countries that have pledged humanitarian aid to Ukraine",
    caption = "Source: Kiel Institute for the World Economy · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = "Outfit") +
  theme(
    plot.background = element_rect(fill = "grey92", color = NA),
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(hjust = 0.5)
  )
