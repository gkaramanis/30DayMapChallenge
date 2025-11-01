library(tidyverse)
library(sf)
library(camcorder)

gg_record(here::here("30daymap-temp"), width = 11.42, height = 6.5, dpi = 320)

ror_csv <- read_csv(here::here("2025/data/v1.72-2025-10-06-ror-data_schema_v2.csv"))

ror <- ror_csv |>
  select(id, display = names.types.ror_display, lat = locations.geonames_details.lat, lon = locations.geonames_details.lng, geoname = locations.geonames_details.name)

ror_points <- ror |>
  group_by(lon, lat, geoname) |>
  summarise(n = n(), displays = paste(display, collapse = "; ")) |>
  ungroup() |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |> 
  filter(admin != "Antarctica") 

f1 <- "Inclusive Sans"
f2 <- "Publico Headline"

pal <- MetBrewer::met.brewer("Tam") |> 
  colorspace::darken()

col_bg <- "#f7f7f7"
col_country <- "#cccccc"
col_point_label <- "#22223b"
col_point_fill <- "white"

ggplot() +
  geom_sf(data = world, fill = "white", color = col_country, linewidth = 0.1) +
  geom_sf(data = ror_points %>% filter(n < 100), aes(color = n, size = n), alpha = 0.3) +
  geom_sf(data = ror_points %>% filter(n >= 100), aes(color = n, size = n), alpha = 0.8, shape = 21, fill = col_point_fill) +
  ggrepel::geom_text_repel(data = ror_points %>% filter(n >= 300), aes(label = geoname, geometry = geometry, size = n), color = col_point_label, stat = "sf_coordinates", segment.size = 0.15, min.segment.length = 0, max.overlaps = 50, seed = 99, box.padding = 0.5, bg.color = col_point_fill, bg.r = 0.15) +
  scale_color_gradientn(colors = pal) +
  scale_size_area(max_size = 3) +
  coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", expand = FALSE) +
  labs(
    title = "Research organizations of the world",
    subtitle = str_wrap("Points represent the locations of research organizations registered in the Research Organization Registry (ROR). The size of the points correspond to the number of organizations in each location.", 100),
    caption = "Source: Research Organization Registry (ROR) · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = col_bg, color = NA),
    plot.title = element_text(family = f2, size = 16, color = col_point_label, face = "bold"),
    plot.subtitle = element_text(size = 11, color = col_point_label, margin = margin(t = 5, b = 10)),
    plot.caption = element_text(size = 9, color = col_point_label),
    plot.margin = margin(5, 10, 5, 10)
  )
  
record_polaroid()
