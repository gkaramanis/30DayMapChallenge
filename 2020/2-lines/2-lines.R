library(tidyverse)
library(sf)
library(janitor)

road_types <- read_sf(here::here("2020", "data", "Uppsala\ kommun\ Shape_GeoDatabas__3359", "Uppsala_kommun_ShapeNVDB_DKGatutyp.shp")) %>% 
  clean_names()

roads <- st_transform(road_types, 4326) %>% 
  mutate(
    typ = case_when(
      typ == "Huvudgata" ~ "Main street",
      typ == "Övergripande länk" ~ "Link",
      # typ == "Lokalgata stor" ~ "Big local street",
      TRUE ~ "Local street"
    )
  )

pal <- c("#0072b2", "#d55e00", "#009e73")

ggplot() +
  geom_sf(data = roads, aes(color = typ, size = typ)) +
	annotate("text", x = 17.76, y = 59.77, label = "UPPSALA", family = "Futura Condensed Medium", size = 16, color = "grey20") +
  annotate("text", x = 17.76, y = 59.76, label = "Source: Trafikverket | Graphic: Georgios Karamanis", family = "IBM Plex Sans Condensed", size = 2.5, color = "grey30", lineheight = 0.8) +
  coord_sf(xlim = c(17.52, 17.82), ylim = c(59.75, 59.91)) +
  scale_color_manual(breaks = c("Local street", "Main street", "Link"), values = pal) +
  scale_size_manual(breaks = c("Local street", "Main street", "Link"), values = c(0.23, 0.30, 0.38)) +
  theme_minimal(base_family = "Atkinson Hyperlegible") +
  theme(
    plot.background = element_rect(fill = "grey98", color = NA),
    legend.position = c(0.85, 0.35),
		legend.spacing.y = unit(0.7, 'lines'),
		legend.text = element_text(size = 11),
    legend.title = element_blank(),
		axis.title = element_blank()
  ) 

ggsave(here::here("2020", "2-lines", "2-lines.png"), dpi = 320, width = 7, height = 7)
