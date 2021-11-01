library(tidyverse)
library(sf)

heat <- read_sf(here::here("2021", "data", "vathmohmeresthermanshs", "vathmohmeres_thermanshs", "vathmohmeres_thermanshs.shp"))

heat15 <- heat %>%
  select(TOT15, geometry)

ggplot(heat15) +
  geom_sf(aes(fill = TOT15), color = NA, size = 1) +
  scale_fill_viridis_c(option = "plasma") +
  labs(
    title = "Total heating degree days",
    subtitle = "Base temperature 15 °C. Measurements from 47 weather stations 1997-2002.",
    caption = "Data: cres.gr · Graphic: Georgios Karamanis"
    ) +
  theme_void(base_family = "Futura") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(color = "grey95"),
    legend.title = element_blank(),
    legend.key.height = unit(0.8, "lines"),
    plot.background = element_rect(fill = "#79A7B4", color = NA),
    plot.title = element_text(size = 26, color = "grey95"),
    plot.subtitle = element_text(size = 18, color = "grey95"),
    plot.caption = element_text(size = 12, color = "grey95"),
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave(here::here("2021", "01-points", "01-points-heating.png"), width = 12, height = 12, dpi = 320)
