library(tidyverse)
library(readxl)
library(janitor)
library(scales)

se_names_xl <- read_excel("2020/data/mi0810_2018a01_tatorter2018_bef_arealer_200827.xlsx", skip = 9) %>% 
  clean_names()

ggplot(se_names_xl) +
  geom_point(aes(x_koordinat_sweref_99tm, y_koordinat_sweref_99tm, size = folkmangd_2018_12_31, alpha = befolknings_tathet_2018_antal_invanare_km2), fill = "grey30", stroke = 0) +
  annotate("text", x = 200000, y = 7600000, label = "SWEDEN", family = "Futura Condensed Medium", size = 16, color = "grey20", hjust = 0) +
  annotate("text", x = 920000, y = 6130000, label = "Source: SCB\nGraphic: Georgios Karamanis", family = "IBM Plex Sans Condensed", size = 3, color = "grey30", hjust = 1, lineheight = 0.9) +
  xlim(200000, 920000) +
  scale_size_continuous(range = c(0.7, 5), name = "Pop. size", labels = comma) +
  scale_alpha_continuous(range = c(0.2, 1), name = "Pop. density (per km²)", labels = comma) +
  # coord_fixed() +
  theme_void() +
  theme(
    legend.position = c(0.23, 0.78),
    legend.text = element_text(size = 10, family = "Futura Medium", color = "grey35"),
    legend.title = element_text(size = 10, family = "Futura Bold", color = "grey35"),
    plot.background = element_rect(color = "grey30", fill = "grey97", size = 2),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  ggsave(here::here("2020", "1-points", "1-points.png"), dpi = 320, height = 10, width = 5.2)
