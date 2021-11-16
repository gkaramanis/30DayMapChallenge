library(sf)
library(tidyverse)
library(camcorder)

gg_record(dir = "2021/temp", device = "png", width = 10, height = 9, units = "in", dpi = 320)

dgurba_2020 <- read_sf("2021/data/DGURBA-2020-01M-SH/DGURBA-2020-01M-SH.shp") %>% 
  filter(CNTR_CODE == "EL")

dgurba_2018 <- read_sf("2021/data/DGURBA-2018-01M-SH/DGURBA_2018_01M.shp") %>% 
  filter(str_detect(GISCO_ID, "^EL")) %>% 
  select(GISCO_ID, DGURBA18 = DGURBA, COASTAL)

st_geometry(dgurba_2018) <- NULL

dgurba_2018_2020 <- dgurba_2020 %>%
  left_join(dgurba_2018) %>% 
  mutate(
    dgurba_diff = factor(DGURBA - DGURBA18),
    change = case_when(
      DGURBA18 == 3 & DGURBA == 2 ~ "Towns and suburbs → Cities",
      DGURBA18 == 2 & DGURBA == 3 ~ "Cities → Towns and suburbs",
      DGURBA18 == DGURBA ~ "Unchanged"
      )
    )


f1 = "Piazzolla"
f2 = "Piazzolla SC"

ggplot(dgurba_2018_2020) +
  geom_sf(aes(fill = change, color = after_scale(colorspace::darken(fill, 0.2))), size = 0.1) +
  annotate("text", 27.37, 39.7, label = str_wrap("For local administrative units, 2018-2021 (provisional data). Classification:  Cities (densely populated areas), towns and suburbs (intermediate density areas), rural areas (thinly populated areas)", 33), hjust = 0, vjust = 1, family = f1, fontface = "bold") +
  scale_fill_manual(breaks = sort(unique(dgurba_2018_2020$change)), values = c("#34B3CB", "#CB4C34", "white")) +
  labs(
    title = "Changes in degree of urbanisation",
    caption = "Source: Eurostat · Graphic: Georgios Karamanis"
    ) +
  theme_void(base_family = f1, base_size = 12) +
  theme(
    legend.position = c(0.87, 0.8),
    legend.title = element_blank(),
    legend.text = element_text(size = 11, face = "bold"),
    plot.background = element_rect(fill = "#b7ccc8", color = NA),
    plot.title = element_text(size = 25, hjust = 0.5, family = f2, face = "bold"),
    plot.caption = element_text(hjust = 0, face = "bold"),
    plot.margin = margin(20, 10, 20, 10)
  )
