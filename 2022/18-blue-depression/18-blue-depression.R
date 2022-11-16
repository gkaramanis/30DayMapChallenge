library(tidyverse)
library(cartogram)
library(sf)
library(camcorder)

gg_record(here::here("30daymap-temp"), width = 12, height = 9, dpi = 320)

url <- "https://en.wikipedia.org/wiki/Epidemiology_of_depression"

daly <- read_csv(here::here("2022/data/dalys-depression-age-std-rate.csv")) %>% 
  janitor::clean_names() %>% 
  rename(daly = 4) %>% 
  filter(year == 2019)

world <- read_sf(here::here("2022/data/world.geo.json")) %>% 
  select(admin, adm0_a3) %>% 
  st_transform(crs = 3857) %>% 
  mutate(adm0_a3 = case_when(
    adm0_a3 == "PSX" ~ "PSE",
    adm0_a3 == "SDS" ~ "SSD",
    TRUE ~ adm0_a3
  ))

world_daly <- world %>% 
  left_join(daly, by = c("adm0_a3" = "code"))

world_daly_cart <- cartogram_cont(world_daly, "daly", itermax = 10)

f1 <- "Outfit"

ggplot(world_daly_cart %>% mutate(lab_c = if_else(daly < 700, "#301934", "white"))) +
  geom_sf(aes(fill = daly), color = "black", linewidth = 0.1) +
  geom_sf_text(aes(label = str_wrap(admin, 10), color = lab_c), family = f1, check_overlap = TRUE, size = 2, lineheight = 0.9) +
  scale_color_identity() +
  # scale_fill_fermenter(direction = 1) +
  scico::scale_fill_scico(palette = "oslo", direction = -1, begin = 0.15, limits = c(250, 1250)) +
  labs(
    title = "Rate of disease burden from depression, 2019",
    subtitle = str_wrap("Disease burden measured in Disability-Adjusted Life Years (DALYs) per 100,000 individuals. DALYs are used to measure total burden of disease - both from years of life lost and years lived with a disability. One DALY equals one lost year of healthy life.", 90),
    caption = "Source: Our World in Data · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    legend.position = "top",
    legend.key.width = unit(3, "lines"),
    legend.key.height = unit(0.5, "lines"),
    legend.title = element_blank(),
    legend.margin = margin(10, 0, 0, 0)
  )
  
