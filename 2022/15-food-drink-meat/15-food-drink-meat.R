library(tidyverse)
library(sf)
library(biscale)
library(patchwork)
library(ggtext)
library(camcorder)

gg_record(here::here("30daymap-temp"), width = 12, height = 9, dpi = 320)

world <- read_sf(here::here("2022/data/world.geo.json")) %>% 
  select(admin, sovereignt, adm0_a3, pop_est)

meat_prod <- read_csv(here::here("2022/data/global-meat-production.csv")) %>% 
  janitor::clean_names() %>% 
  rename(tonnes = 4) %>% 
  filter(year == 2017 & !is.na(code))

meat_consum <- read_csv(here::here("2022/data/per-capita-meat-consumption-by-type-kilograms-per-year.csv")) %>% 
  janitor::clean_names() %>% 
  filter(year == 2017 & !is.na(code)) %>% 
  mutate(across(4:8, ~ replace_na(.x, 0))) %>% 
  mutate(total_kg_percapita = Reduce("+", .[4:8])) %>% 
  select(1:3, last_col())

meat <- meat_prod %>% 
  left_join(meat_consum) %>% 
  filter(!is.na(total_kg_percapita))

world_bi <- world %>% 
  left_join(meat, by = c("adm0_a3" = "code")) %>% 
  mutate(prod_kg_percapita = tonnes / pop_est * 1000) %>% 
  bi_class(., x = total_kg_percapita, y = prod_kg_percapita, style = "quantile", dim = 3)

pal <- "DkViolet2"
f1 <- "Outfit"

p_map <- ggplot(world_bi) +
  geom_sf(aes(fill = bi_class), color = "white", linewidth = 0.15) +
  annotate("richtext", 0, Inf, label = "<span style='color:#4F78AC'>**Production**</span> and <span style='color:#923D48'>**consumption**</span> in kg per capita", family = f1, fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt")) +
  bi_scale_fill(pal = pal, dim = 3, na.value = "#29621860") +
  labs(
    title = "Meat production and consumption by country, 2017",
    caption = "Source: Our World In Data · Graphic: Georgios Karamanis"
  ) +
  coord_sf(clip = "off", ylim = c(-60, 85)) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(0, 0, 10, 0)),
    plot.caption = element_text(hjust = 0.5),
    plot.margin = margin(20, 0, 20, 0)
  )
  

p_legend <- bi_legend(pal = pal, dim = 3, size = 8,
                    xlab = "Higher consumption ",
                    ylab = "Higher production ",
                    base_family = f1
                    )
  
p_lab <- world_bi %>% 
  arrange(admin) %>% 
  mutate(i = row_number()) %>% 
  ggplot() +
  geom_richtext(aes((i - 1) %/% 35, (i - 1) %% 35,
                    label = paste0("**", admin, "**  ",
                                   "<span style='color:#4F78AC'>", round(prod_kg_percapita, 1), "</span>",
                                   ", ",
                                   "<span style='color:#923D48'>", round(total_kg_percapita, 1), "</span>"
                                   )), hjust = 0, size = 2, family = f1, fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt")) +
  xlim(0, 4.5) +
  scale_y_reverse() +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA)
  )
  
p_map / p_lab +
  plot_layout(heights = c(1, 0.8)) +
  inset_element(p_legend, 0.05, 1, 0.2, 2)

