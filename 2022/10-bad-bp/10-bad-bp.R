library(tidyverse)
library(ggsn)
library(camcorder)

gg_record(here::here("30daymap-temp"), width = 12, height = 9, dpi = 320)

# Data: ncdrisc.org
bp <- read_csv(here::here("2022/data/NCD_RisC_Lancet_2016_BP_age_standardised_countries.csv")) %>% 
  janitor::clean_names() %>% 
  select(region = country_region_world, year, sex, prevalence = prevalence_of_raised_blood_pressure) %>% 
  filter(year == 2015, sex == "Men")

world <- map_data("world") %>% 
  group_by(region) %>% 
  mutate(
    c_x = median(long),
    c_y = median(lat)
  ) %>% 
  ungroup()

world_bp <- world %>% 
  left_join(bp)

ggplot(world_bp) +
  geom_map(map = world, aes(long, lat, map_id = region, fill = prevalence)) +
  geom_text(aes(c_x, c_y, label = paste0(region, ": ", round(prevalence, 3))), stat = "unique", family = "Comic Sans MS") +
  scale_fill_gradientn(colors = rainbow(6), na.value = "brown") +
  north(world) +
  labs(
    title = "Prevalence of raised blod pressure among men, 2015"
  ) +
  theme_bw(base_family = "Apple Chancery") +
  theme(
    plot.background = element_rect(fill = "pink", color = NA),
    plot.title = element_text(size = 30)
  )
  
  