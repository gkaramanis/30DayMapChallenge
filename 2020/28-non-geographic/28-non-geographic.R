library(tidyverse)
library(ggforce)

satellites <- read_csv("https://raw.githubusercontent.com/devstronomy/nasa-data-scraper/master/data/csv/satellites.csv")

planets <- read_csv("https://raw.githubusercontent.com/devstronomy/nasa-data-scraper/master/data/csv/planets.csv") %>% 
  mutate(distance_from_sun = distance_from_sun * 10^6) %>% 
  rowwise() %>% 
  mutate(
    a = runif(1, 0, 2 * pi),
    x = distance_from_sun  * cos(a),
    y = distance_from_sun  * sin(a)
  )

ggplot(planets) +
  # geom_circle(aes(x0 = 0, y0 = 0, r = distance_from_sun), size = 0.1) +
  # geom_point(aes(x = distance_from_sun, y = 0, size = diameter / 2)) +
  geom_circle(aes(x0 = distance_from_sun, y0 = 0, r = diameter / 2)) +
  # scale_radius() +
  coord_fixed(clip = "off") +
  # theme_void() +
	theme(
		legend.position = "none"
	) +
	ggsave(here::here("2020", "28-non-geographic", "28-non-geographic.png"), dpi = 320, width = 12, height = 3)
