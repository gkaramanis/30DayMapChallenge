library(tidyverse)
library(sf)
library(rvest)
library(camcorder)

gg_record(here::here("30daymap-temp/"), width = 12, height = 7, dpi = 320)

url <- "https://en.wikipedia.org/wiki/Basketball_at_the_Summer_Olympics#Women's_tournament"

women_wp <- read_html(url) %>% 
  html_table() %>% 
  .[[14]] %>% 
  janitor::clean_names() %>% 
  mutate(iso_a3 = countrycode::countrycode(team, origin = "country.name", destination = "iso3c"))
  

world <- rnaturalearth::ne_countries() %>% 
  select(name, iso_a3, continent) %>% 
  filter(name != "Antarctica") %>% 
  left_join(women_wp)

f1 <- "Paysage"
f1b <- "Graphik Compact"

pal <- MetBrewer::met.brewer("Johnson")

ggplot(world %>% filter(!team %in% c("Soviet Union", "Czechoslovakia"))) +
  geom_sf(aes(fill = winning_percentage)) +
  ggrepel::geom_label_repel(data = . %>% filter(winning_percentage >= 0.5), aes(label = paste0(iso_a3, "\n", str_remove(scales::number(winning_percentage, accuracy = 0.001), "^0")), geometry = geometry), stat = "sf_coordinates", size = 4, label.padding = 0.1, lineheight = 0.8, family = f1b, seed = 999, label.size = 0, fill = alpha("white", 0.8), force = 10, segment.size = 0.2) +
  scale_fill_gradientn(colors = pal, na.value = "white", breaks = seq(0, 1, 0.2), limits = c(0, 1), labels = c("0", ".200", ".400", ".600", ".800", "1.000")) +
  coord_sf(crs = "+proj=eck2", xlim = c(-180, 180) * 0.93e5) +
  labs(
    title = "Olympic Women's Basketball",
    subtitle = "Historical winning percentages by country, as of August 2021",
    caption = "Source: Wikipedia · Graphic: Georgios Karamanis",
    fill = ""
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(4, "lines"),
    legend.key.height = unit(0.5, "lines"),
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.grid = element_line(linewidth = 0.1, color = "grey70"),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.5, margin = margin(20, 0, 0, 0), size = 11),
    plot.margin = margin(10, 20, 10, 20)
  )
