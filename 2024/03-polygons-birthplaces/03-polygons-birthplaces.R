library(tidyverse)
library(sf)
library(rvest)
library(camcorder)

gg_record(here::here("30daymap-temp/"), width = 10, height = 6.5, dpi = 320)

url <- "https://www.basketball-reference.com/friv/birthplaces.fcgi"

birthplaces <- read_html(url) %>%
  html_nodes("#birthplace_1, #birthplace_2") %>%
  map_df(function(node) {
    id <- html_attr(node, "id")
    node %>%
      html_nodes("p") %>%
      html_text() %>%
      strsplit("\\(") %>%
      map_df(function(x) {
        location <- trimws(x[1])
        count <- as.numeric(gsub("\\D", "", x[2]))
        type <- if(id == "birthplace_1") "US State" else "Country"
        data.frame(region = location, count = count, type = type)
      })
  }) %>% 
  mutate(region = str_trim(region)) %>% 
  mutate(region = str_squish(region))

states <- rnaturalearth::ne_states(country = "United States of America") %>% 
  janitor::clean_names() %>% 
  select(admin = name, abbrev) %>% 
  mutate(type = "US State")

world_raw <- rnaturalearth::ne_countries() %>% 
  janitor::clean_names() %>% 
  select(admin, abbrev) %>% 
  mutate(type = "Country")

world <- world_raw  %>% 
  filter(admin != "United States") %>% 
  filter(admin != "Antarctica") %>% 
  add_row(states)

world_birthplaces <- world %>% 
  mutate(
    region = case_when(
      admin == "The Bahamas" ~ "Bahamas",
      admin == "Republic of Serbia" ~ "Serbia",
      admin == "Russia" ~ "Russian Federation",
      admin == "North Macedonia" ~ "Republic of North Macedonia",
      TRUE ~ admin
    )
  ) %>% 
  full_join(birthplaces)


f1 <- "Gabarito"

pal <- MetBrewer::met.brewer("Tam")

ggplot(world_birthplaces) +
  geom_sf(aes(fill = count), color = "white", linewidth = 0.15) +
  ggrepel::geom_text_repel(data = . %>% slice_max(order_by = count, n = 5), aes(geometry = geometry, label = paste0(abbrev, "\n", count), size = count), stat = "sf_coordinates", seed = 99, family = f1, bg.color = "#FDFAF6", min.segment.length = 0, segment.size = 0.3, show.legend = FALSE, lineheight = 0.85, box.padding = 0.5) +
  ggrepel::geom_text_repel(data = . %>% filter(type == "Country") %>% slice_max(order_by = count, n = 5), aes(geometry = geometry, label = paste0(abbrev, "\n", count), size = count), stat = "sf_coordinates", seed = 99, family = f1, bg.color = "#FDFAF6", min.segment.length = 0, segment.size = 0.3, show.legend = FALSE, lineheight = 0.85, box.padding = 0.5) +
  scale_fill_gradientn(colors = pal, na.value = "#E2E6E9") +
  scale_size_continuous(range = c(3.2, 5)) +
  coord_sf(crs = "+proj=eqearth +wktext") +
  labs(
    caption = "**California and New York have produced the most NBA/ABA players in the United States,<br>while France and Canada lead internationally**<br><br>Source: Basketball Reference · Graphic: Georgios Karamanis",
    fill = "NBA players born here"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "top",
    legend.key.width = unit(3, "lines"),
    legend.key.height = unit(0.6, "lines"),
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5, size = 16),
    legend.text = element_text(size = 11),
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.caption = ggtext::element_markdown(hjust = 0.5, size = 11)
  )
  
