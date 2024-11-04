library(tidyverse)
library(sf)
library(rvest)
library(camcorder)

gg_record(here::here("30daymap-temp/"), width = 10, height = 8, dpi = 320)

# Birthplaces
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

# 2024 population estimates
states_pop_raw <- tidycensus::get_estimates(geography = "state", product = "population", vintage = 2023) 

states_pop <- states_pop_raw %>% 
  filter(variable == "POPESTIMATE") %>% 
  select(state = NAME, pop = value)

# Hex grid
us_hex <- read_sf(here::here("2024/data/us_states_hexgrid.geojson")) %>% 
  mutate(state = str_remove(google_name, " \\(United States\\)")) %>% 
  select(state)

# Birth state
states <- data.frame(state = state.name, state_abb = state.abb) %>% 
  add_row(state = "District of Columbia", state_abb = "DC")

us_birth <- us_hex %>% 
  left_join(birthplaces %>% filter(type == "US State"), by = c("state" = "region")) %>% 
  left_join(states_pop) %>% 
  left_join(states) %>% 
  mutate(
    count = replace_na(count, 0),
    players_per_100k = count / pop * 1e5
    )

f1 <- "Gabarito"
f2 <- "Graphik Compact"

pal <- MetBrewer::met.brewer("Tam")

ggplot(us_birth) +
  geom_sf(aes(fill = players_per_100k), color = "orange4", linewidth = 3.5, fill = NA) +
  geom_sf(aes(fill = players_per_100k), color = "white", linewidth = 1.2) +
  ggtext::geom_richtext(aes(geometry = geometry, label = paste0("**", state_abb, "**<br>**", round(players_per_100k, 1), "**<br>(", count, ")"), color = state_abb == "DC"), family = f2, show.legend = FALSE, stat = "sf_coordinates", fill = NA, label.size = 0, size = 4.5, lineheight = 1.05) +
  scale_fill_gradientn(colors = pal, breaks = seq(0, 12, 2)) +
  scale_color_manual(values = c("black", "white")) +
  coord_sf(crs = 3857) +
  theme_void(base_family = f1) +
  labs(
    title = "Where NBA stars are born",
    subtitle = "All-time NBA/ABA players born in each state per 100,000 current residents.\nNumbers in parentheses show total players.",
    caption = "Source: Basketball-Reference.com & US Census Bureau · Graphic: Georgios Karamanis"
  ) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    plot.title = element_text(family = f1, size = 30, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    plot.caption = element_text(hjust = 0.5, size = 11)
  )
  


