library(tidyverse)
library(rvest)
library(sf)
library(camcorder)

gg_record(here::here("30daymap-temp"), width = 9.85, height = 8, dpi = 320)

ni_tbl <- read_html(here::here("2025/data/nature_index.html")) |> 
  html_table() %>%
  .[[1]] |> 
  janitor::clean_names()

ni_tbl_clean <- ni_tbl |> 
  mutate(
    admin = case_when(
      str_detect(country_territory, "United States") ~ "United States of America",
      str_detect(country_territory, "United Kingdom") ~ "United Kingdom",
      str_detect(country_territory, "Tanzania") ~ "United Republic of Tanzania",
      country_territory == "Democratic Republic of the Congo (DRC)" ~ "Democratic Republic of the Congo",
      country_territory == "Congo" ~ "Republic of the Congo",
      str_detect(country_territory, "Serbia") ~ "Republic of Serbia",
      str_detect(country_territory, "Czech") ~ "Czechia",
      str_detect(country_territory, "Bahamas") ~ "The Bahamas",
      str_detect(country_territory, "Palestinian territories") ~ "Palestine",
      str_detect(country_territory, "Côte d'Ivoire") ~ "Ivory Coast",
      TRUE ~ country_territory
    )
  )
  
world <- rnaturalearth::countries110 |> 
  janitor::clean_names()

wni <- world |> 
  select(admin) |> 
  left_join(ni_tbl_clean) %>%
  st_buffer(., -0.0001)  |> 
  st_transform(crs = '+proj=vandg4 +lon_0=0 +x_0=0 +y_0=0 +R_A +datum=WGS84 +units=m +no_defs') 

pal <- MetBrewer::met.brewer("Tam")

f1 <- "Inclusive Sans"
f2 <- "Publico Headline"

ggplot(wni, aes(fill = log10(share))) +
  geom_sf() +
  ggrepel::geom_text_repel(data = . %>% slice_max(order_by = share, n = 5), aes(label = paste0(country_territory, "\n", scales::number(share, accuracy = 0.01)), geometry = geometry), color = "white", stat = "sf_coordinates", bg.color = "black", size = 3.5) +
  scale_fill_stepsn(colors = pal, na.value = "grey90", labels = function(x) scales::number(10^x)) +
  labs(
    title = "Nature Index 2025",
    subtitle = "Share of articles published in scientific journals by country or territory",
    caption = "Data: Nature Index (1 August 2024 - 31 July 2025) · Graphic: Georgios Karamanis",
    fill = "Share (log)"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "top",
    legend.key.width = unit(2, "lines"),
    legend.key.height = unit(0.8, "lines"),
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5),
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.grid = element_line(linewidth = 0.3, color = "#C0C0C0"),
    plot.title = element_text(size = 22, hjust = 0.5, margin = margin(10, 0, 0, 0), face = "bold"),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(5, 0, 15, 0)),
    plot.caption = element_text(size = 10, color = "#353231", hjust = 0.5, margin = margin(10, 0, 10, 0))
  )

record_polaroid()
