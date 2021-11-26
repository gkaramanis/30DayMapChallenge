library(tidyverse)
library(sf)
library(tabulizer)
library(fuzzyjoin)
library(mapdeck)

prices_tables <- extract_tables("2021/data/IMERISIO_DELTIO_ANA_NOMO_24_11_2021.pdf")

colnames <- c("region", "unleaded95", "unleaded100", "super", "diesel", "autogas", "diesel_home")

colnames(prices_tables[[1]]) <- colnames
colnames(prices_tables[[2]]) <- colnames

prices <- prices_tables[[1]] %>% 
  rbind(prices_tables[[2]]) %>% 
  as_tibble() %>% 
  slice(6:n()) %>% 
  mutate(
    across(-1, function(x) as.numeric(str_replace(x, ",", "."))),
    name = str_remove(region, "ΝΟΜΟΣ "),
    name = case_when(
      name == "ΑΙΤΩΛΙΑΣ ΚΑΙ ΑΚΑΡΝΑΝΙΑΣ" ~ "ΑΙΤΩΛΟΑΚΑΡΝΑΝΙΑΣ",
      name == "ΚΟΡΙΝΘΙΑΣ" ~ "ΚΟΡΙΝΘΟΥ",
      TRUE ~ name
    )
    )
  
greece <- read_sf("2021/data/nomoi_okxe/nomoi_okxe.shp", options = "ENCODING=WINDOWS-1253") %>% 
  st_simplify(dTolerance = 100) %>% 
  st_transform(crs = "WGS84") %>% 
  janitor::clean_names() %>% 
  mutate(
    name = str_remove(name_gr, "Ν. "),
    name = case_when(
      str_detect(name, "ΑΤΤΙΚΗΣ|ΑΘΗΝΩΝ|ΠΕΙΡΑΙΩΣ") ~ "ΑΤΤΙΚΗΣ",
      TRUE ~ name
    )
    )

gr_prices <- greece %>% 
  stringdist_left_join(prices) %>% 
  mutate(tooltip = paste0(name_eng, "<br>€", unleaded95))
  
set_token("") # insert token

mapdeck(style = mapdeck_style("light")) %>%
  add_polygon(
    gr_prices,
    fill_colour = "unleaded95",
    tooltip = "tooltip",
    palette = "ylorrd",
    legend = TRUE,
    legend_options = list(title = "Price of<br>Unleaded 95 RON<br>in EUR, 2021-11-24")
    ) %>% 
  mapdeck_view(location = c(25, 38), zoom = 6)

