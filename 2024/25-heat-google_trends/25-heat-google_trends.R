library(tidyverse)
library(sf)
library(camcorder)

gg_record(here::here("30daymap-temp/"), width = 10.5, height = 8, dpi = 320)

# https://trends.google.com/trends/explore?cat=20&date=all&geo=US&q=%2Fm%2F0jm2v&hl=en
heat_google <- read_csv(here::here("2024/data/heat.csv"), skip = 1) %>% 
  rename(dma = 1, interest = 2) %>% 
  mutate(dma = str_remove_all(dma, "\\,")) %>% 
  mutate(dma = case_when(
    dma == "Florence-Myrtle Beach SC" ~ "Myrtle Beach-Florence SC",
    dma == "Montgomery (Selma) AL" ~ "Montgomery-Selma AL",
    dma == "Wichita-Hutchinson KS" ~ "Wichita-Hutchinson KS Plus",
    dma == "Sioux Falls(Mitchell) SD" ~ "Sioux Falls (Mitchell) SD",
    dma == "Boston MA-Manchester NH" ~ "Boston MA (Manchester NH)",
    dma == "Paducah KY-Cape Girardeau MO-Harrisburg-Mount Vernon IL" ~ "Paducah KY-Cape Girardeau MO-Harrisburg IL",
    dma == "Wichita Falls TX & Lawton OK" ~ "Wichita Falls TX-Lawton OK",
    dma == "Birmingham AL" ~ "Birmingham (Anniston and Tuscaloosa) AL",
    dma == "Greenville-Spartanburg SC-Asheville NC-Anderson SC" ~ "Greenville-Spartanburg SC-Asheville NC-AndersonSC",
    dma == "Miami-Ft. Lauderdale FL" ~ "Miami-Fort Lauderdale FL",
    TRUE ~ dma
  )
  )

# https://team.carto.com/u/andrew/viz/47337cfe-66cb-11e5-a378-0ef24382571b/public_map?redirected=true
dma <- read_sf(here::here("2024/data/dma.geojson")) %>% 
  mutate(dma = str_remove_all(dma_1, "\\,")) %>% 
  select(dma) 

heat_dma <- dma %>% 
  left_join(heat_google)

# Colors
bg_color <- "#353231"    
border_color <- "#B35C2B" 
pal <- MetBrewer::met.brewer("Greek")

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Publico Headline"

ggplot(heat_dma) +
  geom_sf(aes(fill = interest), color = border_color, linewidth = 0.15) +
  scale_fill_gradientn(colors = pal, na.value = "black") +
  labs(
    title = "Shocking: Miami Likes the Heat",
    subtitle = "Regional interest in the Miami Heat across US TV markets (DMAs)",
    caption = "Data: Google Trends 2004-2024 · Graphic: Georgios Karamanis",
    fill = "Google Trends score, 100 indicates highest search volume"
  ) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = bg_color, color = NA),
    plot.title = element_text(color = pal[5], size = 24, face = "bold", margin = margin(10, 0, 10, 0), hjust = 0.5),
    plot.subtitle = element_text(color = pal[3], size = 14, margin = margin(0, 0, 8, 0), hjust = 0.5),
    plot.caption = element_text(color = pal[4], size = 10, margin = margin(20, 0, 5, 0), hjust = 0.5),
    legend.position = "top",
    legend.key.width = unit(4, "lines"),
    legend.key.height = unit(0.6, "lines"),
    legend.title = element_text(color = pal[4], hjust = 0.5, margin = margin(0, 0, 8, 0)),
    legend.text = element_text(color = pal[4]),
    # legend.title = element_blank(),
    legend.title.position = "top",
    plot.margin = margin(10, 10, 10, 10)
  )
