library(tidyverse)
library(readxl)
library(janitor)
# library(tidygeocoder)
library(geosphere)
library(maps)

destinations_2019_xlsx <- read_xlsx(here::here("2020", "22-movement", "data", "destinationsstatistik_2019.xlsx"), skip = 5) %>% 
  clean_names()

destinations_2019 <- destinations_2019_xlsx %>% 
  fill(rapporterande_flygplats_stad, land) %>% 
  filter(row_number() != 2) %>% 
  mutate(totalt = as.numeric(totalt))

# countries <- destinations_2019 %>%
#   distinct(land) %>%
#   filter(str_detect(land, "Summa|Country|okänt", negate = TRUE))
# 
# country_coord <- geo(country = countries$land, method = "osm")

# write_csv(country_coord, file = here::here("2020", "22-movement", "data", "countries_coordinates.csv"))

# airports_coord <- destinations_2019 %>% 
#   distinct(rapporterande_flygplats_stad) %>% 
#   filter(str_detect(rapporterande_flygplats_stad, "Summa|summa|Reporting", negate = TRUE))
# 
# write_csv(airports_coord, file = here::here("2020", "22-movement", "data", "airports_coordinates.csv"))

coordinates <- read_csv(here::here("2020", "22-movement", "data", "countries_coordinates.csv")) 
airports <- read_csv(here::here("2020", "22-movement", "data", "airports_coordinates.csv")) 

country_totals_2019 <- destinations_2019 %>% 
  filter(str_detect(land, "Summa")) %>% 
  mutate(country = str_remove(land, " Summa")) %>% 
  left_join(coordinates) %>% 
  rename(to_lat = lat, to_long = long) %>% 
  left_join(airports) %>% 
  rename(from_lat = lat, from_long = long) %>% 
  filter(!is.na(from_lat) & !is.na(to_lat))

# from <- cbind(country_totals_2019$from_long, country_totals_2019$from_lat)
# to <- cbind(country_totals_2019$to_long, country_totals_2019$to_lat)
# 
# inter <- gcIntermediate(from, to, addStartEnd=TRUE)
# 
# map("world", col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05)
# lines(inter, lwd=2)

world_map <- map_data("world")

ggplot(country_totals_2019) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey30", colour = "grey10", size = 0.05) +
  geom_curve(aes(x = from_long, y = from_lat, xend = to_long, yend = to_lat, alpha = totalt, color = rapporterande_flygplats_stad), curvature = 0.2, size = 0.15) +
  scale_alpha_continuous(range = c(0.1, 1)) +
	coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey10", color = NA)
  ) 

ggsave(here::here("2020", "22-movement", "22-movement.png"), dpi = 320)
