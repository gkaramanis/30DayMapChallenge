library(tidyverse)
library(sf)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")
zip_ul <- file.path(dir_data, "ul.zip")
day_date <- "20261124"
centre <- st_point(c(17.646, 59.858)) |> st_sfc(crs = 4326) |> st_transform(crs_se) |> st_coordinates()
view <- map_box_view(centre, 8000)
view_sf <- st_as_sfc(view)

read_ul <- function(file, ...) read_csv(unz(zip_ul, file), col_types = cols(.default = "c"), ...)

food <- readRDS(file.path(dir_data, "osm-food-lan.rds"))
cols <- c("osm_id", "amenity", "shop", "cuisine")
all_places <- bind_rows(food$osm_points |> select(any_of(cols)), food$osm_polygons |> select(any_of(cols)) |> st_centroid()) |> st_transform(crs_se) |> st_filter(view_sf)
bakeries <- readRDS(file.path(dir_data, "osm-cafes-bakeries-city.rds"))
bakery_places <- bind_rows(bakeries$osm_points |> select(any_of(cols)), bakeries$osm_polygons |> select(any_of(cols)) |> st_centroid()) |> st_transform(crs_se) |> st_filter(view_sf)
places <- bind_rows(all_places, bakery_places |> filter(shop %in% "bakery")) |> mutate(kind = case_when(str_detect(cuisine, "kebab|turkish|middle_eastern|falafel") ~ "kebab", str_detect(cuisine, "pizza") ~ "pizza", shop %in% "bakery" | str_detect(cuisine, "bakery") ~ "bakery", TRUE ~ NA_character_)) |> filter(!is.na(kind))

# Busiest city lines on a Tuesday, by trip count
calendar <- read_ul("calendar.txt")
extra_days <- read_ul("calendar_dates.txt") |> filter(date == day_date, exception_type == "1")
services <- c(calendar |> filter(tuesday == "1", start_date <= day_date, end_date >= day_date) |> pull(service_id), extra_days$service_id)
city_routes <- read_ul("routes.txt") |> filter(as.integer(route_short_name) < 100)
trips <- read_ul("trips.txt") |> filter(service_id %in% services) |> semi_join(city_routes, by = join_by(route_id))
top_routes <- trips |> count(route_id, sort = TRUE) |> slice_head(n = 5) |> left_join(city_routes |> select(route_id, route_short_name), by = join_by(route_id))
line_shapes <- trips |> semi_join(top_routes, by = join_by(route_id)) |> count(route_id, shape_id) |> slice_max(n, n = 2, by = route_id, with_ties = FALSE)
lines <- read_ul("shapes.txt") |> semi_join(line_shapes, by = join_by(shape_id)) |> mutate(across(c(shape_pt_lon, shape_pt_lat), as.numeric)) |> arrange(shape_id, as.integer(shape_pt_sequence)) |> summarise(geometry = st_sfc(st_linestring(cbind(shape_pt_lon, shape_pt_lat)), crs = 4326), .by = shape_id) |> st_as_sf() |> st_transform(crs_se) |> st_filter(view_sf)

# Places within 150 m of a line
near <- places |> filter(lengths(st_is_within_distance(geometry, st_union(lines), 150)) > 0)
roads <- readRDS(file.path(dir_data, "osm-roads-city.rds"))$osm_lines |> st_transform(crs_se) |> st_filter(view_sf)

p <- ggplot() +
  geom_sf(data = roads, colour = clr_grid, linewidth = 0.2) +
  geom_sf(data = lines, colour = clr_text2, linewidth = 1.2, alpha = 0.5) +
  geom_sf(data = places, colour = "grey70", size = 0.6) +
  geom_sf(data = near, aes(colour = kind), size = 1.8) +
  coord_map_box(view) +
  theme_map()

message(nrow(places), " places, ", nrow(near), " near lines; ", paste(top_routes$route_short_name, collapse = ","))
print(count(st_drop_geometry(near), kind))
ggsave(here::here("2026/23-taste/alternatives/23-taste-kebab-pizza-bakery.png"), p, width = map_box$width, height = map_box$height, dpi = 150)
