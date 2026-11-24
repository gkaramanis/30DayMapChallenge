library(tidyverse)
library(sf)
library(igraph)

source(here::here("2026", "R", "helpers.R"))

pbf <- file.path(dir_2026, "data", "r5", "uppsala.osm.pbf")

centre <- st_sfc(st_point(c(17.64609, 59.85853)), crs = 4326) |> st_transform(crs_se) |> st_coordinates()
view <- map_box_view(centre, 14000)
view_wkt <- view |> st_as_sfc() |> st_transform(4326) |> st_as_text()

sql <- "SELECT osm_id, highway, other_tags FROM lines WHERE highway IS NOT NULL"
roads <- read_sf(pbf, query = sql, wkt_filter = view_wkt)

tag <- function(x, key) str_match(x, paste0('"', key, '"=>"([^"]*)"'))[, 2]

roads_tagged <- roads |>
  mutate(
    maxspeed = suppressWarnings(as.numeric(tag(other_tags, "maxspeed"))),
    bicycle = tag(other_tags, "bicycle"),
    access = tag(other_tags, "access"),
    cycleway_tags = str_extract_all(other_tags, '"cycleway[^"]*"=>"[^"]*"'),
    separate = map_lgl(cycleway_tags, \(x) any(str_detect(x, "track|separate")))
  )

low_stress <- roads_tagged |>
  filter(is.na(access) | !access %in% c("private", "no")) |>
  filter(
    highway %in% c("cycleway", "living_street", "pedestrian", "residential") |
      (highway == "path" & (is.na(bicycle) | bicycle != "no")) |
      (highway %in% c("footway", "track") & bicycle %in% c("yes", "designated")) |
      (highway %in% c("service", "unclassified", "tertiary") & !is.na(maxspeed) & maxspeed <= 30) |
      (highway %in% c("primary", "secondary", "trunk", "tertiary") & separate)
  ) |>
  st_transform(crs_se) |>
  st_crop(view) |>
  filter(st_is(geometry, "LINESTRING"))

# Segments between consecutive vertices, joined on shared coordinates
coords <- low_stress |>
  st_coordinates() |>
  as_tibble() |>
  mutate(key = paste(round(X), round(Y)))

segs <- coords |>
  group_by(L1) |>
  mutate(from = key, to = lead(key), xend = lead(X), yend = lead(Y)) |>
  ungroup() |>
  filter(!is.na(to), from != to)

g <- graph_from_data_frame(segs |> select(from, to), directed = FALSE)
comp <- components(g)

sizes <- tibble(key = names(comp$membership), comp = comp$membership) |>
  count(comp, name = "n") |>
  arrange(desc(n)) |>
  mutate(rank = row_number(), group = if_else(rank <= 8, as.character(rank), "other"))

segs_comp <- segs |>
  mutate(comp = comp$membership[from]) |>
  left_join(sizes, by = "comp")

p <- ggplot(segs_comp |> arrange(rank > 8, desc(rank))) +
  geom_segment(aes(X, Y, xend = xend, yend = yend, color = group)) +
  coord_map_box(view) +
  theme_map()

ggsave(here::here("2026/24-network/alternatives/24-network-cycling-components.png"), p, width = map_box$width, height = map_box$height, dpi = 150)
