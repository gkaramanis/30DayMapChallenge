library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")
zip_ul <- file.path(dir_data, "ul.zip")
bb_city <- c(17.55, 59.80, 17.76, 59.90)
day_date <- "20261124"
sigma <- 80

cached <- function(name, expr) {
  path <- file.path(dir_data, paste0(name, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  saveRDS(expr, path)
  expr
}

osm_get <- function(name, bb, features) {
  cached(paste0("osm-", name), {
    res <- imap(features, \(value, key) osmdata::opq(bb, timeout = 600) |> osmdata::add_osm_feature(key, value) |> osmdata::osmdata_sf())
    do.call(c, unname(res))
  })
}

read_ul <- function(file, ...) read_csv(unz(zip_ul, file), col_types = cols(.default = "c"), ...)

# Café count near each point, nearer ones weighted more (Gaussian)
nearness <- function(x, y, xy) {
  d2 <- outer(x, xy[, "X"], "-")^2 + outer(y, xy[, "Y"], "-")^2
  rowSums(exp(-d2 / (2 * sigma^2)))
}

centre <- st_point(c(17.646, 59.858)) |> st_sfc(crs = 4326) |> st_transform(crs_se) |> st_coordinates()
view <- map_box_view(centre, 6000)
view_sf <- st_as_sfc(view)

roads <- osm_get("roads-city", bb_city, list(highway = c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential", "living_street", "service", "motorway_link", "trunk_link", "primary_link", "secondary_link")))$osm_lines |> st_transform(crs_se) |> st_filter(view_sf)

food <- osm_get("cafes-bakeries-city", bb_city, list(amenity = "cafe", shop = "bakery"))
osm_cafes <- bind_rows(food$osm_points, st_centroid(food$osm_polygons)) |> filter(amenity %in% "cafe" | shop %in% "bakery") |> select(name) |> st_transform(crs_se)

# Overture adds places missing from OSM: open, confidence 0.7+, not within 50 m of an OSM café, one per name
ov_places <- cached("overture-cafes-bakeries-city", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "INSTALL httpfs; LOAD httpfs; INSTALL spatial; LOAD spatial; SET s3_region = 'us-west-2';")
  DBI::dbGetQuery(con, sprintf("SELECT names.primary AS name, taxonomy.primary AS category, confidence, operating_status, ST_X(geometry) AS lon, ST_Y(geometry) AS lat FROM read_parquet('s3://overturemaps-us-west-2/release/2026-11-18.0/theme=places/type=place/*', hive_partitioning = 1) WHERE bbox.xmin BETWEEN %f AND %f AND bbox.ymin BETWEEN %f AND %f AND taxonomy.primary IN ('cafe', 'coffee_shop', 'bakery')", bb_city[1], bb_city[3], bb_city[2], bb_city[4]))
})
ov_cafes <- ov_places |> filter(confidence >= 0.7, !operating_status %in% "permanently_closed") |> distinct(key = str_to_lower(str_remove_all(name, "[^[:alnum:]]")), .keep_all = TRUE) |> st_as_sf(coords = c("lon", "lat"), crs = 4326) |> st_transform(crs_se) |> select(name)
ov_only <- ov_cafes[lengths(st_is_within_distance(ov_cafes, osm_cafes, 50)) == 0, ]

cafes <- bind_rows(osm_cafes, ov_only) |> st_filter(st_buffer(view_sf, 500))
cafe_xy <- st_coordinates(cafes)

# Each city line's most frequent shape on a Tuesday; ul.zip comes from 17-light-dark
calendar <- read_ul("calendar.txt")
extra_days <- read_ul("calendar_dates.txt") |> filter(date == day_date, exception_type == "1")
services <- c(calendar |> filter(tuesday == "1", start_date <= day_date, end_date >= day_date) |> pull(service_id), extra_days$service_id)
city_routes <- read_ul("routes.txt") |> filter(as.integer(route_short_name) < 100)
line_shapes <- read_ul("trips.txt") |> filter(service_id %in% services) |> semi_join(city_routes, by = join_by(route_id)) |> count(route_id, shape_id) |> slice_max(n, n = 1, by = route_id, with_ties = FALSE)
lines <- read_ul("shapes.txt") |> semi_join(line_shapes, by = join_by(shape_id)) |> mutate(across(c(shape_pt_lon, shape_pt_lat), as.numeric)) |> arrange(shape_id, as.integer(shape_pt_sequence)) |> summarise(geometry = st_sfc(st_linestring(cbind(shape_pt_lon, shape_pt_lat)), crs = 4326), .by = shape_id) |> st_as_sf() |> st_transform(crs_se)

# 10 m steps along each line, coloured at their midpoints
line_xy <- lines |> st_segmentize(10) |> st_coordinates()
steps <- tibble(x = line_xy[, "X"], y = line_xy[, "Y"], line = line_xy[, "L1"]) |> mutate(xend = lead(x), yend = lead(y), .by = line) |> filter(!is.na(xend)) |> mutate(near = nearness((x + xend) / 2, (y + yend) / 2, cafe_xy)) |> arrange(near)

# Faint glow on the ground, capped at the strongest value on a line
field <- expand_grid(x = seq(view[["xmin"]], view[["xmax"]], by = 40), y = seq(view[["ymin"]], view[["ymax"]], by = 40)) |> mutate(glow = sqrt(pmin(nearness(x, y, cafe_xy) / max(steps$near), 1)))

# pbf-suburbs.rds comes from 08-utopia
districts <- readRDS(file.path(dir_data, "pbf-suburbs.rds")) |> filter(name %in% c("Luthagen", "Svartbäcken", "Gränby", "Fålhagen", "Kungsängen", "Eriksberg")) |> st_transform(crs_se)

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

p <- ggplot() +
  geom_raster(data = field, aes(x, y, alpha = glow), fill = clr_accent, interpolate = TRUE) +
  geom_sf(data = roads, colour = clr_grid, linewidth = 0.25) +
  geom_segment(data = steps, aes(x, y, xend = xend, yend = yend, colour = sqrt(near), linewidth = sqrt(near)), lineend = "round") +
  geom_sf(data = st_filter(cafes, view_sf), colour = clr_text, size = 0.7) +
  geom_sf_text(data = districts, aes(label = name), family = f1, colour = clr_text2, size = 3.6) +
  scale_alpha_continuous(range = c(0, 0.35), guide = "none") +
  scale_colour_gradientn(colours = c(colorspace::darken(clr_grid, 0.3), clr_accent, "#8A4E0C"), values = c(0, 0.35, 1), guide = "none") +
  scale_linewidth_continuous(range = c(0.4, 2), guide = "none") +
  coord_sf(xlim = as.numeric(view[c("xmin", "xmax")]), ylim = as.numeric(view[c("ymin", "ymax")]), crs = crs_se, expand = FALSE) +
  theme_map()

save_map(p, here::here("2026/19-smell/19-smell-coffee-lines.png"),
  title = "Most Uppsala cafés\\ sit by a bus line",
  subtitle = "City bus lines turn amber near cafés and\\ bakeries (dots), darker where several are close",
  caption = caption_src("UL GTFS via Trafiklab, OpenStreetMap, Overture Maps")
)
