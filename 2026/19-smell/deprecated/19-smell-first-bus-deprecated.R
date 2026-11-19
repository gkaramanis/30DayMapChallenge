library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")
zip_ul <- file.path(dir_data, "ul.zip")

# Bounding box (xmin, ymin, xmax, ymax) in WGS84, for the OSM queries
bb_city <- c(17.55, 59.80, 17.76, 59.90)

# An ordinary Tuesday in the feed
day_date <- "20261124"

# Cafés count as near a stop within this many metres, as the crow flies
near_m <- 400

# Cache as rds in data/
cached <- function(name, expr) {
  path <- file.path(dir_data, paste0(name, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  saveRDS(expr, path)
  expr
}

# features: named list of key = values; each key is queried separately and the results combined
osm_get <- function(name, bb, features) {
  cached(paste0("osm-", name), {
    res <- imap(features, \(value, key) osmdata::opq(bb, timeout = 600) |> osmdata::add_osm_feature(key, value) |> osmdata::osmdata_sf())
    do.call(c, unname(res))
  })
}

# "HH:MM" or "HH:MM:SS" as minutes after midnight; GTFS hours can pass 24
hm_min <- function(x) {
  parts <- str_split_fixed(x, ":", 3)
  as.numeric(parts[, 1]) * 60 + as.numeric(parts[, 2])
}

day_codes <- c("Mo", "Tu", "We", "Th", "Fr", "Sa", "Su")
month_codes <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# TRUE when a spec like "Mo-Fr,Su" or "Sep-May" covers position i of codes; ranges may wrap
covers <- function(spec, codes, i) {
  parts <- str_split_1(str_remove_all(spec, " "), ",")
  any(map_lgl(parts, \(part) {
    ends <- match(str_split_1(part, "-"), codes)
    if (anyNA(ends)) return(FALSE)
    if (length(ends) == 1) return(ends == i)
    if (ends[1] <= ends[2]) i >= ends[1] & i <= ends[2] else i >= ends[1] | i <= ends[2]
  }))
}

# Opening time in minutes from an OSM opening_hours value on one weekday and month.
# NA when the place is closed that day or the value has no rule for it.
opening_min <- function(hours, day = "Tu", month = "Nov") {
  if (is.na(hours)) return(NA_real_)
  if (str_detect(hours, "^\\d{1,2}:\\d{2}")) return(hm_min(str_extract(hours, "^\\d{1,2}:\\d{2}")))
  rules <- str_trim(str_split_1(hours, ";"))
  hits <- keep(rules, \(rule) {
    m <- str_match(rule, "^(?:([A-Za-z,\\- ]+):\\s*)?([A-Za-z,\\- ]+?)\\s+(\\d{1,2}:\\d{2})")
    if (is.na(m[1])) return(FALSE)
    month_ok <- is.na(m[2]) || covers(m[2], month_codes, match(month, month_codes))
    month_ok && covers(m[3], day_codes, match(day, day_codes))
  })
  if (length(hits) == 0) return(NA_real_)
  hm_min(str_extract(last(hits), "\\d{1,2}:\\d{2}"))
}

# Central Uppsala, the same view as day 15
centre <- st_point(c(17.646, 59.858)) |> st_sfc(crs = 4326) |> st_transform(crs_se) |> st_coordinates()
view <- map_box_view(centre, 6000)
view_sf <- st_as_sfc(view)

roads <- osm_get("roads-city", bb_city, list(highway = c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential", "living_street", "service", "motorway_link", "trunk_link", "primary_link", "secondary_link")))$osm_lines |> st_transform(crs_se) |> st_filter(view_sf)

# Cafés and bakeries as points with their Tuesday opening time; mapped buildings become their centroids
food <- osm_get("cafes-bakeries-city", bb_city, list(amenity = "cafe", shop = "bakery"))
food_cols <- c("osm_id", "name", "amenity", "shop", "opening_hours")
cafes <- bind_rows(food$osm_points |> select(any_of(food_cols)), food$osm_polygons |> select(any_of(food_cols)) |> st_centroid()) |> filter(amenity %in% "cafe" | shop %in% "bakery") |> mutate(open_min = map_dbl(opening_hours, opening_min)) |> st_transform(crs_se)

# Services running on the day, from the weekly calendar plus added dates; ul.zip comes from 17-light-dark's GTFS download
calendar <- read_csv(unz(zip_ul, "calendar.txt"), col_types = cols(.default = "c"))
calendar_dates <- read_csv(unz(zip_ul, "calendar_dates.txt"), col_types = cols(.default = "c"))
services <- union(calendar |> filter(tuesday == "1", start_date <= day_date, end_date >= day_date) |> pull(service_id), calendar_dates |> filter(date == day_date, exception_type == "1") |> pull(service_id))
trips_day <- read_csv(unz(zip_ul, "trips.txt"), col_types = cols(.default = "c")) |> filter(service_id %in% services)

# First departure of the day at each stop, platforms pooled under their parent station
gtfs_stops <- read_csv(unz(zip_ul, "stops.txt"), col_types = cols(.default = "c"))
stop_times <- read_csv(unz(zip_ul, "stop_times.txt"), col_types = cols(.default = "c"), col_select = c(trip_id, stop_id, departure_time))
first_dep <- stop_times |> semi_join(trips_day, by = join_by(trip_id)) |> left_join(gtfs_stops |> select(stop_id, parent_station), by = join_by(stop_id)) |> summarise(first_min = min(hm_min(departure_time)), .by = parent_station)
stops <- gtfs_stops |> filter(location_type == "1") |> inner_join(first_dep, by = join_by(stop_id == parent_station)) |> st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) |> st_transform(crs_se) |> st_filter(view_sf)

# Wait from the first bus until the earliest café or bakery near the stop opens
near <- st_is_within_distance(stops, cafes, dist = near_m)
earliest_open <- map_dbl(near, \(i) if (all(is.na(cafes$open_min[i]))) NA_real_ else min(cafes$open_min[i], na.rm = TRUE))
wait_breaks <- c(0, 2, 3, 4, 5, Inf) * 60
wait_labels <- c("Under 2 h", "2–3 h", "3–4 h", "4–5 h", "Over 5 h")
stops_wait <- stops |> mutate(n_cafes = lengths(near), wait_min = earliest_open - first_min, wait_class = cut(wait_min, wait_breaks, labels = wait_labels, right = FALSE))

stops_far <- stops_wait |> filter(n_cafes == 0)
stops_unknown <- stops_wait |> filter(n_cafes > 0, is.na(wait_min))
stops_known <- stops_wait |> filter(!is.na(wait_min)) |> arrange(wait_min)

# Light to dark roast: the longer the wait, the darker the stop
clr_wait <- setNames(c("#F2C766", "#E8A020", "#B8700F", "#7E450B", "#45250A"), wait_labels)

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

p <- ggplot() +
  geom_sf(data = roads, colour = clr_grid, linewidth = 0.25) +
  geom_sf(data = stops_far, colour = clr_text2, size = 0.4, alpha = 0.6) +
  geom_sf(data = stops_unknown, aes(shape = "Stop near a café with no hours in OSM"), colour = clr_text2, fill = NA, size = 1.6, stroke = 0.35) +
  geom_sf(data = cafes |> st_filter(view_sf), aes(shape = "Café or bakery"), colour = clr_text, size = 1.1) +
  geom_sf(data = stops_known, aes(fill = wait_class), shape = 21, colour = clr_bg, size = 3.2, stroke = 0.5) +
  scale_fill_manual(values = clr_wait, drop = FALSE, name = "Wait for coffee after the first bus") +
  scale_shape_manual(values = c("Café or bakery" = 18, "Stop near a café with no hours in OSM" = 21), name = NULL) +
  guides(fill = guide_legend(order = 1, nrow = 1, label.position = "bottom", override.aes = list(size = 5)), shape = guide_legend(order = 2, ncol = 1, override.aes = list(size = 3, colour = c(clr_text, clr_text2)))) +
  coord_sf(xlim = as.numeric(view[c("xmin", "xmax")]), ylim = as.numeric(view[c("ymin", "ymax")]), crs = crs_se, expand = FALSE) +
  theme_map() +
  theme(legend.background = element_rect(fill = alpha(clr_bg, 0.85), colour = NA), legend.margin = margin(6, 8, 6, 8), legend.key.spacing.x = unit(12, "pt"), legend.box.just = "left", legend.spacing.y = unit(6, "pt"))

save_map(p, here::here("2026/19-smell/19-smell-first-bus.png"),
  title = "The first bus comes hours before the coffee",
  subtitle = "Time from each stop's first Tuesday bus until the\\ earliest café or bakery within 400 m opens",
  caption = caption_src("UL GTFS via Trafiklab, OpenStreetMap")
)
