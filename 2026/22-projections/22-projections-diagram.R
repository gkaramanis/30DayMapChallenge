library(tidyverse)
library(sf)
library(xml2)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")

# Lines and terminal roundels from UL's city bus map, autumn 2026, in page
# points (y down): one path per line (line-9) and two circles per line (end-9-1).
# https://www.ul.se/globalassets/kartor/host-2026/stadsbussarnas-linjenat-i-uppsala_host_2026.pdf
diagram_svg <- file.path(dir_data, "ul-stadsbussar-host-2026-lines.svg")

# UL GTFS from 1 November 2026, the feed this map is fitted to
gtfs_file <- file.path(dir_data, "ul.zip")

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

svg <- xml_ns_strip(read_xml(diagram_svg))
paths <- xml_find_all(svg, "//path")
circles <- xml_find_all(svg, "//circle")

# Flip y so north is up
page_xy <- \(d) matrix(as.numeric(str_extract_all(d, "[0-9.]+")[[1]]), ncol = 2, byrow = TRUE) %*% diag(c(1, -1))

diagram_lines <- st_sf(line = str_remove(xml_attr(paths, "id"), "line-"), geometry = st_sfc(map(xml_attr(paths, "d"), \(d) st_linestring(page_xy(d)))))

roundels <- tibble(line = str_extract(xml_attr(circles, "id"), "(?<=end-)[0-9]+"), x = as.numeric(xml_attr(circles, "cx")), y = -as.numeric(xml_attr(circles, "cy")))

# Real network: per line and direction, the longest shape among those
# run by at least a tenth of the trips, so short turns don't set the terminals
gtfs <- tidytransit::read_gtfs(gtfs_file)

city_routes <- gtfs$routes |>
  filter(route_desc == "Stadsbuss", route_short_name %in% as.character(1:14)) |>
  select(route_id, line = route_short_name)

top_trips <- gtfs$trips |>
  inner_join(city_routes, by = join_by(route_id)) |>
  inner_join(count(gtfs$stop_times, trip_id, name = "n_stops"), by = join_by(trip_id)) |>
  mutate(n_shape = n(), .by = c(line, direction_id, shape_id)) |>
  filter(n_shape >= 0.1 * n(), .by = c(line, direction_id)) |>
  slice_max(n_stops, n = 1, with_ties = FALSE, by = c(line, direction_id))

real_lines <- tidytransit::shapes_as_sf(gtfs$shapes) |>
  inner_join(select(top_trips, shape_id, line, direction_id), by = join_by(shape_id)) |>
  st_transform(crs_se)

# Terminals a and b: first and last point of each line's shape
outbound <- filter(real_lines, direction_id == 0)

terminals <- st_coordinates(outbound) |>
  as_tibble() |>
  summarise(Xa = first(X), Ya = first(Y), Xb = last(X), Yb = last(Y), .by = L1) |>
  mutate(line = outbound$line[L1])

# Pair each line's two roundels with its terminals, keeping the line's direction
control <- roundels |>
  filter(n() == 2, .by = line) |>
  inner_join(terminals, by = join_by(line)) |>
  mutate(to_a = (row_number() == 1) == (diff(x) * (Xb - Xa) + diff(y) * (Yb - Ya) > 0)[1], .by = line) |>
  transmute(line, x, y, X = if_else(to_a, Xa, Xb), Y = if_else(to_a, Ya, Yb))

# Place the diagram with one scale and a shift for the whole network, no
# rotation or shear, so its lines keep UL's 0, 45 and 90 degree angles
stacked <- tibble(real = c(control$X, control$Y), page = c(control$x, control$y), axis = rep(c("x", "y"), each = nrow(control)))
fit <- coef(lm(real ~ 0 + axis + page, data = stacked))

diagram_se <- diagram_lines |>
  mutate(geometry = geometry * fit[["page"]] + fit[c("axisx", "axisy")]) |>
  st_set_crs(crs_se)

# Three lines that cross the city in different directions
focus_colors <- c("3" = clr_accent2, "6" = clr_text, "9" = clr_accent)
focus_real <- filter(outbound, line %in% names(focus_colors))
focus_drawn <- filter(diagram_se, line %in% names(focus_colors))

# Roundels where UL draws them, at the ends of the diagram lines
focus_roundels <- roundels |>
  filter(line %in% names(focus_colors)) |>
  mutate(X = x * fit[["page"]] + fit[["axisx"]], Y = y * fit[["page"]] + fit[["axisy"]])

# View around both versions of the lines, in the map box shape
focus_bb <- st_bbox(st_buffer(st_as_sfc(st_bbox(c(st_geometry(focus_real), st_geometry(focus_drawn)))), 600))
focus_w <- focus_bb[["xmax"]] - focus_bb[["xmin"]]
focus_h <- focus_bb[["ymax"]] - focus_bb[["ymin"]]
view <- map_box_view(c(focus_bb[["xmin"]] + focus_w / 2, focus_bb[["ymin"]] + focus_h / 2), max(focus_w, focus_h * map_box$width / map_box$height))

# Streets under the bus network so the map reaches every edge; cached by day 18
road_types <- c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential", "living_street")
roads <- readRDS(file.path(dir_data, "osm-roads-city.rds"))$osm_lines |> filter(highway %in% road_types) |> st_transform(crs_se) |> st_crop(view)

p <- ggplot() +
  geom_sf(data = roads, color = colorspace::lighten(clr_grid, 0.4), linewidth = 0.2) +
  geom_sf(data = st_crop(real_lines, view), color = clr_grid, linewidth = 0.4) +
  geom_sf(data = focus_real, aes(color = line), linewidth = 0.8, alpha = 0.55) +
  geom_sf(data = focus_drawn, aes(color = line), linewidth = 2) +
  geom_label(data = focus_roundels, aes(x = X, y = Y, label = line, fill = line), family = f1, fontface = "bold", size = 5, color = clr_bg, border.colour = NA, label.r = unit(0.5, "lines")) +
  scale_color_manual(values = focus_colors) +
  scale_fill_manual(values = focus_colors) +
  coord_sf(xlim = view[c("xmin", "xmax")], ylim = view[c("ymin", "ymax")], crs = crs_se, expand = FALSE) +
  guides(color = "none", fill = "none") +
  theme_map()

save_map(p, here::here("2026/22-projections/22-projections-diagram.png"),
  title = "Uppsala's bus map\\ has no curves",
  subtitle = "Three of the city's bus lines on UL's map (thick) \\ and the streets they take (thin)",
  caption = caption_src("UL, UL GTFS via Trafiklab")
)
