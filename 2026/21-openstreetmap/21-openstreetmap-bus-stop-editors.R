library(tidyverse)
library(sf)

source(here::here("2026/R/helpers.R"))

bb_city <- c(xmin = 17.50, ymin = 59.78, xmax = 17.78, ymax = 59.92)

# Bus stops and bus platforms; stop positions and stop areas would count each stop twice
stops_file <- file.path(dir_2026, "data", "osm-stops-meta-city.rds")

if (!file.exists(stops_file)) {
  bb_q <- str_glue("{bb_city[['ymin']]},{bb_city[['xmin']]},{bb_city[['ymax']]},{bb_city[['xmax']]}")
  query <- str_glue("[out:json][timeout:300];(nw[\"highway\"=\"bus_stop\"]({bb_q});nw[\"public_transport\"=\"platform\"][\"bus\"=\"yes\"]({bb_q}););out meta center;")
  resp <- httr2::request("https://overpass-api.de/api/interpreter") |> httr2::req_user_agent("osm-map-script/1.0") |> httr2::req_body_form(data = query) |> httr2::req_timeout(360) |> httr2::req_retry(max_tries = 4) |> httr2::req_perform()
  elements <- httr2::resp_body_json(resp)$elements
  tibble(type = map_chr(elements, "type"), id = map_dbl(elements, "id"), user = map_chr(elements, "user"), lon = map_dbl(elements, \(el) el$lon %||% el$center$lon), lat = map_dbl(elements, \(el) el$lat %||% el$center$lat), name = map_chr(elements, \(el) el$tags$name %||% NA_character_)) |> st_as_sf(coords = c("lon", "lat"), crs = 4326) |> saveRDS(stops_file)
}

city <- st_bbox(bb_city, crs = 4326) |> st_as_sfc() |> st_transform(crs_se)

stops <- readRDS(stops_file) |> st_transform(crs_se) |> st_filter(city)

roads_file <- file.path(dir_2026, "data", "osm-highways-city.rds")

if (!file.exists(roads_file)) {
  query <- str_glue("[out:json][timeout:300];way[\"highway\"]({bb_city[['ymin']]},{bb_city[['xmin']]},{bb_city[['ymax']]},{bb_city[['xmax']]});out geom;")
  resp <- httr2::request("https://overpass-api.de/api/interpreter") |> httr2::req_user_agent("osm-map-script/1.0") |> httr2::req_body_form(data = query) |> httr2::req_timeout(360) |> httr2::req_retry(max_tries = 4) |> httr2::req_perform()
  elements <- httr2::resp_body_json(resp)$elements
  geoms <- map(elements, \(el) st_linestring(do.call(rbind, map(el$geometry, \(pt) c(pt$lon, pt$lat)))))
  tibble(id = map_dbl(elements, "id")) |> st_sf(geometry = st_sfc(geoms, crs = 4326)) |> saveRDS(roads_file)
}

roads <- readRDS(roads_file) |> st_transform(crs_se) |> st_filter(city)

top_users <- stops |> st_drop_geometry() |> count(user, sort = TRUE) |> slice_head(n = 3) |> pull(user)

stops_editor <- stops |> mutate(editor = factor(if_else(user %in% top_users, user, "Everyone else"), levels = c(top_users, "Everyone else"))) |> arrange(desc(editor))

share_first <- mean(stops$user == top_users[1])

city_bb <- st_bbox(city)
view <- map_box_view(c(mean(city_bb[c("xmin", "xmax")]), mean(city_bb[c("ymin", "ymax")])), (city_bb[["ymax"]] - city_bb[["ymin"]]) * map_box$width / map_box$height)

# Background-colored ring keeps overlapping stops apart
p <- ggplot() +
  geom_sf(data = roads, color = colorspace::darken(clr_grid, 0.1), linewidth = 0.15) +
  geom_sf(data = stops_editor, aes(fill = editor, size = editor == "Everyone else"), shape = 21, color = clr_bg, stroke = 0.5) +
  scale_fill_manual(values = set_names(c(clr_accent, clr_accent2, "#C2453A", colorspace::lighten(clr_text2, 0.3)), levels(stops_editor$editor)), breaks = top_users, name = "Last edited by") +
  scale_size_manual(values = c("FALSE" = 3, "TRUE" = 1.8), guide = "none") +
  coord_sf(xlim = view[c("xmin", "xmax")], ylim = view[c("ymin", "ymax")], crs = crs_se, expand = FALSE) +
  guides(fill = guide_legend(override.aes = list(size = 5))) +
  theme_map() +
  theme(legend.title = shadowtext::element_shadowtext(size = 14, hjust = 0, color = clr_text, bg.color = clr_bg), legend.text = shadowtext::element_shadowtext(size = 13, color = clr_text, bg.color = clr_bg))

save_map(p, here::here("2026/21-openstreetmap/21-openstreetmap-bus-stop-editors.png"),
  title = paste0("One user last edited\\ ", scales::percent(share_first, accuracy = 1), " of Uppsala's bus stops"),
  subtitle = "OpenStreetMap bus stops in Uppsala,\\ colored by the user who last edited them",
  caption = caption_src("OpenStreetMap")
)
