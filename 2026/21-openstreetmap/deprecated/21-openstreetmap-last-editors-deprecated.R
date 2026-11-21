library(tidyverse)
library(sf)

source(here::here("2026/R/helpers.R"))

bb_city <- c(xmin = 17.50, ymin = 59.78, xmax = 17.78, ymax = 59.92)

# Overpass returns each way's geometry with its last editor (out meta geom)
ways_file <- file.path(dir_2026, "data", "osm-highways-meta-city.rds")

if (!file.exists(ways_file)) {
  query <- str_glue("[out:json][timeout:300];way[\"highway\"]({bb_city[['ymin']]},{bb_city[['xmin']]},{bb_city[['ymax']]},{bb_city[['xmax']]});out meta geom;")
  resp <- httr2::request("https://overpass-api.de/api/interpreter") |> httr2::req_user_agent("osm-map-script/1.0") |> httr2::req_body_form(data = query) |> httr2::req_timeout(360) |> httr2::req_perform()
  elements <- httr2::resp_body_json(resp)$elements
  geoms <- map(elements, \(el) st_linestring(do.call(rbind, map(el$geometry, \(pt) c(pt$lon, pt$lat)))))
  tibble(id = map_dbl(elements, "id"), user = map_chr(elements, "user"), version = map_int(elements, "version"), timestamp = map_chr(elements, "timestamp"), highway = map_chr(elements, \(el) el$tags$highway)) |> st_sf(geometry = st_sfc(geoms, crs = 4326)) |> saveRDS(ways_file)
}

city <- st_bbox(bb_city, crs = 4326) |> st_as_sfc() |> st_transform(crs_se)

ways <- readRDS(ways_file) |> st_transform(crs_se) |> st_filter(city)

top_users <- ways |> st_drop_geometry() |> count(user, sort = TRUE) |> slice_head(n = 3) |> pull(user)

ways_editor <- ways |> mutate(editor = factor(if_else(user %in% top_users, user, "Everyone else"), levels = c(top_users, "Everyone else"))) |> arrange(desc(editor))

share_top <- weighted.mean(ways_editor$editor != "Everyone else", as.numeric(st_length(ways_editor)))

# As tall as the city, cropped at the sides to the map box shape
city_bb <- st_bbox(city)
view <- map_box_view(c(mean(city_bb[c("xmin", "xmax")]), mean(city_bb[c("ymin", "ymax")])), (city_bb[["ymax"]] - city_bb[["ymin"]]) * map_box$width / map_box$height)

clr_editors <- set_names(c(clr_accent, clr_accent2, "#C2453A", colorspace::lighten(clr_text2, 0.6)), levels(ways_editor$editor))

p <- ggplot(ways_editor) +
  geom_sf(aes(color = editor, linewidth = editor == "Everyone else")) +
  scale_color_manual(values = clr_editors, breaks = top_users, name = "Last edited by") +
  scale_linewidth_manual(values = c("TRUE" = 0.15, "FALSE" = 0.45), guide = "none") +
  coord_sf(xlim = view[c("xmin", "xmax")], ylim = view[c("ymin", "ymax")], crs = crs_se, expand = FALSE) +
  guides(color = guide_legend(override.aes = list(linewidth = 2))) +
  theme_map()

save_map(p, here::here("2026/21-openstreetmap/21-openstreetmap-last-editors.png"),
  title = paste0("Three users last edited\\ ", scales::percent(share_top, accuracy = 1), " of Uppsala's roads"),
  subtitle = "OpenStreetMap roads and paths by last editor,\\ share by length",
  caption = caption_src("OpenStreetMap")
)
