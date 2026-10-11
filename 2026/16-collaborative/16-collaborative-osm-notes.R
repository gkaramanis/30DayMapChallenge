library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = map_width, height = map_height, units = "in", dpi = 320)

# OSM Notes: problems reported by anyone, fixed by other mappers
notes <- cached("osm-notes-city", {
  url <- paste0(
    "https://api.openstreetmap.org/api/0.6/notes.json?limit=10000&closed=-1&bbox=",
    paste(bb_city, collapse = ",")
  )
  jsonlite::fromJSON(url, simplifyVector = FALSE)$features |>
    map(function(f) {
      tibble(
        id = f$properties$id,
        status = f$properties$status,
        created = as.Date(substr(f$properties$date_created, 1, 10)),
        text = paste(map_chr(f$properties$comments, ~ .x$text %||% ""), collapse = " "),
        lon = f$geometry$coordinates[[1]],
        lat = f$geometry$coordinates[[2]]
      )
    }) |>
    list_rbind()
})

pattern <- "cykel|cykl|bike|cycl|buss|bus|hållplats|stop|parkering|parking|väg|gata|road|street|korsning|övergångsställe|crossing"

transport_notes <- notes |>
  filter(str_detect(str_to_lower(text), pattern)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(crs_se) |>
  mutate(status = if_else(status == "open", "Still open", "Fixed by someone"))

city <- bb_sf(bb_city)
roads <- osm_roads()
water <- osm_water()

ggplot() +
  geom_sf(data = water, fill = clr_water, color = NA) +
  geom_sf(data = roads, color = clr_grid, linewidth = 0.2) +
  geom_sf(data = transport_notes, aes(color = status), size = 2.4, alpha = 0.85) +
  scale_color_manual(values = c("Still open" = clr_accent, "Fixed by someone" = clr_accent2)) +
  coord_sf(xlim = st_bbox(city)[c(1, 3)], ylim = st_bbox(city)[c(2, 4)], expand = FALSE) +
  labs(
    title = "Mapping Uppsala together",
    subtitle = paste0(nrow(transport_notes), " OpenStreetMap notes about roads,\nbikes, buses and parking.\nIndigo ones were fixed by another mapper"),
    caption = caption_src("OpenStreetMap Notes API"),
    color = NULL
  ) +
  guides(color = guide_legend(override.aes = list(size = 4, alpha = 1))) +
  theme_map()

record_polaroid()
