library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = 9, height = 9, dpi = 320)

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
  geom_sf(data = water, fill = "#dbe9f0", color = NA) +
  geom_sf(data = roads, color = "grey82", linewidth = 0.15) +
  geom_sf(data = transport_notes, aes(color = status), size = 1.8, alpha = 0.8) +
  scale_color_manual(values = c("Still open" = "#e63946", "Fixed by someone" = "#457b9d")) +
  coord_sf(xlim = st_bbox(city)[c(1, 3)], ylim = st_bbox(city)[c(2, 4)], expand = FALSE) +
  labs(
    title = "Mapping Uppsala together",
    subtitle = paste0(nrow(transport_notes), " OpenStreetMap notes about roads, bikes, buses and parking. Blue ones were fixed by another mapper"),
    caption = caption_src("OpenStreetMap Notes API"),
    color = NULL
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "top",
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(family = f2, size = 18, face = "bold"),
    plot.subtitle = element_text(size = 10, margin = margin(5, 0, 10, 0)),
    plot.caption = element_text(size = 8),
    plot.margin = margin(10, 10, 10, 10)
  )

record_polaroid()
