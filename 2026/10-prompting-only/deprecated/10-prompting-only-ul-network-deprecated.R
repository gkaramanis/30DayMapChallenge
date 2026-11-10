# Written end to end by Claude Code from prompts only.

library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")
dir.create(dir_data, showWarnings = FALSE, recursive = TRUE)

bb_lan <- c(16.70, 59.30, 18.95, 60.75)

# Evaluate expr once and cache the result as rds in data/
cached <- function(name, expr) {
  path <- file.path(dir_data, paste0(name, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  out <- expr
  saveRDS(out, path)
  out
}

osm_boundary <- function(name, admin_level, bb) {
  cached(paste0("boundary-", janitor::make_clean_names(name)), {
    osmdata::opq(bb, timeout = 300) |>
      osmdata::add_osm_feature("boundary", "administrative") |>
      osmdata::add_osm_feature("admin_level", admin_level) |>
      osmdata::add_osm_feature("name", name) |>
      osmdata::osmdata_sf() |>
      pluck("osm_multipolygons") |>
      select(name) |>
      st_make_valid()
  })
}

uppsala_lan <- function() osm_boundary("Uppsala län", "4", bb_lan)

# Keys from https://developer.trafiklab.se, stored in ~/.Renviron:
# TRAFIKLAB_GTFS_REGIONAL_KEY, TRAFIKLAB_GTFS_SWEDEN_KEY
gtfs_download <- function(url, file) {
  path <- file.path(dir_data, file)
  if (!file.exists(path)) {
    download.file(url, path, mode = "wb", headers = c("Accept-Encoding" = "gzip"))
  }
  path
}

ul_gtfs_path <- function() {
  key <- Sys.getenv("TRAFIKLAB_GTFS_REGIONAL_KEY")
  stopifnot("Set TRAFIKLAB_GTFS_REGIONAL_KEY" = key != "" || file.exists(file.path(dir_data, "ul.zip")))
  gtfs_download(paste0("https://opendata.samtrafiken.se/gtfs/ul/ul.zip?key=", key), "ul.zip")
}

ul_gtfs <- function() tidytransit::read_gtfs(ul_gtfs_path())

# service_ids running on a date, from calendar and calendar_dates
services_on <- function(gtfs, on) {
  day <- c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")[lubridate::wday(on, week_start = 1)]
  ids <- character()
  if (!is.null(gtfs$calendar) && nrow(gtfs$calendar) > 0) {
    ids <- gtfs$calendar |>
      filter(start_date <= on, end_date >= on, .data[[day]] == 1) |>
      pull(service_id)
  }
  if (!is.null(gtfs$calendar_dates)) {
    cd <- gtfs$calendar_dates |> filter(date == on)
    ids <- union(setdiff(ids, cd$service_id[cd$exception_type == 2]), cd$service_id[cd$exception_type == 1])
  }
  ids
}

# Bus route types: 3 and extended 700-799
is_bus <- function(route_type) route_type == 3 | between(route_type, 700, 799)


gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

gtfs <- ul_gtfs()
on <- as.Date("2026-11-10")
stopifnot("No service on this date" = length(services_on(gtfs, on)) > 0)

# City buses are lines under 100; the UL feed has no train routes
route_kind <- gtfs$routes |>
  mutate(num = suppressWarnings(as.numeric(route_short_name)), kind = case_when(is_bus(route_type) & num < 100 ~ "City bus", is_bus(route_type) ~ "Regional bus", TRUE ~ "Other")) |>
  select(route_id, kind)

shape_kind <- gtfs$trips |>
  filter(service_id %in% services_on(gtfs, on), !is.na(shape_id)) |>
  left_join(route_kind, by = "route_id") |>
  count(shape_id, kind, name = "trips")

shapes <- tidytransit::shapes_as_sf(gtfs$shapes) |>
  inner_join(shape_kind, by = "shape_id") |>
  filter(kind != "Other") |>
  st_transform(crs_se) |>
  mutate(kind = factor(kind, levels = c("Regional bus", "City bus"))) |>
  arrange(kind)

lan <- uppsala_lan() |> st_transform(crs_se)

p <- ggplot() +
  geom_sf(data = lan, fill = clr_land, color = clr_grid, linewidth = 0.3) +
  geom_sf(data = shapes, aes(color = kind, linewidth = trips), alpha = 0.8) +
  scale_color_manual(values = c("City bus" = clr_accent, "Regional bus" = clr_accent2)) +
  scale_linewidth(range = c(0.15, 1.5), guide = "none") +
  coord_map_box(st_bbox(lan)) +
  guides(color = guide_legend(title = NULL, override.aes = list(linewidth = 1.5))) +
  theme_map() +
  theme(legend.text = element_text(margin = margin(l = 7, r = 20)), legend.key.height = unit(8, "mm"), legend.key.width = unit(12, "mm"))

save_map(p, here::here("2026/10-prompting-only/10-prompting-only-ul-network.png"), title = "UL, all of it", subtitle = "Every bus route running on \\ Tuesday 10 November 2026, width by trips", caption = paste(caption_src("UL GTFS via Trafiklab"), "· Code by Claude Code from prompts only"))
