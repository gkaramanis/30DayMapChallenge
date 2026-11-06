library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")
dir.create(dir_data, showWarnings = FALSE)

bb_city <- c(17.55, 59.80, 17.76, 59.90)

cached <- function(name, expr) {
  path <- file.path(dir_data, paste0(name, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  out <- expr
  saveRDS(out, path)
  out
}

osm_get <- function(name, bb, features) {
  cached(paste0("osm-", name), {
    res <- imap(features, function(value, key) {
      q <- osmdata::opq(bb, timeout = 600)
      q <- if (all(is.na(value))) {
        osmdata::add_osm_feature(q, key)
      } else {
        osmdata::add_osm_feature(q, key, value)
      }
      osmdata::osmdata_sf(q)
    })
    do.call(c, unname(res))
  })
}

osm_water <- function(bb = bb_city, name = "water-city") {
  w <- osm_get(name, bb, list(natural = "water", waterway = "river"))
  bind_rows(
    w$osm_polygons |> select(osm_id),
    w$osm_multipolygons |> select(osm_id),
    w$osm_lines |>
      filter(waterway == "river") |>
      st_transform(crs_se) |>
      st_buffer(8) |>
      st_transform(4326) |>
      select(osm_id)
  ) |>
    st_transform(crs_se)
}

osm_roads <- function(bb = bb_city, name = "roads-city") {
  osm_get(name, bb, list(highway = c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential", "living_street", "service", "motorway_link", "trunk_link", "primary_link", "secondary_link")))$osm_lines |>
    select(osm_id, any_of(c("name", "highway"))) |>
    st_transform(crs_se)
}

# Two-ink print from the shared palette: amber-cast paper, black ink, amber
# spot colour. tint() mixes col into the paper; amount 0 = paper, 1 = col.
tint <- function(col, amount, paper = clr_paper) colorRampPalette(c(paper, col))(101)[round(amount * 100) + 1]

clr_paper <- tint(clr_accent, 0.12, paper = clr_bg)
clr_ink <- clr_text

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

tram <- cached("ohm-tram", {
  osmdata::set_overpass_url("https://overpass-api.openhistoricalmap.org/api/interpreter")
  res <- osmdata::opq(bb_city, timeout = 300) |>
    osmdata::add_osm_feature("railway", "tram") |>
    osmdata::osmdata_sf()
  osmdata::set_overpass_url("https://overpass-api.de/api/interpreter")
  res
})

tram_lines <- tram$osm_lines |>
  select(osm_id, any_of(c("name", "start_date", "end_date"))) |>
  filter(start_date <= "1926-12-31", end_date >= "1926-01-01") |>
  st_transform(crs_se)

roads <- osm_roads()
water <- osm_water()

area <- st_buffer(st_union(tram_lines), 600) |> st_bbox()

buildings <- cached("buildings-tram-area", {
  pbf <- file.path(dir_data, "r5", "uppsala.osm.pbf")
  tram_wkt <- st_as_text(st_as_sfc(area, crs = crs_se) |> st_transform(4326))
  st_read(pbf, layer = "multipolygons", query = "SELECT building FROM multipolygons WHERE building IS NOT NULL", wkt_filter = tram_wkt, quiet = TRUE)
}) |>
  st_transform(crs_se)

p <- ggplot() +
  geom_sf(data = buildings, fill = tint(clr_ink, 0.10), color = tint(clr_ink, 0.22), linewidth = 0.08) +
  geom_sf(data = water, fill = tint(clr_accent2, 0.25), color = tint(clr_accent2, 0.55), linewidth = 0.35) +
  geom_sf(data = roads, color = tint(clr_ink, 0.25), linewidth = 0.2) +
  geom_sf(data = tram_lines, color = clr_ink, linewidth = 2.0) +
  geom_sf(data = tram_lines, color = clr_accent, linewidth = 0.8, linetype = "11") +
  coord_map_box(area) +
  theme_map(bg = clr_paper, text = clr_ink)

save_map(p, here::here("2026/06-vintage/06-vintage-tram.png"), 
    title = "UPPSALA TRAMWAYS\\ 1926", 
    subtitle = "The tram network a century ago, drawn on today's\\ streets. A new tramway is due to open in 2029",
    caption = caption_src("OpenHistoricalMap, OpenStreetMap")
)
