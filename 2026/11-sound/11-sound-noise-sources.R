library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

dir_data <- file.path(dir_2026, "data")
dir.create(dir_data, showWarnings = FALSE, recursive = TRUE)

# Evaluate expr once and cache the result as rds in data/
cached <- function(name, expr) {
  path <- file.path(dir_data, paste0(name, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  out <- expr
  saveRDS(out, path)
  out
}

# Central Uppsala, 14 km across, kept inside the kommun border
centre <- st_sfc(st_point(c(17.645, 59.864)), crs = 4326) |>
  st_transform(crs_se) |>
  st_coordinates()

view <- map_box_view(centre, 14000)

# EEA noise contours reported under the Environmental Noise Directive, 2022
# round, Lnight (night, 22–06). Sweden's GeoPackage is 7 GB; GDAL reads only the view over
# HTTP range requests. https://doi.org/10.2909/853e72e4-4642-47d1-8556-2cc231bd43e0
# Uppsala kommun is reported as an agglomeration, so all its roads are mapped
eea_gpkg <- "/vsicurl/https://sdi.eea.europa.eu/datashare/public.php/webdav/SE.gpkg"

noise_layers <- c("NoiseContours_majorRoadsIncludingAgglomeration_Lnight", "NoiseContours_roadsInAgglomeration_Lnight")

noise_area <- st_as_sfc(view) |>
  st_transform(3035) |>
  st_as_text()

noise <- cached("eea-noise-2022-lnight-uppsala-city-road", {
  withr::with_envvar(c(GDAL_HTTP_AUTH = "BASIC", GDAL_HTTP_USERPWD = "8GaXK7iYzCbPreb:", CPL_VSIL_CURL_CHUNK_SIZE = "1048576", GDAL_HTTP_MULTIRANGE = "NO"), {
    map(noise_layers, \(layer) read_sf(eea_gpkg, layer, wkt_filter = noise_area) |> select(category)) |>
      list_rbind() |>
      st_as_sf()
  })
})

kommun <- readRDS(file.path(dir_data, "boundary-uppsala_kommun.rds")) |>
  st_transform(crs_se)

# 50 dB and up; the WHO guideline for road noise at night is 45 dB.
# The loudest band draws last
bands <- c(Lnight5054 = "50", Lnight5559 = "55", Lnight6064 = "60", Lnight6569 = "65", LnightGreaterThan70 = "70+")

loud <- noise |>
  filter(category %in% names(bands)) |>
  mutate(band = factor(bands[category], levels = bands)) |>
  st_transform(crs_se) |>
  st_make_valid() |>
  summarise(geometry = st_union(geom), .by = band) |>
  st_intersection(st_as_sfc(view)) |>
  arrange(band)

p <- ggplot() +
  geom_sf(data = kommun, fill = clr_land, color = NA) +
  geom_sf(data = loud, aes(fill = band), color = NA) +
  scale_fill_manual(values = colorRampPalette(c("#C9CFEA", clr_accent2, "#141A40"))(5), name = "Road noise at night, dB", guide = guide_legend(nrow = 1, label.position = "bottom", keywidth = unit(12, "mm"), keyheight = unit(4, "mm"))) +
  coord_map_box(view) +
  theme_map() +
  theme(legend.background = element_rect(fill = clr_bg, color = NA), legend.margin = margin(8, 12, 8, 12))

save_map(p, here::here("2026/11-sound/11-sound-noise-sources.png"),
  title = "The city goes quiet at night,\\ but its main roads don't",
  subtitle = "Road noise at night above 50 dB,\\ 5 dB above the WHO guideline",
  caption = caption_src("EEA, OpenStreetMap")
)
