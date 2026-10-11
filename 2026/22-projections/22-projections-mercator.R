library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = map_width, height = map_height, units = "in", dpi = 320)

kommun <- uppsala_kommun()

uppsala_c <- c(17.6459, 59.8586)
stockholm_c <- c(18.0575, 59.3300)
trip <- st_sfc(st_linestring(rbind(uppsala_c, stockholm_c)), crs = 4326)

# Project, measure in projected units and shift so Uppsala C sits at the origin
measure <- function(crs, name) {
  origin <- st_transform(st_sfc(st_point(uppsala_c), crs = 4326), crs) |> st_coordinates() |> as.numeric()
  shift <- function(x) st_set_crs(st_geometry(st_transform(x, crs)) - origin, NA)
  k <- shift(kommun)
  t <- shift(trip)
  end <- st_coordinates(t)[2, 1:2]
  tibble(
    projection = name,
    kommun = k,
    trip = t,
    area_km2 = as.numeric(st_area(k)) / 1e6,
    length_km = as.numeric(st_length(t)) / 1e3,
    end_x = end[[1]],
    end_y = end[[2]]
  )
}

proj <- bind_rows(
  measure(3857, "Web Mercator"),
  measure(3006, "SWEREF 99 TM")
) |>
  mutate(
    projection = fct_inorder(projection),
    label = paste0(projection, "\n", scales::number(area_km2, big.mark = " "), " km², ", round(length_km), " km")
  )

kommun_sf <- st_sf(proj["projection"], geometry = proj$kommun)
trip_sf <- st_sf(proj["projection"], geometry = proj$trip)
labels <- proj |>
  st_drop_geometry() |>
  select(projection, label, end_x, end_y)

ggplot() +
  geom_sf(data = kommun_sf, aes(fill = projection), color = NA, alpha = 0.7) +
  geom_sf(data = trip_sf, aes(color = projection), linewidth = 1, linetype = "dashed") +
  geom_text(data = labels, aes(x = end_x, y = end_y, label = label, color = projection), family = f1, fontface = "bold", size = 6, lineheight = 1, hjust = 1, nudge_x = -9000, show.legend = FALSE) +
  annotate("point", x = 0, y = 0, size = 3, color = clr_text) +
  annotate("text", x = 0, y = 12000, label = "Uppsala C", family = f1, size = 6, color = clr_text, vjust = 0) +
  scale_fill_manual(values = c("Web Mercator" = clr_accent, "SWEREF 99 TM" = clr_accent2)) +
  scale_color_manual(values = c("Web Mercator" = colorspace::darken(clr_accent, 0.2), "SWEREF 99 TM" = clr_accent2)) +
  coord_sf(datum = NA, expand = TRUE) +
  labs(
    title = "Same kommun,\nfour times the size",
    subtitle = "At 60°N Web Mercator stretches distances\nby about 2× and areas by about 4×",
    caption = caption_src("OpenStreetMap"),
    fill = NULL
  ) +
  guides(color = "none", fill = guide_legend(override.aes = list(alpha = 1))) +
  theme_map()

record_polaroid()
