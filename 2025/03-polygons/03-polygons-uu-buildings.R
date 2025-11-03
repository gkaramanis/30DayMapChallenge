library(tidyverse)
library(patchwork)
library(sf)
library(osmdata)
library(camcorder)

gg_record(here::here("30daymap-temp"), width = 9.5, height = 8, dpi = 320)

# Uppsala, Sweden
bb <- c(xmin = 17.55, ymin = 59.8, xmax = 17.75, ymax = 59.9)

q <- opq(bbox = bb) %>%
  add_osm_feature(key = "building")

osm_buildings <- osmdata_sf(q)
buildings <- osm_buildings$osm_polygons

polys <- buildings |>
  filter(building %in% c("university")) |>
  filter(!is.na(name)) |> 
  mutate(name = case_when(
    str_detect(name, "[A-Z]\\d+") ~ "Biomedicinskt centrum",
    str_detect(name, "Veterinär") ~ "VHC",
    str_detect(name, "Paleontologiska") ~ "Paleontologiska inst.",
    TRUE ~ name
    ),
  name = str_wrap(name, 25)
  ) |> 
  group_by(name) |>
  summarize(geometry = st_union(geometry)) |>
  st_make_valid() |>
  st_cast("POLYGON") |>
  ungroup() |> 
  mutate(area = st_area(geometry)) |> 
  arrange(-area) |> 
  mutate(poly_id = row_number()) |>
  group_split(poly_id)

# Calculate max width and height among all polygons
all_bboxes <- lapply(polys, st_bbox)
widths <- sapply(all_bboxes, function(b) b["xmax"] - b["xmin"])
heights <- sapply(all_bboxes, function(b) b["ymax"] - b["ymin"])
max_width <- max(widths)
max_height <- max(heights)

f1 <- "Inclusive Sans"
f2 <- "Publico Headline"

plots <- lapply(polys, function(poly) {
  bbox <- st_bbox(poly)
  centroid <- st_coordinates(st_centroid(st_union(poly$geometry)))
  center_x <- centroid[1]
  center_y <- centroid[2]
  xlim <- c(center_x - max_width/2, center_x + max_width/2)
  ylim <- c(center_y - max_height/2, center_y + max_height/2)
  plot_bbox <- st_bbox(c(xmin = xlim[1], xmax = xlim[2], ymin = ylim[1], ymax = ylim[2]), crs = st_crs(poly))
  buildings_cropped <- st_crop(buildings, plot_bbox)
  
  ggplot() +
    geom_sf(data = buildings_cropped, fill = "grey85", color = NA) +
    geom_sf(data = poly, fill = "#ff4d00", color = "black") +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_void(base_family = f1) +
    labs(
      caption = unique(poly$name)
    ) +
    theme(
      plot.background = element_rect(fill = "grey99", color = NA),
      plot.caption = element_text(hjust = 0.5, vjust = 0, size = 9, margin = margin(5, 0, 5, 0)),
      plot.margin = margin(5, 20, 5, 20),
      panel.background = element_rect(fill = "grey97", color = NA)
    )
})

wrap_plots(plots, ncol = 5) +
  plot_annotation(
    title = "Uppsala University buildings",
    caption = "Source: OpenStreetMap · Graphic: Georgios Karamanis",
    theme = theme(
      plot.background = element_rect(fill = "grey99", color = NA),
      plot.title = element_text(size = 15, family = f2, face = "bold", hjust = 0.5, margin = margin(0, 0, 10, 0)),
      plot.caption = element_text(size = 8, family = f1, hjust = 0.5)
      )
  )

# record_polaroid()
