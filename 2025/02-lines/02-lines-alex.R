# Load libraries ----
library(tidyverse)
library(jsonlite)
library(sf)
library(camcorder)

gg_record(here::here("30daymap-temp"), width = 10, height = 6.85, dpi = 320)

uppsala_id <- "i123387679"
robin_crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# Fetch data from OpenAlex API ----
# Get top 100 institutions collaborating with Uppsala University
institutions_raw <- fromJSON(
  paste0(
    "https://api.openalex.org/works?",
    "group_by=authorships.institutions.lineage&",
    "per_page=200&",
    "filter=authorships.institutions.lineage:", uppsala_id
  )
)

institutions <- institutions_raw$group_by |>
  as.data.frame() |>
  head(100) |>
  mutate(inst_id = str_remove(key, "https://openalex.org/"))

# Get geographic coordinates for institutions ----
get_institution_coords <- function(inst_id) {
  url <- paste0("https://api.openalex.org/institutions/", inst_id)
  inst <- fromJSON(url)
  tibble(
    inst_id = inst_id,
    dest_lon = inst$geo$longitude,
    dest_lat = inst$geo$latitude
  )
}

institution_coords <- map_dfr(institutions$inst_id, get_institution_coords)

# Get Uppsala University coordinates
uppsala_coords <- fromJSON(paste0("https://api.openalex.org/institutions/", uppsala_id))

# Combine institution data with coordinates ----
collab_data <- institutions |>
  left_join(institution_coords, by = "inst_id") |>
  mutate(
    origin_lon = uppsala_coords$geo$longitude,
    origin_lat = uppsala_coords$geo$latitude
  )

# Transform coordinates to Robinson projection ----
collab_data_proj <- collab_data |>
  rowwise() |>
  mutate(
    dest_proj = list(st_transform(
      st_sfc(st_point(c(dest_lon, dest_lat)), crs = 4326),
      robin_crs
    )),
    origin_proj = list(st_transform(
      st_sfc(st_point(c(origin_lon, origin_lat)), crs = 4326),
      robin_crs
    )),
    dest_x = st_coordinates(dest_proj)[1],
    dest_y = st_coordinates(dest_proj)[2],
    origin_x = st_coordinates(origin_proj)[1],
    origin_y = st_coordinates(origin_proj)[2]
  ) |>
  ungroup()

# Load world map ----
world <- rnaturalearth::ne_countries(returnclass = "sf", scale = "medium") |>
  select(admin, continent) |>
  filter(admin != "Antarctica") |>
  st_transform(crs = robin_crs)

# Create sf object from destination points ----
destination_points <- st_as_sf(
  collab_data_proj,
  coords = c("dest_x", "dest_y"),
  crs = robin_crs
)

# Identify countries with collaboration points ----
countries_with_collabs <- st_join(
  world,
  st_buffer(destination_points, dist = 1000),
  left = FALSE
)

#  Top institutions per continent for labeling ----
top_institutions <- countries_with_collabs |>
  filter(key_display_name != "Uppsala University") |>
  select(key_display_name, count, admin, continent, dest_proj) |>
  st_drop_geometry() |>
  group_by(continent) |>
  slice_max(order_by = count, n = 5) |>
  ungroup() |>
  mutate(geometry = st_sfc(map(dest_proj, \(x) x[[1]]), crs = robin_crs)) |>
  st_as_sf()


f1 <- "Inclusive Sans"
f2 <- "Publico Headline"
color_bg <- "#f7f7f7"
color_country <- "#cccccc"

# Create plot ----
ggplot() +
  geom_sf(
    data = countries_with_collabs,
    fill = "white",
    color = color_country,
    linewidth = 0.2
  ) +
  geom_point(
    data = collab_data_proj,
    aes(
      x = dest_x,
      y = dest_y,
      size = if_else(key_display_name == "Uppsala University", 2, count)
    ),
    fill = "red",
    alpha = 0.5,
    shape = 21,
    stroke = 0
  ) +
  # geom_curve(
  #   data = collab_data_proj |> filter(origin_y != dest_y & dest_x > 0),
  #   aes(x = dest_x, xend = origin_x, y = dest_y, yend = origin_y),
  #   curvature = 0.2,
  #   linewidth = 0.2,
  #   linetype = "dashed",
  #   alpha = 0.5,
  #   color = "purple4"
  # ) +
  # geom_curve(
  #   data = collab_data_proj |> filter(origin_y != dest_y & dest_x < 0),
  #   aes(x = dest_x, xend = origin_x, y = dest_y, yend = origin_y),
  #   curvature = -0.2,
  #   linewidth = 0.2,
  #   linetype = "dashed",
  #   alpha = 0.5,
  #   color = "purple4"
  # ) +
  ggrepel::geom_label_repel(
    data = top_institutions,
    aes(
      geometry = geometry,
      label = paste0(key_display_name, "\n", scales::number(count), if_else(str_detect(key_display_name, "de la"), " publications", ""))
    ),
    size = 3,
    max.overlaps = 15,
    stat = "sf_coordinates",
    nudge_y = -5e6,
    direction = "y",
    family = f1,
    seed = 999,
    segment.size = 0.3,
    segment.linetype = "dotted",
    segment.color = "purple4",
    label.padding = 0.2,
    label.size = 0,
    fill = alpha("white", 0.9)
  ) +
  scale_size_area(max_size = 6) +
  coord_sf() +
  labs(
    title = "Uppsala University's research partnerships",
    subtitle = "Top 100 institutional collaborations based on co-authored publications",
    caption = "Source: OpenAlex · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = color_bg, color = NA),
    plot.title = element_text(family = f2, size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(hjust = 0.5),
    plot.margin = margin(t = 10.1, b = 10)
  )

record_polaroid()

