library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")
dir.create(dir_data, showWarnings = FALSE)

# Evaluate expr once and cache the result as rds in data/
cached <- function(name, expr) {
  path <- file.path(dir_data, paste0(name, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  saveRDS(expr, path)
  expr
}

kommun <- cached("boundary-uppsala_kommun", {
  osmdata::opq(c(17.15, 59.60, 18.15, 60.20), timeout = 300) |>
    osmdata::add_osm_feature("boundary", "administrative") |>
    osmdata::add_osm_feature("admin_level", "7") |>
    osmdata::add_osm_feature("name", "Uppsala kommun") |>
    osmdata::osmdata_sf() |>
    pluck("osm_multipolygons") |>
    select(name) |>
    st_make_valid()
}) |>
  st_transform(crs_se)

# Key from https://developer.trafiklab.se, stored in ~/.Renviron
ul_zip <- file.path(dir_data, "ul.zip")
if (!file.exists(ul_zip)) {
  key <- Sys.getenv("TRAFIKLAB_GTFS_REGIONAL_KEY")
  download.file(
    paste0("https://opendata.samtrafiken.se/gtfs/ul/ul.zip?key=", key),
    ul_zip, mode = "wb", headers = c("Accept-Encoding" = "gzip")
  )
}
gtfs <- tidytransit::read_gtfs(ul_zip)
on <- as.Date("2026-11-04")

feed_on <- tidytransit::filter_feed_by_date(gtfs, on)

# Hourly departure profile per stop, 05-24
profiles <- feed_on$stop_times |>
  mutate(hour = floor(as.numeric(departure_time) / 3600) %% 24) |>
  filter(hour >= 5) |>
  count(stop_id, hour) |>
  complete(stop_id, hour = 5:23, fill = list(n = 0)) |>
  group_by(stop_id) |>
  filter(sum(n) >= 10) |>
  ungroup()

wide <- profiles |>
  pivot_wider(names_from = hour, values_from = n)

# Cluster on log-scaled profiles so level and shape both count
set.seed(99)
km <- kmeans(log1p(as.matrix(wide[, -1])), centers = 5, nstart = 25)

clusters <- tibble(stop_id = wide$stop_id, cluster = factor(km$cluster))

# Name clusters by mean daily departures, lowest to highest
cluster_order <- profiles |>
  left_join(clusters, by = join_by(stop_id)) |>
  summarise(n = sum(n), .by = c(cluster, stop_id)) |>
  summarise(mean_dep = mean(n), .by = cluster) |>
  arrange(mean_dep) |>
  mutate(label = paste0("~", round(mean_dep), " departures/day"))

clusters <- clusters |>
  left_join(cluster_order, by = join_by(cluster)) |>
  mutate(label = fct_reorder(label, mean_dep))

stops <- gtfs$stops |>
  filter(!is.na(stop_lat)) |>
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) |>
  inner_join(clusters, by = join_by(stop_id)) |>
  st_transform(crs_se) |>
  st_filter(kommun)

pal <- c(
  clr_text2,
  colorspace::lighten(clr_accent2, 0.4),
  clr_accent2,
  colorspace::lighten(clr_accent, 0.25),
  clr_accent
)

gg_record(
  here::here("30daymap-temp"),
  width = map_box$width, height = map_box$height, units = "in", dpi = 320
)

p_profiles <- profiles |>
  left_join(clusters, by = join_by(stop_id)) |>
  summarise(n = mean(n), .by = c(label, hour)) |>
  ggplot(aes(hour, n, color = label)) +
  geom_line(linewidth = 1.3) +
  scale_color_manual(values = pal) +
  scale_x_continuous(breaks = c(6, 12, 18)) +
  facet_wrap(
    vars(label), nrow = 1, scales = "free_y",
    labeller = labeller(label = \(x) str_wrap(x, 11))
  ) +
  labs(x = "Hour of day", y = NULL) +
  theme_minimal(base_family = f1, base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = clr_grid, linewidth = 0.3),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = clr_text2),
    axis.title.x = element_text(color = clr_text2, margin = margin(t = 6)),
    strip.text = element_text(size = 11, lineheight = 1.05, color = clr_text, vjust = 1),
    panel.spacing = unit(0.8, "lines"),
    plot.background = element_blank(),
    plot.margin = margin(t = 10, r = 8, b = 6, l = 8)
  )

bb_kommun <- st_bbox(kommun)
# Free the top of the panel for the profiles
bb_panel <- bb_kommun + c(0, 0, 0, 30000)

p <- ggplot() +
  geom_sf(data = kommun, fill = clr_land, color = clr_grid, linewidth = 0.3) +
  geom_sf(data = arrange(stops, mean_dep), aes(color = label), size = 1.1) +
  annotation_custom(
    ggplotGrob(p_profiles),
    xmin = bb_kommun[["xmin"]], xmax = bb_kommun[["xmax"]],
    ymin = bb_kommun[["ymax"]] + 1500, ymax = bb_kommun[["ymax"]] + 30000
  ) +
  scale_color_manual(values = pal) +
  coord_map_box(bb_panel) +
  theme_map() +
  theme(legend.position = "none")

save_map(
  p, here::here("2026/04-clusters/04-clusters-stop-patterns.png"),
  title = "Stops that keep\\ the same rhythm",
  subtitle = "UL stops in Uppsala kommun clustered (k-means) \\ by their hourly departure profile \\ on Wednesday 4 November 2026",
  caption = caption_src("UL GTFS via Trafiklab")
)
