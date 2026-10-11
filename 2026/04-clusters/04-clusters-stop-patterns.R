library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = map_width, height = map_height, units = "in", dpi = 320)

gtfs <- ul_gtfs()
on <- typical_weekday(gtfs)

# Hourly departure profile per stop, 05-24
profiles <- stop_times_on(gtfs, on) |>
  mutate(hour = floor(dep_s / 3600) %% 24) |>
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
k <- 5
km <- kmeans(log1p(as.matrix(wide[, -1])), centers = k, nstart = 25)

clusters <- tibble(stop_id = wide$stop_id, cluster = factor(km$cluster))

# Name clusters by mean daily departures, lowest to highest
cluster_order <- profiles |>
  left_join(clusters, by = "stop_id") |>
  group_by(cluster, stop_id) |>
  summarise(n = sum(n), .groups = "drop") |>
  group_by(cluster) |>
  summarise(mean_dep = mean(n)) |>
  arrange(mean_dep) |>
  mutate(label = paste0("~", round(mean_dep), " departures/day"))

clusters <- clusters |>
  left_join(cluster_order, by = "cluster") |>
  mutate(label = fct_reorder(label, mean_dep))

kommun <- uppsala_kommun() |> st_transform(crs_se)

stops <- stops_sf(gtfs) |>
  inner_join(clusters, by = "stop_id") |>
  st_transform(crs_se) |>
  st_filter(kommun)

pal <- c(
  clr_text2,
  colorspace::lighten(clr_accent2, 0.4),
  clr_accent2,
  colorspace::lighten(clr_accent, 0.25),
  clr_accent
)

p_map <- ggplot() +
  geom_sf(data = kommun, fill = clr_land, color = clr_grid, linewidth = 0.3) +
  geom_sf(data = stops |> arrange(mean_dep), aes(color = label), size = 1.1) +
  scale_color_manual(values = pal) +
  coord_sf(expand = FALSE) +
  theme_void(base_family = f1) +
  theme(legend.position = "none")

p_profiles <- profiles |>
  left_join(clusters, by = "stop_id") |>
  group_by(label, hour) |>
  summarise(n = mean(n), .groups = "drop") |>
  ggplot(aes(hour, n, color = label)) +
  geom_line(linewidth = 1.3) +
  scale_color_manual(values = pal) +
  scale_x_continuous(breaks = c(6, 12, 18)) +
  facet_wrap(vars(label), nrow = 1, scales = "free_y", labeller = labeller(label = \(x) str_wrap(x, 11))) +
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
    plot.margin = margin(t = 10)
  )

patchwork::wrap_plots(p_map, p_profiles, ncol = 1, heights = c(2.2, 1)) +
  patchwork::plot_annotation(
    title = "Stops that keep\nthe same rhythm",
    subtitle = "UL stops in Uppsala kommun clustered (k-means)\nby their hourly departure profile",
    caption = caption_src("UL GTFS via Trafiklab"),
    theme = theme_map()
  )

record_polaroid()
