library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = 10, height = 8, dpi = 320)

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

pal <- MetBrewer::met.brewer("Archambault", k)

p_map <- ggplot() +
  geom_sf(data = kommun, fill = "white", color = "grey70", linewidth = 0.2) +
  geom_sf(data = stops |> arrange(mean_dep), aes(color = label), size = 0.8) +
  scale_color_manual(values = pal) +
  coord_sf(expand = FALSE) +
  theme_void(base_family = f1) +
  theme(legend.position = "none")

p_profiles <- profiles |>
  left_join(clusters, by = "stop_id") |>
  group_by(label, hour) |>
  summarise(n = mean(n), .groups = "drop") |>
  ggplot(aes(hour, n, color = label)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = pal) +
  facet_wrap(vars(label), ncol = 1, scales = "free_y") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = f1) +
  theme(legend.position = "none", panel.grid.minor = element_blank())

patchwork::wrap_plots(p_map, p_profiles, widths = c(2.5, 1)) +
  patchwork::plot_annotation(
    title = "Stops that keep the same rhythm",
    subtitle = "UL stops in Uppsala kommun clustered (k-means) by their hourly departure profile",
    caption = caption_src("UL GTFS via Trafiklab"),
    theme = theme_void(base_family = f1) +
      theme(
        plot.background = element_rect(fill = "#f7f5f0", color = NA),
        plot.title = element_text(family = f2, size = 18, face = "bold"),
        plot.subtitle = element_text(size = 11, margin = margin(5, 0, 10, 0)),
        plot.margin = margin(10, 10, 10, 10)
      )
  )

record_polaroid()
