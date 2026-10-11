library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = 12, height = 7, dpi = 320)

city <- bb_sf(bb_city)

roads <- osm_roads() |>
  st_filter(city) |>
  mutate(
    maxspeed = tag(pick(everything()), "maxspeed"),
    surface = tag(pick(everything()), "surface")
  )

roads_long <- roads |>
  select(maxspeed, surface) |>
  pivot_longer(c(maxspeed, surface), names_to = "tag", values_to = "value") |>
  mutate(
    is_null = is.na(value),
    tag = paste0(tag, "=NULL")
  ) |>
  st_as_sf()

shares <- roads_long |>
  mutate(len = as.numeric(st_length(geometry))) |>
  st_drop_geometry() |>
  group_by(tag) |>
  summarise(share = sum(len[is_null]) / sum(len)) |>
  mutate(label = paste0(tag, "\n", scales::percent(share, accuracy = 1), " of road length"))

roads_long <- roads_long |>
  left_join(shares |> select(tag, label), by = "tag") |>
  arrange(is_null)

ggplot() +
  geom_sf(data = roads_long, aes(color = is_null, linewidth = is_null)) +
  scale_color_manual(values = c("TRUE" = "#ff006e", "FALSE" = "grey80")) +
  scale_linewidth_manual(values = c("TRUE" = 0.5, "FALSE" = 0.2)) +
  facet_wrap(vars(label)) +
  coord_sf(xlim = st_bbox(city)[c(1, 3)], ylim = st_bbox(city)[c(2, 4)], expand = FALSE) +
  labs(
    title = "NULL roads",
    subtitle = "Uppsala streets in OpenStreetMap with no speed limit or no surface tagged (pink)",
    caption = caption_src("OpenStreetMap")
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(family = "mono", size = 12, margin = margin(0, 0, 5, 0)),
    plot.title = element_text(family = "mono", size = 22, face = "bold"),
    plot.subtitle = element_text(size = 10, margin = margin(5, 0, 10, 0)),
    plot.caption = element_text(size = 8),
    plot.margin = margin(10, 10, 10, 10)
  )

record_polaroid()
