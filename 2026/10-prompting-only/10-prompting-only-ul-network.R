# Written end to end by Claude Code from prompts only.

library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = 8, height = 9, dpi = 320)

gtfs <- ul_gtfs()
on <- typical_weekday(gtfs)

# UL colours: green city buses (lines under 100), yellow regional buses, trains
route_kind <- gtfs$routes |>
  mutate(
    num = suppressWarnings(as.numeric(route_short_name)),
    kind = case_when(
      is_rail(route_type) ~ "Train",
      is_bus(route_type) & num < 100 ~ "City bus",
      is_bus(route_type) ~ "Regional bus",
      TRUE ~ "Other"
    )
  ) |>
  select(route_id, kind)

shape_kind <- gtfs$trips |>
  filter(service_id %in% services_on(gtfs, on), !is.na(shape_id)) |>
  left_join(route_kind, by = "route_id") |>
  count(shape_id, kind, name = "trips")

shapes <- tidytransit::shapes_as_sf(gtfs$shapes) |>
  inner_join(shape_kind, by = "shape_id") |>
  filter(kind != "Other") |>
  st_transform(crs_se) |>
  mutate(kind = factor(kind, levels = c("Regional bus", "Train", "City bus"))) |>
  arrange(kind)

lan <- uppsala_lan() |> st_transform(crs_se)

ggplot() +
  geom_sf(data = lan, fill = "#1d1f24", color = NA) +
  geom_sf(data = shapes, aes(color = kind, linewidth = trips), alpha = 0.8) +
  scale_color_manual(values = c("City bus" = "#00a651", "Regional bus" = "#ffd100", "Train" = "#e9e9e9")) +
  scale_linewidth(range = c(0.15, 1.5), guide = "none") +
  coord_sf(expand = FALSE) +
  labs(
    title = "UL, all of it",
    subtitle = paste0("Every bus and train shape running on ", format(on, "%e %B %Y"), ", width by trips"),
    caption = caption_src("UL GTFS via Trafiklab") |> paste("· Code by Claude Code from prompts only"),
    color = NULL
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "top",
    legend.text = element_text(color = "white"),
    plot.background = element_rect(fill = "#121316", color = NA),
    plot.title = element_text(family = f2, size = 20, face = "bold", color = "white"),
    plot.subtitle = element_text(size = 10, color = "grey80", margin = margin(5, 0, 10, 0)),
    plot.caption = element_text(size = 8, color = "grey70"),
    plot.margin = margin(10, 10, 10, 10)
  )

record_polaroid()
