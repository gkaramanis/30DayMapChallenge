# Written end to end by Claude Code from prompts only.

library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = map_width, height = map_height, units = "in", dpi = 320)

gtfs <- ul_gtfs()
on <- typical_weekday(gtfs)

# City buses are lines under 100; the UL feed has no train routes
route_kind <- gtfs$routes |>
  mutate(
    num = suppressWarnings(as.numeric(route_short_name)),
    kind = case_when(
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
  mutate(kind = factor(kind, levels = c("Regional bus", "City bus"))) |>
  arrange(kind)

lan <- uppsala_lan() |> st_transform(crs_se)

ggplot() +
  geom_sf(data = lan, fill = clr_land, color = clr_grid, linewidth = 0.3) +
  geom_sf(data = shapes, aes(color = kind, linewidth = trips), alpha = 0.8) +
  scale_color_manual(values = c("City bus" = clr_accent, "Regional bus" = clr_accent2)) +
  scale_linewidth(range = c(0.15, 1.5), guide = "none") +
  coord_sf(expand = FALSE) +
  labs(
    title = "UL, all of it",
    subtitle = paste0("Every bus route running on\n", str_squish(format(on, "%e %B %Y")), ", width by trips"),
    caption = caption_src("UL GTFS via Trafiklab") |> paste("· Code by Claude Code from prompts only"),
    color = NULL
  ) +
  guides(color = guide_legend(override.aes = list(linewidth = 1.5))) +
  theme_map()

record_polaroid()
