# Shared helpers for the 2026 Uppsala transport maps.
# Paths assume this folder sits at 2026/ in the 30daymapchallenge project.

library(tidyverse)
library(sf)

dir_2026 <- here::here("2026")

crs_se <- 3006 # SWEREF 99 TM

# Style shared with the 2026 #30DayChartChallenge (30daychartchallenge/2026)

f1 <- "Metropolis"
f2 <- "Karst"

clr_bg <- "#FAFAF6"
clr_text <- "#1A1A18"
clr_text2 <- "#6B6B65"
clr_accent <- "#E8A020"
clr_accent2 <- "#3D4F9F"
clr_grid <- "#E0E0D8"

# Base-map layers, tints of the palette above
clr_land <- "#F0EFE8"
clr_water <- "#D9DDEC"
clr_road <- clr_grid

# Portrait canvas of the chart challenge: 8 x 11.08 in at 320 dpi (2560 x 3545 px)
map_width <- 8
map_height <- 11.08

caption_src <- function(src) {
  paste0("Source: ", src, " · Graphic: Georgios Karamanis")
}

theme_map <- function(base_size = 24, bg = clr_bg, text = clr_text) {
  theme_void(base_family = f1, base_size = base_size) +
    theme(
      plot.background = element_rect(fill = bg, color = NA),
      text = element_text(color = text),
      legend.position = "top",
      legend.title = element_text(size = 14, hjust = 0.5),
      legend.title.position = "top",
      legend.text = element_text(size = 13),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_text(family = f2, face = "bold", size = 36, hjust = 0.5, margin = margin(t = 20, b = 6)),
      plot.subtitle = element_text(size = 22, hjust = 0.5, lineheight = 1.1, margin = margin(t = 6, b = 30)),
      plot.caption = element_text(size = 12, hjust = 0.5, margin = margin(t = 30, b = 10)),
      plot.margin = margin(t = 20, l = 20, r = 20, b = 18)
    )
}

# Typst frame -------------------------------------------------------------

# Map box on the portrait canvas, in inches from the top left. The band above
# holds the title and subtitle, the band below the caption.
frame_box <- list(x = 0.3, y = 2.6, width = 7.4, height = 7.88)
frame_template <- file.path(dir_2026, "frame", "map-frame.typ")

# Save p at the map box size with no plot margin; Typst sets the title,
# subtitle and caption around it
frame_map <- function(p, file) {
  lab <- get_labs(p)
  dir <- tempfile("frame-")
  dir.create(dir)
  panel <- file.path(dir, "panel.png")
  data <- file.path(dir, "data.json")

  body <- p +
    labs(title = NULL, subtitle = NULL, caption = NULL) +
    theme(plot.margin = margin(0, 0, 0, 0))

  ggsave(
    panel, body,
    width = frame_box$width, height = frame_box$height, dpi = 320,
    device = ragg::agg_png
  )
  jsonlite::write_json(
    list(
      title = lab$title %||% "",
      subtitle = lab$subtitle %||% "",
      caption = lab$caption %||% "",
      panel = panel,
      box = frame_box,
      page = list(width = map_width, height = map_height),
      bg = clr_bg,
      text = clr_text
    ),
    data,
    auto_unbox = TRUE
  )

  # Root at / so Typst can read the panel from the temp folder
  status <- system2("typst", c(
    "compile", "--root", "/", "--format", "png", "--ppi", "320",
    "--input", paste0("data=", data), shQuote(frame_template), shQuote(file)
  ))
  if (status != 0) cli::cli_abort("Typst failed to render {.file {file}}.")
  invisible(file)
}
