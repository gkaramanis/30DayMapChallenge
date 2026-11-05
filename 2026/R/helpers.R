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

caption_src <- function(src) {
  paste0("Source: ", src, " · Graphic: Georgios Karamanis")
}

# Map box on the portrait canvas, in inches. Keep in sync with map-box in
# frame/map-frame.typ, which sets the page and the text around the map.
map_box <- list(width = 7.4, height = 7.88)

# View bbox in the shape of the map box, so layers cropped to it reach every
# edge. centre is c(x, y) in crs, width in crs units (metres for crs_se).
map_box_view <- function(centre, width, crs = crs_se) {
  half <- c(width, width * map_box$height / map_box$width) / 2
  st_bbox(c(xmin = centre[[1]] - half[1], ymin = centre[[2]] - half[2], xmax = centre[[1]] + half[1], ymax = centre[[2]] + half[2]), crs = crs)
}

# coord_sf with limits in the shape of the map box, so the panel fills it.
# bb is an sf bbox in any CRS; its shorter side grows around the centre.
coord_map_box <- function(bb, crs = crs_se) {
  bb <- st_bbox(st_transform(st_as_sfc(bb), crs))
  w <- bb[["xmax"]] - bb[["xmin"]]
  h <- bb[["ymax"]] - bb[["ymin"]]
  ratio <- map_box$width / map_box$height
  if (w / h < ratio) w <- h * ratio else h <- w / ratio
  cx <- (bb[["xmin"]] + bb[["xmax"]]) / 2
  cy <- (bb[["ymin"]] + bb[["ymax"]]) / 2
  coord_sf(
    xlim = cx + c(-w, w) / 2, ylim = cy + c(-h, h) / 2,
    crs = crs, expand = FALSE
  )
}

theme_map <- function(base_size = 24, bg = clr_bg, text = clr_text) {
  theme_void(base_family = f1, base_size = base_size) +
    theme(
      plot.background = element_rect(fill = bg, color = NA),
      text = element_text(color = text),
      legend.position = "inside",
      legend.position.inside = c(0.02, 0.98),
      legend.justification.inside = c(0, 1),
      legend.title = element_text(size = 14, hjust = 0.5),
      legend.title.position = "top",
      legend.text = element_text(size = 13),
      plot.margin = margin(0, 0, 0, 0)
    )
}

frame_template <- file.path(dir_2026, "frame", "map-frame.typ")

# Draw p at the map box size, then compile frame/map-frame.typ with the text
# to file. Text is Typst markup. Drawing the grob on our own device keeps
# camcorder from blanking it. A timestamped copy goes to record_dir, the
# camcorder folder, unless it is NULL.
save_map <- function(p, file, title = "", subtitle = "", caption = "",
                     record_dir = here::here("30daymap-temp")) {
  map <- tempfile("map-", fileext = ".png")
  on.exit(unlink(map))
  ragg::agg_png(
    map,
    width = map_box$width, height = map_box$height, units = "in",
    res = 320, background = clr_bg
  )
  dev <- grDevices::dev.cur()
  grid::grid.draw(ggplotGrob(p))
  grDevices::dev.off(dev)

  inputs <- c(map = map, title = title, subtitle = subtitle, caption = caption)
  # Root at / so Typst can read the map from the temp folder
  status <- system2("typst", c(
    "compile", "--root", "/", "--format", "png", "--ppi", "320",
    rbind("--input", shQuote(paste0(names(inputs), "=", inputs))),
    shQuote(frame_template), shQuote(file)
  ))
  if (status != 0) cli::cli_abort("Typst failed to render {.file {file}}.")

  if (!is.null(record_dir)) {
    dir.create(record_dir, showWarnings = FALSE)
    stamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%OS6")
    file.copy(file, file.path(record_dir, paste0(stamp, ".png")))
  }
  invisible(file)
}
