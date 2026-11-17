library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")

# Night of Tuesday 3 to Wednesday 4 November, 23:00 to 05:00
night <- as.numeric(as.POSIXct(c("2026-11-03 23:00", "2026-11-04 05:00"), tz = "Europe/Stockholm"))
hour_lvls <- sprintf("%02d:00", c(23, 0:4))

# Cache as rds in data/
cached <- function(name, expr) {
  path <- file.path(dir_data, paste0(name, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  saveRDS(expr, path)
  expr
}

# TRAFIKLAB_KODA_KEY in ~/.Renviron
koda_download <- function(date, file) {
  key <- Sys.getenv("TRAFIKLAB_KODA_KEY")
  stopifnot("Set TRAFIKLAB_KODA_KEY" = key != "")
  req <- httr2::request("https://api.koda.trafiklab.se/KoDa/api/v2/gtfs-rt/ul/VehiclePositions") |> httr2::req_url_query(date = format(date), key = key) |> httr2::req_timeout(900) |> httr2::req_retry(max_tries = 60, is_transient = \(resp) httr2::resp_status(resp) %in% c(202, 503), backoff = \(i) 60)
  httr2::req_perform(req, path = file)
  file
}

varint <- function(n) if (n < 128) as.raw(n) else c(as.raw(n %% 128 + 128), varint(n %/% 128))

# One row per vehicle and GPS timestamp; trip is empty when the bus is out of service
awk_pings <- "/^        trip_id:/{gsub(/\"/,\"\",$2); tid=$2} /^        latitude:/{lat=$2} /^        longitude:/{lon=$2} /^      timestamp:/{ts=$2} /^        id:/{gsub(/\"/,\"\",$2); vid=$2} /^  }/{if (lat != \"\" && !((vid SUBSEP ts) in seen)) {seen[vid SUBSEP ts]=1; print vid \",\" tid \",\" ts \",\" lat \",\" lon}; lat=\"\"; lon=\"\"; ts=\"\"; vid=\"\"; tid=\"\"}"

# One protoc call per hour of snapshots
decode_hour <- function(hour_dir, proto_dir) {
  batch <- tempfile(fileext = ".bin")
  on.exit(unlink(batch))
  con <- file(batch, "wb")
  walk(list.files(hour_dir, full.names = TRUE), \(f) {
    b <- readBin(f, "raw", file.size(f))
    writeBin(c(as.raw(10), varint(length(b)), b), con)
  })
  close(con)
  cmd <- sprintf("protoc --proto_path=%s --decode=Batch batch.proto < %s | awk '%s'", shQuote(proto_dir), shQuote(batch), awk_pings)
  read_csv(pipe(cmd), col_names = c("vehicle", "trip", "ts", "lat", "lon"), col_types = "ccddd")
}

# Download and unpack a day in a temp folder, decode only the listed hour folders
decode_hours <- function(date, hours) {
  tmp <- tempfile("koda-")
  on.exit(unlink(tmp, recursive = TRUE))
  dir.create(tmp)
  download.file("https://raw.githubusercontent.com/google/transit/master/gtfs-realtime/proto/gtfs-realtime.proto", file.path(tmp, "gtfs-realtime.proto"))
  writeLines(c('syntax = "proto2";', 'import "gtfs-realtime.proto";', "message Batch { repeated transit_realtime.FeedMessage feed = 1; }"), file.path(tmp, "batch.proto"))
  archive <- koda_download(date, file.path(tmp, "vp.7z"))
  system2("bsdtar", c("-xf", shQuote(archive), "-C", shQuote(tmp)))
  hour_dirs <- list.dirs(file.path(tmp, "ul/VehiclePositions", format(date, "%Y/%m/%d")), recursive = FALSE)
  map(hour_dirs[basename(hour_dirs) %in% sprintf("%02d", hours)], \(d) decode_hour(d, tmp)) |> list_rbind()
}

# Hours 21 to 23 reach 23:00 local whether KoDa names folders in UTC or local time
pings_late <- cached("koda-ul-vp-2026-11-03-late", decode_hours(as.Date("2026-11-03"), 21:23))
pings_early <- cached("koda-ul-vp-2026-11-04-early", decode_hours(as.Date("2026-11-04"), 0:5))

# Buses in service only; out-of-service buses carry no trip
pings_raw <- bind_rows(pings_late, pings_early) |> distinct(vehicle, ts, .keep_all = TRUE) |> filter(ts >= night[1], ts < night[2], !is.na(trip))

# Two columns by three rows; each panel is half the box wide and a third of it high
panel_w <- 20000
panel_h <- panel_w * (map_box$height / 3) / (map_box$width / 2)
view <- c(xmin = 649500 - panel_w / 2, xmax = 649500 + panel_w / 2, ymin = 6636000 - panel_h / 2, ymax = 6636000 + panel_h / 2)

xy <- sf_project("EPSG:4326", "EPSG:3006", cbind(pings_raw$lon, pings_raw$lat))

pings <- pings_raw |> mutate(X = xy[, 1], Y = xy[, 2], hour = factor(format(as_datetime(ts, tz = "Europe/Stockholm"), "%H:00"), levels = hour_lvls)) |> filter(between(X, view[["xmin"]], view[["xmax"]]), between(Y, view[["ymin"]], view[["ymax"]]))

# Distinct buses per grid cell and hour
count_cells <- function(cell) {
  pings |> mutate(gx = (floor(X / cell) + 0.5) * cell, gy = (floor(Y / cell) + 0.5) * cell) |> summarise(n = n_distinct(vehicle), .by = c(hour, gx, gy))
}

cells <- count_cells(40)
halo <- count_cells(150)

# Full brightness from the 99th percentile, so busy streets stand out from the station
n_max <- quantile(cells$n, 0.99)
light_ramp <- scales::colour_ramp(c(colorspace::darken(clr_accent, 0.5), clr_accent, "#FFF6DC"))

lights <- cells |> mutate(colour = light_ramp(pmin(log(n) / log(n_max), 1)))
glow <- halo |> mutate(opacity = 0.12 + 0.3 * log(n) / log(max(n)))

# Night sky from the palette's indigo, the gaps between panels a step lighter
clr_night <- colorspace::darken(clr_accent2, 0.7) |> colorspace::desaturate(0.4)
clr_night_gap <- colorspace::darken(clr_accent2, 0.45) |> colorspace::desaturate(0.4)

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

p <- ggplot() +
  geom_raster(data = glow, aes(gx, gy, alpha = opacity), fill = clr_accent, interpolate = TRUE) +
  geom_raster(data = lights, aes(gx, gy, fill = colour)) +
  geom_text(data = tibble(hour = factor(hour_lvls, levels = hour_lvls)), aes(x = view[["xmin"]] + 700, y = view[["ymax"]] - 700, label = hour), hjust = 0, vjust = 1, family = f1, size = 4.2, colour = clr_bg) +
  scale_fill_identity() +
  scale_alpha_identity() +
  facet_wrap(vars(hour), ncol = 2, drop = FALSE) +
  coord_fixed(xlim = view[c("xmin", "xmax")], ylim = view[c("ymin", "ymax")], expand = FALSE) +
  theme_map(bg = clr_night_gap) +
  theme(strip.text = element_blank(), panel.spacing = unit(2, "pt"), panel.background = element_rect(fill = clr_night, colour = NA))

save_map(p, here::here("2026/17-light-dark/17-light-dark-gps-night.png"),
  title = "Uppsala's buses \\ go dark at 2 a.m.",
  subtitle = "Buses in service, night of 3–4 November 2026. \\ Brighter where more buses passed.",
  caption = caption_src("Trafiklab KoDa")
)
