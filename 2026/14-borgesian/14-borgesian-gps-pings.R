library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")

on <- as.Date("2026-11-04")

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

# One row per vehicle and GPS timestamp
awk_pings <- "/^        latitude:/{lat=$2} /^        longitude:/{lon=$2} /^      timestamp:/{ts=$2} /^        id:/{gsub(/\"/,\"\",$2); vid=$2} /^  }/{if (lat != \"\" && !((vid SUBSEP ts) in seen)) {seen[vid SUBSEP ts]=1; print vid \",\" ts \",\" lat \",\" lon}; lat=\"\"; lon=\"\"; ts=\"\"; vid=\"\"}"

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
  read_csv(pipe(cmd), col_names = c("vehicle", "ts", "lat", "lon"), col_types = "cddd")
}

# Download, unpack and decode a day in a temp folder; only the result is kept
decode_day <- function(date) {
  tmp <- tempfile("koda-")
  on.exit(unlink(tmp, recursive = TRUE))
  dir.create(tmp)
  download.file("https://raw.githubusercontent.com/google/transit/master/gtfs-realtime/proto/gtfs-realtime.proto", file.path(tmp, "gtfs-realtime.proto"))
  writeLines(c('syntax = "proto2";', 'import "gtfs-realtime.proto";', "message Batch { repeated transit_realtime.FeedMessage feed = 1; }"), file.path(tmp, "batch.proto"))
  archive <- koda_download(date, file.path(tmp, "vp.7z"))
  system2("bsdtar", c("-xf", shQuote(archive), "-C", shQuote(tmp)))
  hour_dirs <- list.dirs(file.path(tmp, "ul/VehiclePositions", format(date, "%Y/%m/%d")), recursive = FALSE)
  day_bounds <- as.numeric(as.POSIXct(format(c(date, date + 1)), tz = "Europe/Stockholm"))
  map(hour_dirs, \(d) decode_hour(d, tmp)) |> list_rbind() |> distinct(vehicle, ts, .keep_all = TRUE) |> filter(ts >= day_bounds[1], ts < day_bounds[2])
}

pings_raw <- cached(paste0("koda-ul-vp-", on), decode_day(on))

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

# Norra Depågatan depot
view <- map_box_view(c(652515, 6638230), 600)

# One cell per 2 × 2 output pixels
cell <- (view[["xmax"]] - view[["xmin"]]) / (map_box$width * 160)
alpha_ping <- 0.25

# Night is 22–06; colour mixes by daytime share
day_ramp <- scales::colour_ramp(c(clr_accent2, clr_accent))

grid <- sf_project("EPSG:4326", "EPSG:3006", cbind(pings_raw$lon, pings_raw$lat)) |> as.data.frame() |> setNames(c("X", "Y")) |> bind_cols(ts = pings_raw$ts) |> filter(between(X, view[["xmin"]], view[["xmax"]]), between(Y, view[["ymin"]], view[["ymax"]])) |> mutate(night = hour(as_datetime(ts, tz = "Europe/Stockholm")) %in% c(22:23, 0:5), gx = floor((X - view[["xmin"]]) / cell) + 0.5, gy = floor((Y - view[["ymin"]]) / cell) + 0.5) |> summarise(n = n(), day_share = mean(!night), .by = c(gx, gy)) |> mutate(opacity = 1 - (1 - alpha_ping)^n, colour = day_ramp(day_share))

p <- ggplot(grid, aes(gx, gy, fill = colour, alpha = opacity)) +
  geom_raster() +
  # Positions in grid cells, beside each yard
  annotate("text", x = 1040, y = 830, label = "City buses", family = f1, size = 4.2, colour = clr_text2) +
  annotate("text", x = 1040, y = 310, label = "Regional buses", family = f1, size = 4.2, colour = clr_text2) +
  scale_fill_identity() +
  scale_alpha_identity() +
  coord_fixed(xlim = c(0, (view[["xmax"]] - view[["xmin"]]) / cell), ylim = c(0, (view[["ymax"]] - view[["ymin"]]) / cell), expand = FALSE) +
  theme_map(bg = clr_land)

key <- \(word, clr) sprintf('#text(fill: rgb("%s"), weight: "bold")[%s]', clr, word)

save_map(p, here::here("2026/14-borgesian/14-borgesian-gps-pings.png"),
  title = "Parked buses draw \\ their depot in Fyrislund",
  subtitle = paste0("Uppsala's buses report their position every second, \\ by ", key("day", clr_accent), " and at ", key("night", clr_accent2), ", even when standing still."),
  caption = caption_src(paste0("Trafiklab KoDa, ", str_squish(format(on, "%e %B %Y"))))
)
