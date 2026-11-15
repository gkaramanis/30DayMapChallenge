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

# One row per vehicle and GPS timestamp; trip is empty when the bus is not on a trip
awk_pings <- "/^        latitude:/{lat=$2} /^        longitude:/{lon=$2} /^        speed:/{spd=$2} /^        trip_id:/{gsub(/\"/,\"\",$2); trip=$2} /^      timestamp:/{ts=$2} /^        id:/{gsub(/\"/,\"\",$2); vid=$2} /^  }/{if (lat != \"\" && !((vid SUBSEP ts) in seen)) {seen[vid SUBSEP ts]=1; print vid \",\" ts \",\" lat \",\" lon \",\" spd \",\" trip}; lat=\"\"; lon=\"\"; ts=\"\"; vid=\"\"; spd=\"\"; trip=\"\"}"

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
  read_csv(pipe(cmd), col_names = c("vehicle", "ts", "lat", "lon", "speed", "trip"), col_types = "cddddc")
}

# Unpack and decode a day in a temp folder; downloads the archive unless one is given
decode_day <- function(date, archive = NULL) {
  tmp <- tempfile("koda-")
  on.exit(unlink(tmp, recursive = TRUE))
  dir.create(tmp)
  download.file("https://raw.githubusercontent.com/google/transit/master/gtfs-realtime/proto/gtfs-realtime.proto", file.path(tmp, "gtfs-realtime.proto"))
  writeLines(c('syntax = "proto2";', 'import "gtfs-realtime.proto";', "message Batch { repeated transit_realtime.FeedMessage feed = 1; }"), file.path(tmp, "batch.proto"))
  archive_file <- archive %||% koda_download(date, file.path(tmp, "vp.7z"))
  system2("bsdtar", c("-xf", shQuote(archive_file), "-C", shQuote(tmp)))
  hour_dirs <- list.dirs(file.path(tmp, "ul/VehiclePositions", format(date, "%Y/%m/%d")), recursive = FALSE)
  day_bounds <- as.numeric(as.POSIXct(format(c(date, date + 1)), tz = "Europe/Stockholm"))
  cl <- parallel::makeCluster(8)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterEvalQ(cl, library(tidyverse))
  parallel::clusterExport(cl, c("varint", "awk_pings"))
  parallel::parLapply(cl, hour_dirs, decode_hour, proto_dir = tmp) |> list_rbind() |> distinct(vehicle, ts, .keep_all = TRUE) |> filter(ts >= day_bounds[1], ts < day_bounds[2])
}

pings_raw <- cached(paste0("koda-ul-vp-trips-", on), decode_day(on))
