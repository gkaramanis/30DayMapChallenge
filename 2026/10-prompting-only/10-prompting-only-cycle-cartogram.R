library(tidyverse)
library(sf)
library(camcorder)

source(here::here("2026/R/helpers.R"))

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

dir_data <- file.path(dir_2026, "data")
dir.create(dir_data, showWarnings = FALSE, recursive = TRUE)

# Evaluate expr once and cache the result as rds in data/
cached <- function(name, expr) {
  path <- file.path(dir_data, paste0(name, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  out <- expr
  saveRDS(out, path)
  out
}

# Cycle network in metres per road owner, per kommun, sheet 2025.
# Downloaded by hand from https://www.nvdb.se/sv/kund/statistik/
nvdb <- readxl::read_xlsx(file.path(dir_data, "antal-meter-cykelnat-per-vaghallare-efter-lan-och-kommun.xlsx"), sheet = "2025") |>
  filter(Län == "Uppsala", Kommun != "Total") |>
  mutate(cycle_m = as.numeric(Enskild) + as.numeric(Kommunal) + as.numeric(Statlig)) |>
  select(kommun = Kommun, cycle_m)

# Population on 31 December 2024 from SCB's PxWeb API
kommun_codes <- c("0305", "0319", "0330", "0331", "0360", "0380", "0381", "0382")

pop_query <- list(query = list(list(code = "Region", selection = list(filter = "item", values = kommun_codes)), list(code = "ContentsCode", selection = list(filter = "item", values = list("BE0101N1"))), list(code = "Tid", selection = list(filter = "item", values = list("2024")))), response = list(format = "json"))

pop <- cached("scb-folkmangd-kommun-lan-2024", {
  httr2::request("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101A/BefolkningNy") |>
    httr2::req_body_json(pop_query) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    pluck("data") |>
    map(\(d) tibble(kommunkod = d$key[[1]], people = as.numeric(d$values[[1]]))) |>
    list_rbind()
})

# DeSO 2025 dissolved into kommuner, for the county and its neighbours
deso_url <- "https://geodata.scb.se/geoserver/stat/wfs?service=WFS&version=2.0.0&request=GetFeature&typeNames=stat:DeSO_2025&outputFormat=application/json&CQL_FILTER="

deso <- cached("scb-deso-2025-lan-area", st_read(paste0(deso_url, URLencode("BBOX(sp_geometry,560000,6530000,760000,6780000,'EPSG:3006')", reserved = TRUE)), quiet = TRUE))

kommuner <- deso |>
  st_transform(crs_se) |>
  st_make_valid() |>
  summarise(geometry = st_union(geometry), .by = c(kommunkod, lanskod))

lan <- filter(kommuner, lanskod == "03")

# SCB names the kommuner by code; NVDB by name
kommun_names <- tibble(kommunkod = kommun_codes, kommun = c("Håbo", "Älvkarleby", "Knivsta", "Heby", "Tierp", "Uppsala", "Enköping", "Östhammar"))

cycle <- lan |>
  inner_join(kommun_names, by = join_by(kommunkod), relationship = "one-to-one", unmatched = "error") |>
  inner_join(nvdb, by = join_by(kommun), relationship = "one-to-one", unmatched = "error") |>
  inner_join(pop, by = join_by(kommunkod), relationship = "one-to-one", unmatched = "error") |>
  mutate(m_per_person = cycle_m / people)

# Mosaic cartogram: one square per 500 residents, exact counts per kommun
cells <- cycle |>
  select(kommun, people) |>
  popgridR::popgrid(values = "people", region_col = "kommun", value_per_cell = 500) |>
  st_transform(crs_se) |>
  left_join(st_drop_geometry(select(cycle, kommun, m_per_person)), by = join_by(kommun), relationship = "many-to-one")

blocks <- summarise(cells, geometry = st_union(geometry), .by = c(kommun, m_per_person))

labels <- blocks |>
  st_point_on_surface() |>
  mutate(label = paste0(kommun, "\n", scales::number(m_per_person, 0.1), " m"), x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]) |>
  st_drop_geometry()

top <- slice_max(cycle, m_per_person, n = 1)
county_m <- sum(cycle$cycle_m) / sum(cycle$people)
below_share <- sum(cycle$people[cycle$m_per_person < county_m]) / sum(cycle$people)

p <- ggplot() +
  geom_sf(data = cells, aes(fill = m_per_person), color = clr_bg, linewidth = 0.15) +
  geom_sf(data = blocks, fill = NA, color = clr_text, linewidth = 0.4) +
  geom_label(data = labels, aes(x, y, label = label), family = f1, size = 4, lineheight = 0.9, color = clr_text, fill = clr_bg, linewidth = 0, label.r = unit(1, "mm")) +
  scale_fill_stepsn(colours = colorRampPalette(c("#DDE0F0", clr_accent2))(5), values = scales::rescale(seq(1.75, 3.75, 0.5), from = c(1.5, 4)), limits = c(1.5, 4), breaks = seq(2, 3.5, 0.5), labels = \(x) if_else(x %% 1 == 0, paste(x, "m"), ""), name = "Cycle network per resident", guide = guide_coloursteps(show.limits = TRUE)) +
  coord_map_box(st_buffer(st_as_sfc(st_bbox(cells)), 10000)) +
  theme_map() +
  theme(legend.position.inside = c(0.04, 0.98), legend.direction = "horizontal", legend.key.width = unit(13, "mm"), legend.key.height = unit(4, "mm"))

save_map(p, here::here("2026/10-prompting-only/10-prompting-only-cycle-cartogram.png"),
  title = str_glue("{top$kommun} leads on\\ cycle paths") |> as.character(),
  subtitle = str_glue("Each square is 500 residents of Uppsala County.\\ {scales::percent(below_share)} live below the county's {scales::number(county_m, 0.1)} m per person") |> as.character(),
  caption = caption_src("NVDB, SCB")
)
