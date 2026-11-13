library(tidyverse)
library(sf)
library(camcorder)
library(ggarrow)

source(here::here("2026/R/helpers.R"))

dir_data <- file.path(dir_2026, "data")
dir.create(dir_data, showWarnings = FALSE)

# Cache expr as rds in data/
cached <- function(name, expr) {
  path <- file.path(dir_data, paste0(name, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  saveRDS(expr, path)
  expr
}

gg_record(here::here("30daymap-temp"), width = map_box$width, height = map_box$height, units = "in", dpi = 320)

year <- "2024"
min_commuters <- 400
county_codes <- c("0305", "0319", "0330", "0331", "0360", "0380", "0381", "0382")

# SCB: employed people living or working in the county, by home and work municipality
pend_url <- "https://api.scb.se/OV0104/v1/doris/en/ssd/AM/AM0210/AM0210F/ArRegPend2"
commuting <- cached(paste0("scb-pendling-", year), bind_rows(
  pxweb::pxweb_get_data(pend_url, pxweb::pxweb_query(list(Kon = "1+2", Bostadskommun = county_codes, Arbetsstallekommun = "*", Tid = year, ContentsCode = "000000QT")), column.name.type = "code", variable.value.type = "code"),
  pxweb::pxweb_get_data(pend_url, pxweb::pxweb_query(list(Kon = "1+2", Bostadskommun = "*", Arbetsstallekommun = county_codes, Tid = year, ContentsCode = "000000QT")), column.name.type = "code", variable.value.type = "code")
)) |>
  distinct() |>
  transmute(home = Bostadskommun, work = Arbetsstallekommun, commuters = `000000QT`) |>
  filter(home != work)

# SCB boundaries: nested zips, SWEREF 99 TM with no CRS set
scb_zip <- file.path(dir_data, "scb-digitala-granser-260225.zip")
if (!file.exists(scb_zip)) download.file("https://scb.se/contentassets/3443fea3fa6640f7a57ea15d9a372d33/shape_svenska_260225.zip", scb_zip, mode = "wb")
municipalities <- read_sf(sprintf("/vsizip/{/vsizip/%s/Kommun_Sweref99TM.zip}", scb_zip)) |> st_set_crs(crs_se)
counties <- read_sf(sprintf("/vsizip/{/vsizip/%s/LanSweref99TM.zip}", scb_zip)) |> st_set_crs(crs_se)

uppsala_county <- filter(counties, LnKod == "03")

# Neighbouring counties
neighbours <- counties |>
  st_filter(uppsala_county, .predicate = st_is_within_distance, dist = 1000) |>
  filter(LnKod != "03") |>
  st_drop_geometry() |>
  transmute(LnKod, county = paste(str_remove(LnNamn, "s$"), "County"))

# County municipalities stay; neighbours group by county
places <- municipalities |>
  mutate(geometry = st_point_on_surface(geometry), LnKod = str_sub(KnKod, 1, 2)) |>
  left_join(neighbours, by = join_by(LnKod)) |>
  mutate(node = if_else(KnKod %in% county_codes, KnNamn, county)) |>
  filter(!is.na(node))

node_of <- st_drop_geometry(places) |> select(KnKod, node)

flows <- commuting |>
  inner_join(node_of, by = join_by(home == KnKod)) |>
  inner_join(node_of, by = join_by(work == KnKod), suffix = c("_from", "_to")) |>
  summarise(commuters = sum(commuters), .by = c(node_from, node_to)) |>
  filter(node_from != node_to)

# Neighbouring counties sit at their commuter-weighted centre
weights <- bind_rows(commuting |> select(KnKod = home, commuters), commuting |> select(KnKod = work, commuters)) |>
  summarise(commuters = sum(commuters), .by = KnKod)

nodes <- places |>
  inner_join(weights, by = join_by(KnKod)) |>
  mutate(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]) |>
  st_drop_geometry() |>
  summarise(x = weighted.mean(x, commuters), y = weighted.mean(y, commuters), .by = node)

arrows <- flows |>
  filter(commuters >= min_commuters) |>
  left_join(nodes, by = join_by(node_from == node)) |>
  left_join(nodes, by = join_by(node_to == node), suffix = c("", "end")) |>
  mutate(kind = if_else(node_from %in% neighbours$county | node_to %in% neighbours$county, "Across the county border", "Within Uppsala County")) |>
  arrange(commuters)

nodes_drawn <- filter(nodes, node %in% c(arrows$node_from, arrows$node_to))

to_stockholm <- flows |> filter(node_from == "Uppsala", node_to == "Stockholm County") |> pull(commuters)

p <- ggplot() +
  geom_sf(data = municipalities, aes(fill = KnKod %in% county_codes), color = clr_bg, linewidth = 0.3, show.legend = FALSE) +
  geom_sf(data = uppsala_county, fill = NA, color = clr_text2, linewidth = 0.5) +
  geom_arrow_curve(data = arrows, aes(x, y, xend = xend, yend = yend, linewidth = commuters, color = kind), curvature = 0.3, arrow_head = arrow_head_wings(offset = 25, inset = 40), length_head = 3, resect = 2, alpha = 0.85) +
  geom_point(data = nodes_drawn, aes(x, y), size = 1.8, shape = 21, fill = clr_bg, color = clr_text) +
  # Stockholm County label below its dot
  ggrepel::geom_text_repel(data = nodes_drawn, aes(x, y, label = node), family = f1, size = 3.4, color = clr_text, bg.color = clr_bg, bg.r = 0.12, min.segment.length = Inf, nudge_y = if_else(nodes_drawn$node == "Stockholm County", -4000, 0), seed = 4) +
  scale_fill_manual(values = c("TRUE" = "#E4E2D6", "FALSE" = "#F5F4EE")) +
  scale_color_manual(values = c("Within Uppsala County" = clr_accent2, "Across the county border" = clr_accent)) +
  scale_linewidth(range = c(0.3, 7), guide = "none") +
  guides(color = guide_legend(title = NULL, override.aes = list(linewidth = 2))) +
  coord_map_box(st_buffer(st_as_sf(nodes_drawn, coords = c("x", "y"), crs = crs_se), 15000)) +
  theme_map() +
  theme(
    legend.position.inside = c(0.02, 0.12),
    legend.text = element_text(size = 11), legend.key.size = unit(1, "lines")
  )

save_map(p, here::here("2026/13-interactions/13-interactions-commuting.png"),
  title = "Uppsala's commuters \\ head for Stockholm",
  subtitle = paste0(scales::number(to_stockholm, big.mark = " "), " Uppsala residents work in Stockholm County, \\ almost double the county's commuters to Uppsala"),
  caption = caption_src("SCB 2024")
)
