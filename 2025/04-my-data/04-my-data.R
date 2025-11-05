library(tidyverse)
library(sf)
library(patchwork)
library(camcorder)


gg_record(here::here("30daymap-temp"), width = 11, height = 8, dpi = 320)

my_works <- read_csv(here::here("2025/data/works-2025-11-03T22-27-32.csv"))
inst_coords <- read_csv(here::here("2025/data/institutes_coords.csv"))

works_coods <- my_works |> 
  select(title, publication_year, authorships.raw_affiliation_strings, authorships.countries) |> 
  separate_longer_delim(authorships.raw_affiliation_strings, "|") |> 
  distinct(title, publication_year, authorships.raw_affiliation_strings) |> 
  left_join(inst_coords)

robin <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

works_sf <- works_coods |> 
  filter(!is.na(longitude), !is.na(latitude)) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |> 
  st_transform(crs = robin)

world_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |> 
  st_transform(crs = robin)

margin <- 5e5
f1 <- "Inclusive Sans"
f2 <- "Publico Headline"

works_sf_sorted <- works_sf |> 
  mutate(title = fct_reorder(title, publication_year))

plot_list <- split(works_sf_sorted, works_sf_sorted$title) |> 
  lapply(function(df) {
    bbox <- st_bbox(df)
    bbox[1] <- bbox[1] - margin
    bbox[3] <- bbox[3] + margin
    bbox[2] <- bbox[2] - margin
    bbox[4] <- bbox[4] + margin
    
    # Make the bounding box square by expanding to match the larger dimension
    width <- bbox[3] - bbox[1]
    height <- bbox[4] - bbox[2]
    
    if (width > height) {
      # Expand height to match width
      center_y <- (bbox[2] + bbox[4]) / 2
      bbox[2] <- center_y - width / 2
      bbox[4] <- center_y + width / 2
    } else {
      # Expand width to match height
      center_x <- (bbox[1] + bbox[3]) / 2
      bbox[1] <- center_x - height / 2
      bbox[3] <- center_x + height / 2
    }
    
    world_crop <- st_crop(world_sf, bbox)
    
    ggplot() +
      geom_sf(data = world_crop, fill = "white", color = "grey70", size = 0.1) +
      geom_sf(data = df, color = "#ff4d00", size = 4, shape = "★", stat = "unique") +
      coord_sf(expand = FALSE) +
      labs(
        caption = str_wrap(paste0(df$title, " (", df$publication_year, ")"), 55)
      ) +
      theme_void(base_family = f1) +
      theme(
        plot.background = element_rect(fill = "#f7f7f7", color = NA),
        panel.background = element_rect(fill = "#EFF3F6"),
        plot.caption = element_text(hjust = 0.5, size = 8),
        plot.margin = margin(10, 10, 10, 10)
      )
  })

wrap_plots(plotlist = plot_list, ncol = 3) +
  plot_annotation(
    caption = "Graphic: Georgios Karamanis",
    theme = theme_void(base_family = f1) +
      theme(
        plot.background = element_rect(fill = "#f7f7f7", color = NA),
        plot.caption = element_text(family = f1),
        plot.margin = margin(5, 5, 5, 5)
      )
  )

