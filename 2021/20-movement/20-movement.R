library(raster)
library(dplyr)
library(ggplot2)
library(camcorder)

gg_record(dir = "2021/temp", device = "png", width = 7, height = 10, units = "in", dpi = 320)

greece <- rgeoboundaries::gb_adm0("greece")

passenger <- raster("2021/data/passenger density greece.tif")

pass_aggr <- raster::aggregate(passenger, fact = 2, fun = "mean") %>% 
  as.data.frame(xy = TRUE) %>% 
  rename(density = 3) %>%  
  filter(density > 0)


f1 = "General Sans"

ggplot(pass_aggr) +
  geom_sf(data = greece, fill = NA, color = "#295C9C", size = 0.4) +
  geom_point(aes(x, y, alpha = density), size = 0.01, color = "#FFFCC8") +
  annotate("text", 23.5, 33, hjust = 0.5, vjust = 1, label = "Passenger ship movement", family = f1, fontface = "bold", size = 7, color = "grey97") +
  scale_alpha_continuous(trans = "log10", range = c(0.05, 0.95), labels = scales::number_format(big.mark = " ", accuracy = 1)) +
  guides(alpha = guide_legend(override.aes = list(size = 2))) +
  labs(
    caption = "Total number of AIS positions that have been reported by ships between January 2015 and February 2021\nSource: The World Bank · Graphic: Georgios Karamanis"
  ) +
  coord_sf(xlim = c(19, 28), ylim = c(33, 42)) +
  theme_void(base_size = 12, base_family = f1) +
  theme(
    legend.position = c(0.11, 0.09),
    legend.title = element_blank(),
    legend.text = element_text(color = "grey97", size = 11),
    plot.background = element_rect(fill = "#032C3F", color = NA),
    plot.caption = element_text(color = "grey97", hjust = 0.5, lineheight = 1)
  )
  
