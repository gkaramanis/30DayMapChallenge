library(tidyverse)
library(camcorder)

gg_record(dir = "2021/temp", device = "png", width = 11, height = 10, units = "in", dpi = 320)

centroids <- read_tsv("2021/data/country-centroids.csv") %>% 
  mutate(
    a = longitude * pi/180,
    d = ifelse(country == "GR", 2, 1.3),
    h = ifelse(abs(a) < pi/2, 0, 1)
    # name = str_wrap(name, 30)
    )

longs <- data.frame(
  l = seq(-180, 180, 90)
  ) %>% 
  mutate(la = l * pi/180)

f1 = "JetBrains Mono"
f2 = "Publico Headline"

ggplot(centroids) +
  geom_segment(aes(x = cos(a), xend = 0.999 * d * cos(a), y = sin(a), yend = 0.999 * d * sin(a)), size = 0.05) +
  geom_point(aes(x = cos(a), y = sin(a)), size = 0.2) +
  geom_text(aes(x = d * cos(a), y = d * sin(a),
                label = ifelse(h == 0, paste("·", name), paste(name, "·")),
                hjust = h,
                angle = ifelse(h == 0, longitude, longitude - 180),
                color = ifelse(name == "Greece", "purple", "grey10")),
            check_overlap = TRUE, size = 2.7,
            family = f1, lineheight = 0.9) +
  geom_text(data = longs, aes(x = 0.85 * cos(la), y = 0.85 * sin(la),
                              label = ifelse(l == -180, "±180", l)), check_overlap = TRUE, family = f2, size = 3.5) +
  annotate("text", 0, 0, label = "The\nEarth\nis not\nflat", family = f2, size = 15, fontface = "bold", lineheight = 0.9) +
  scale_color_identity() +
  labs(caption = "Longitude of country centroids · Graphic: Georgios Karamanis") +
  coord_fixed(xlim = c(-2, 2), ylim = c(-2, 2), clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey98", color = NA),
    plot.caption = element_text(hjust = 0.5, family = f2),
    plot.margin = margin(20, 20, 20, 20)
  )
  
