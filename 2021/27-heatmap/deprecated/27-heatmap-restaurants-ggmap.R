library(ggmap)

gr_base <- get_stamenmap(bbox = c(left = 19, right = 30, bottom = 33, top = 42),
                         maptype = "toner-lite",crop = TRUE, zoom = 8)


ggmap(gr_base) +
  stat_density2d(data = gr_rest_df, aes(x, y, fill = ..density..), geom = "tile", contour = FALSE, n = 60, alpha = 0.8) +
  scale_fill_viridis_c(option = "turbo") +
  labs(fill = "Density of\nrestaurants") +
  theme_void() +
  theme(
    legend.position = c(0.1, 0.17),
    legend.title = element_text(family = f1, color = "grey97"),
    legend.text = element_text(family = f1, color = "grey97"),
    plot.background = element_rect(fill = "#2C1439", color = NA)
  )
