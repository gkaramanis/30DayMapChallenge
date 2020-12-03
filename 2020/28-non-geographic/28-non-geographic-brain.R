library(tidyverse)
library(ggseg)

ggplot() +
  geom_brain(atlas = dk) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 7)) +
  guides(fill = guide_legend(ncol = 4))

ggplot() +
  geom_brain(atlas = aseg) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 7)) +
  guides(fill = guide_legend(ncol = 4))

