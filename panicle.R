library(tidyverse)
library(ptrapr)
library(ggraph)

# get data ----------------------------------------------------------------

data("good_panicle")

good_panicle


# plot panicle ------------------------------------------------------------

point_size <- 2

p <- 
  good_panicle %>% 
  ggraph() + 
  geom_edge_link(colour = "grey40",
                 edge_width = .54,
                 arrow = grid::arrow(length = unit(1.2,
                                                   "mm"),
                                     type = "closed"),
                 end_cap = circle(point_size/2, "mm"),
                 start_cap = circle(point_size/2, "mm")) +
  geom_node_point(aes(colour = type),
                  # size = 3.7,
                  size = point_size,
                  alpha = 1) +
  scale_x_reverse() +
  coord_flip() +
  scale_color_manual(values = scico::scico(
    10, palette = "lajolla")[c(3,8,7,6,5)],
    guide = FALSE) +
  theme_void() +
  theme(aspect.ratio = 1.2,
        panel.background = element_rect(fill = "ivory1"),
        panel.grid.major = element_line(colour = "grey40",
                                        size = .2, linetype = "dotted"),
        panel.grid.minor = element_line(colour = "grey60",
                                        size = .1, linetype = "dotted"))

p
png("cards/panicle.png",
    height = 1200,
    width = 1200,
    res = 300)
p
dev.off()

