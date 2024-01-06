library(datasets)
library(tidyverse)
library(cowplot)

anscombe <- datasets::anscombe

all_y <- c(anscombe$y1, anscombe$y2, anscombe$y3, anscombe$y4)
all_x <- c(anscombe$x1, anscombe$x2, anscombe$x3, anscombe$x4)

x_max <- max(all_x) + 1
x_min <- min(all_x) - 1
y_max <- max(all_y) + 1
y_min <- min(all_y) - 1

p1 <- ggplot(anscombe, aes(x=x1, y=y1)) + geom_point() +
  geom_smooth(method="lm", fullrange=TRUE, se = FALSE) +
  scale_x_continuous(limits=c(x_min, x_max)) +
  scale_y_continuous(limits=c(y_min, y_max))+ theme_minimal(base_size=14)
p2 <- ggplot(anscombe, aes(x=x2, y=y2)) + geom_point() +
  geom_smooth(method="lm", fullrange=TRUE, se = FALSE) +
  scale_x_continuous(limits=c(x_min, x_max)) +
  scale_y_continuous(limits=c(y_min, y_max))+ theme_minimal(base_size=14)
p3 <- ggplot(anscombe, aes(x=x3, y=y3)) + geom_point() +
  geom_smooth(method="lm", fullrange=TRUE, se = FALSE) +
  scale_x_continuous(limits=c(x_min, x_max)) +
  scale_y_continuous(limits=c(y_min, y_max))+ theme_minimal(base_size=14)
p4 <- ggplot(anscombe, aes(x=x4, y=y4)) + geom_point() +
  geom_smooth(method="lm", fullrange=TRUE, se = FALSE) +
  scale_x_continuous(limits=c(x_min, x_max)) +
  scale_y_continuous(limits=c(y_min, y_max))+ theme_minimal(base_size=14)

p <- plot_grid(p1, p2, p3, p4, ncol=2, nrow=2) + theme_minimal(base_size=14)
ggsave("anscombe.pdf", p)
