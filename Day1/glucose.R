# -------------------------------------------------------------
# Applied Statistics / Statistical methods for SCIENCE
# Day 1
# Example on glucose change
# Anton Rask Lundborg
# November 2023
# -------------------------------------------------------------
# Set ggplot options
library(ggplot2)
theme_set(theme_minimal(base_size = 14))

# Read data and compute basic summary statistics
change <- c(0.77, 5.14, 3.38, 1.44, 5.34, -0.55, -0.72, 2.89)
mean(change)
sd(change)

# Graphical check of normality
df <- data.frame(change = change)
p <- ggplot(df, aes(sample = change)) +
  stat_qq() +
  stat_qq_line()
p

# Quantitative check of normality
shapiro.test(change)
ks.test(change, "pnorm", mean(change), sd(change))

# One sample T-test
t.test(change)
