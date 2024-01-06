# -------------------------------------------------------------
# Applied Statistics / Statistical methods for SCIENCE
# World running records using non-linear regression
# Anton Rask Lundborg
# December 15, 2023
# -------------------------------------------------------------

# Read data from txt-file
wr <- read.delim("WR2011.txt")
wr$sex <- factor(wr$sex)
head(wr)

# Create plot
library(ggplot2)
theme_set(theme_minimal())

ggplot(wr, aes(x = distance, y = time, col = sex)) +
  geom_point(size = 2) +
  geom_smooth() +
  scale_x_log10() +
  scale_y_log10()

# Find guesses for the parameters using a linear model
summary(lm(log(time) ~ 0 + sex + log(distance), data = wr))

# Remarks:
# 1) Note syntax for the "interaction" between alpha and sex!
# 2) For "alpha[sex]" to work it is necessary that sex is
#    encoded as a factor. This is done in line 10 above!

# Estimate the change-point by non-linear regression (recall that we removed
# some observations in the dataset when we analyzed it in Exercise 4.2):
nls(
  log(time) ~ alpha[sex] + beta * log(distance) +
    gamma * log(distance / delta) * (distance > delta),
  start = list(alpha = c(-2.84, -2.72), beta = 1.1, gamma = -0.1, delta = 1200),
  data = wr[-c(1, 11, 12, 14, 15, 17, 27, 28, 30, 31), ]
)
nls(
  log(time) ~ alpha[sex] + beta * log(distance) +
    gamma * log(distance / delta) * (distance > delta),
  start = list(alpha = c(-2.84, -2.72), beta = 1.1, gamma = -0.1, delta = 1500),
  data = wr[-c(1, 11, 12, 14, 15, 17, 27, 28, 30, 31), ]
)
nls(
  log(time) ~ alpha[sex] + beta * log(distance) +
    gamma * log(distance / delta) * (distance > delta),
  start = list(alpha = c(-2.84, -2.72), beta = 1.1, gamma = -0.1, delta = 1800),
  data = wr[-c(1, 11, 12, 14, 15, 17, 27, 28, 30, 31), ]
)

# Please note that the estimates found by nls() depend on the starting point!
# Surprisingly the starting points with delta = 1200 and delta = 1800 both find
# the same solution (delta = 1212.94), whereas the starting point with
# delta = 1500 gives another solution.

# The overall preferable solution is the one with the lowest residual
#  sum-of-squares, i.e. the one with delta = 1212.94. Associated confidence
# intervals can be found like this:
m0 <- nls(
  log(time) ~ alpha[sex] + beta * log(distance) +
    gamma * log(distance / delta) * (distance > delta),
  start = list(alpha = c(-2.84, -2.72), beta = 1.1, gamma = -0.1, delta = 1200),
  data = wr[-c(1, 11, 12, 14, 15, 17, 27, 28, 30, 31), ]
)
cbind(estimate = coef(m0), confint(m0))

# Thus, the change point between short and long running distances is estimated
# to be 1213 meters, with 95% confidence interval given by
# (1056 meters ; 1412 meters). This is roughly in agreement with our general
# perception of athletics.
