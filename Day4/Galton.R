# --------------------------------------------------------------
# Applied Statistics / Statistical methods for SCIENCE
# Day 4: Linear normal models
# Galton example
# Anton Rask Lundborg
# December 8, 2023
# --------------------------------------------------------------

# Load packages
library(tidyverse)

# Set theme
theme_set(theme_minimal())

# Data from HistData-package: select one random son per father
library(HistData)
data("GaltonFamilies")

height <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  slice_sample(n = 1)


# Plot regression lines for both father on child and child on father
ggplot(height) +
  geom_point(aes(x = father, y = childHeight)) +
  geom_smooth(aes(x = father, y = childHeight, color = "child"),
    method = "lm", se = FALSE
  ) +
  geom_smooth(aes(x = childHeight, y = father, color = "father"),
    method = "lm",
    se = FALSE, orientation = "y"
  ) +
  scale_x_continuous(name = "Height of father in inches") +
  scale_y_continuous(name = "Height of child in inches") +
  scale_color_manual(values = c("red", "blue"), name = "Response") +
  coord_equal()

# Fit the linear regression of child on father
m1 <- lm(childHeight ~ father, data = height)

# Model validation
par(mfrow = c(2, 2))
plot(m1)
par(mfrow = c(1, 1))

# Is there an effect?
drop1(m1, test = "F")

# What is the effect?
cbind(coef(m1), confint(m1))


# Plot of observations together with model predictions
x <- seq(min(height$father), max(height$father), length.out = 100)
pred_height <- cbind(
  data.frame(father = x),
  predict(m1,
    interval = "prediction",
    newdata = data.frame(father = x)
  )
)
ggplot(height, aes(x = father, y = childHeight)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_line(aes(x = father, y = lwr),
    data = pred_height,
    col = "red", lty = 2
  ) +
  geom_line(aes(x = father, y = upr),
    data = pred_height,
    col = "red", lty = 2
  ) +
  scale_x_continuous(name = "Height of father in inches") +
  scale_y_continuous(name = "Height of child in inches") +
  coord_equal()


# Illustration of RMSE
height$pred_height <- fitted(m1)
height$residuals <- resid(m1)
RMSE <- summary(m1)$sigma
ggplot(height, aes(x = pred_height, y = residuals)) +
  geom_point() +
  geom_abline(slope = 0, intercept = 0, color = "gray", lty = "dashed") +
  geom_abline(slope = 0, intercept = 1.96 * RMSE, color = "red",
              lty = "dashed") +
  geom_abline(slope = 0, intercept = -1.96 * RMSE, color = "red",
              lty = "dashed") +
  scale_x_continuous(name = "Predicted height") +
  scale_y_continuous(name = "Residual (observed minus predicted)")


# Illustration of R^2
ggplot(height, aes(x = childHeight, y = pred_height)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(name = "Observed height of child") +
  scale_y_continuous(name = "Predicted height of child") +
  coord_equal()


plot(height$childHeight, height$pred_height)
lines(height$childHeight, height$childHeight + 1.96 * summary(m1)$sigma)
lines(height$childHeight, height$childHeight - 1.96 * summary(m1)$sigma)
