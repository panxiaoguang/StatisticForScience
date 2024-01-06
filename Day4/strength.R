# -------------------------------------------------------------
# Applied Statistics / Statistical methods for SCIENCE
# Day 4: Estimated marginal means in an ANCOVA
# Anton Rask Lundborg
# December 11, 2023
# -------------------------------------------------------------

# load libraries
library(emmeans)
library(LabApplStat)
library(tidyverse)

# Set theme
theme_set(theme_minimal())

# Read data on strength of men and women
strength <- read.delim("strength.txt")
summary(strength)

# t-test
t.test(strength ~ sex, data = strength)

# Fit and validate ANCOVA
m1 <- lm(strength ~ sex * lean.body.mass, data = strength)
par(mfrow = c(2, 2))
plot(m1)
par(mfrow = c(1, 1))

# Model reduction (note that drop1 incorrectly does not test sex!)
drop1(m1, test = "F")
m2 <- lm(strength ~ sex + lean.body.mass, data = strength)

# Original design diagram
plot(DD(~ sex * lean.body.mass, data = strength, center = FALSE))

# New design diagram
plot(DD(~ sex + lean.body.mass, data = strength))


# -------------------------------------------------------------------------
# Balanced LS-means (standard em-means, also used by the emmeans-package)
# -------------------------------------------------------------------------

emmeans(m2, ~sex)

# Here is the contrast of the balanced em-means between men and women
pairs(emmeans(m2, ~sex))

# We can plot this:
avg_mass <- mean(strength$lean.body.mass)
man_pred_avg_lean_body_mass <- predict(
  m2,
  data.frame(sex = "men", lean.body.mass = avg_mass)
)
woman_pred_avg_lean_body_mass <- predict(
  m2,
  data.frame(sex = "women", lean.body.mass = avg_mass)
)
ggplot(strength, aes(x = lean.body.mass, y = strength, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, lty = 3) +
  geom_vline(xintercept = avg_mass, lty = 2) +
  geom_hline(aes(color = "men", yintercept = man_pred_avg_lean_body_mass)) +
  geom_hline(aes(color = "women", yintercept = woman_pred_avg_lean_body_mass)) +
  scale_color_discrete(name = "Sex", labels = c("Man", "Woman")) +
  scale_x_continuous(name = "Lean body mass (kg)") +
  scale_y_continuous(name = "Strength") +
  ggtitle("Balanced em-means (p=0.0996)")

# ------------------------------------------------------------------------------
# Marginalized em-means (means for the covariates are computed within the sexes)
# ------------------------------------------------------------------------------

emmeans(m2, ~sex, cov.reduce = lean.body.mass ~ sex)

# Here is the contrast of the marginalized ls-means between men and women
pairs(emmeans(m2, ~sex, cov.reduce = lean.body.mass ~ sex))

# We can plot this:
avg_mass_men <- mean(strength[strength$sex == "men", "lean.body.mass"])
avg_mass_women <- mean(strength[strength$sex == "women", "lean.body.mass"])
man_pred_avg_lean_body_mass <- predict(
  m2,
  data.frame(sex = "men", lean.body.mass = avg_mass_men)
)
woman_pred_avg_lean_body_mass <- predict(
  m2,
  data.frame(sex = "women", lean.body.mass = avg_mass_women)
)
ggplot(strength, aes(x = lean.body.mass, y = strength, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, lty = 3) +
  geom_vline(aes(xintercept = avg_mass_men, color = "men"), lty = 2) +
  geom_vline(aes(xintercept = avg_mass_women, color = "women"), lty = 2) +
  geom_hline(aes(color = "men", yintercept = man_pred_avg_lean_body_mass)) +
  geom_hline(aes(color = "women", yintercept = woman_pred_avg_lean_body_mass)) +
  scale_color_discrete(name = "Sex", labels = c("Man", "Woman")) +
  scale_x_continuous(name = "Lean body mass (kg)") +
  scale_y_continuous(name = "Strength") +
  ggtitle("Marginalized em-means (p<0.0001)")
