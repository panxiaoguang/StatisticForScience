# --------------------------------------------------------------
# Applied Statistics / Statistical methods for SCIENCE
# Day 4: Growth of rats
# Anton Rask Lundborg
# December 11, 2023
# --------------------------------------------------------------

# Load libraries
library(LabApplStat)
library(emmeans)
library(tidyverse)

# Set theme
theme_set(theme_minimal())

# Create data frame
rats <- data.frame(
  antibio = rep(c(0, 40), each = 6),
  vitamin = rep(rep(c(0, 5), each = 3), times = 2),
  growth = c(
    1.30, 1.19, 1.08, 1.26, 1.21, 1.19,
    1.05, 1.00, 1.05, 1.52, 1.56, 1.55
  )
)

# Remark: Above 'antibio' and 'vitamin' have been encoded as numerical
# variables. This provides a better layout for the interaction plots.

# Interaction plots
ggplot(rats, aes(x = antibio, y = growth, col = factor(vitamin))) +
  geom_smooth(method = "lm") +
  geom_point() +
  scale_color_discrete(name = "vitamin") +
  ggtitle("Modification of antibio effect")

ggplot(rats, aes(x = vitamin, y = growth, col = factor(antibio))) +
  geom_smooth(method = "lm") +
  geom_point() +
  scale_color_discrete(name = "antibio") +
  ggtitle("Modification of vitamin effect")


# Remark: But for the design diagram it is better to have 'antibio' and
# 'vitamin' as categorical variables. So before proceeding we do the recoding.
rats$antibio <- factor(rats$antibio)
rats$vitamin <- factor(rats$vitamin)

# Design Diagram
plot(DD(~ antibio * vitamin, data = rats))


# Model fit, validation, and reduction
m1 <- lm(growth ~ antibio * vitamin, data = rats)
par(mfrow = c(2, 2))
plot(m1)
par(mfrow = c(1, 1))
drop1(m1, test = "F")
summary(m1)

# Where is the effect?
# Tukey grouping
(my.groups <- multcomp::cld(emmeans(m1, ~ antibio * vitamin),
  Letters = letters
))
my.groups$antibio <- factor(my.groups$antibio)
my.groups$vitamin <- factor(my.groups$vitamin)
ggplot(my.groups) +
  geom_bar(aes(x = antibio:vitamin, y = emmean, fill = antibio:vitamin),
    stat = "identity"
  ) +
  geom_errorbar(aes(x = antibio:vitamin, ymin = lower.CL, ymax = upper.CL),
    width = 0.25
  ) +
  geom_text(aes(
    x = antibio:vitamin, y = 1.05 * max(my.groups$upper.CL),
    label = .group
  )) +
  ylab("Estimated marginal mean") +
  guides(fill = "none")


# Corresponding plot of family-wise confidence intervals
plot(pairs(emmeans(m1, ~ antibio * vitamin), reverse = TRUE),
  int.adjust = "tukey"
) +
  geom_vline(xintercept = 0) +
  scale_y_discrete(labels = c(
    "A40V0 - A0V0", "A0V5 - A0V0", "A0V5 - A40V0",
    "A40V5 - A0V0", "A40V5 - A40V0", "A40V5 - A0V5"
  ))


# Family-wise confidence intervals of effect modifications
plot(pairs(emmeans(m1, ~ antibio | vitamin), reverse = TRUE)) +
  geom_vline(xintercept = 0) + scale_y_discrete(labels = c("A40-A0", "A40-A0"))
plot(pairs(emmeans(m1, ~ vitamin | antibio), reverse = TRUE)) +
  geom_vline(xintercept = 0) + scale_y_discrete(labels = c("V5-V0", "V5-V0"))
