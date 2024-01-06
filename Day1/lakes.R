# -------------------------------------------------------------
# Applied Statistics / Statistical methods for SCIENCE
# Day 1
# Example on two sample t-tests: Phosphorus in lakes
# Anton Rask Lundborg
# November 2023
# -------------------------------------------------------------

# Load packages and set ggplot2 theme
library(readxl)
library(ggplot2)
theme_set(theme_minimal(base_size = 14))


# Read data from Excel sheet:
lakes <- read_excel("lakes.xlsx")

# Let us investigate the data
str(lakes)
summary(lakes)

# The read_excel()-function provides a 'tibble' which produces a nice output
# when simply printed
lakes

# Graphical investigation of normality within groups using the ggplot-package
# Remark: To avoid confusion we insert labels on the y-axis
ggplot(lakes, aes(sample = phosphorus)) +
  ylab("Phosphorus concentration") +
  facet_grid(. ~ location) +
  geom_qq() +
  geom_qq_line()
ggplot(lakes, aes(sample = log(phosphorus))) +
  ylab("log(Phosphorus concentration)") +
  facet_grid(. ~ location) +
  geom_qq() +
  geom_qq_line()

# Normal quantile plots shows that the cell counts should be
# log transformed. Let us confirm this by some numerical Goodness-of-Fit tests,
# although usually graphical summaries suffice.
# Remarks:
# 1) Note the difference in the statistical power.
# 2) High power is not always desired here!?
# 3) Note that there is warning message from the Kolmogorov-Smirnov test.
#             Often such warnings are simply ignored.
by(lakes$phosphorus, lakes$location, shapiro.test)
by(lakes$phosphorus, lakes$location, function(x) {
  ks.test(x, "pnorm", mean(x), sd(x))
})

# The two sample t-test without assuming equal variances
t.test(log(phosphorus) ~ location, data = lakes)


# ------------------------------------------------------------
# For illustration -- short syntax for doing the correct analysis
# ------------------------------------------------------------
MASS::boxcox(lm(phosphorus ~ location, data = lakes))
par(mfrow = c(2, 2))
plot(lm(log(phosphorus) ~ location, data = lakes))
t.test(log(phosphorus) ~ location, data = lakes)

# Remarks: 1) The BoxCox analysis suggests that the
#             logarithm (lambda=0) is a good transformation.
#          2) The validation plots look ok!
#          3) WARNING: The validation done using plot(lm()) implicitly assumes
#             equal variances in the two groups. So this can be misleading when
#             the variances are very different.
#          4) We do the Welch test.
