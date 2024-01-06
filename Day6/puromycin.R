# -------------------------------------------------------------
# Applied Statistics / Statistical methods for SCIENCE
# Non-linear regression: Puromycin
# Anton Rask Lundborg
# December 15, 2023
# -------------------------------------------------------------

# Read data from text file
puromycin <- read.delim("puromycin.txt")
summary(puromycin)

# Create plot
plot(reaction ~ concentration, data = puromycin,
  xlab = "Concentration", ylab = "Reaction velocity"
)

# Create interactive plot
library(manipulate)
manipulate(
  {
    plot(reaction ~ concentration, data = puromycin)
    x <- seq(0, 1.1, 0.01)
    y <- a * x / (b + x)
    lines(x, y)
  },
  a = slider(100, 300, initial = 200, step = 1),
  b = slider(0.01, 0.2, initial = 0.1, step = 0.01)
)

# Fit non-linear regression
m1 <- nls(
  reaction ~ a * concentration / (b + concentration),
  start = list(a = 200, b = 0.1), data = puromycin
)

# Can the conclusions be trusted?
qqnorm(residuals(m1))

plot(predict(m1), residuals(m1),
  xlab = "Predicted reaction velocity", ylab = "Residual",
  main = "Residual by Predicted plot: Puromycin"
)
abline(0, 0, lty = 2, lwd = 2)

# Well, model validity is not very convincing.
# So we also try a Lack-of-Fit test, which is possible since we have
# two observations at each concentration
m0 <- lm(reaction ~ factor(concentration), data = puromycin)
par(mfrow = c(2, 2))
plot(m0)
par(mfrow = c(1, 1))
anova(m1, m0)

# The saturated model is acceptable (although large variance at lowest
# concentration), and the Lack-of-Fit test is not critical (p = 0.4468).

# Thus, we proceed with the Michaelis-Menten model, and give parameter estimates
summary(m1)
cbind(estimate = coef(m1), confint(m1))

# Joint plot of observed data and model prediction
plot(reaction ~ concentration, data = puromycin,
  xlab = "Concentration", ylab = "Reaction velocity",
  main = "Michaelis-Menten kinetics"
)
x <- seq(0, 1.2, 0.01)
lines(x, predict(m1, newdata = data.frame(concentration = x)), lwd = 2)
