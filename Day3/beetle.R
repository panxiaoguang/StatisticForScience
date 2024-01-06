# --------------------------------------------------------------
# Applied Statistics / Statistical methods for SCIENCE
# Day 3: Categorical Regression
# Beetle example
# Anton Rask Lundborg
# December 1, 2023
# --------------------------------------------------------------

# Install gof-package from GitHub
# > install.packages("devtools")
# > devtools::install_github("kkholst/gof")

# Load libraries we will be using
library(dobson)
library(gof)
library(LabApplStat)

# Library to enable fancy annotations
library(latex2exp)

# Set theme
theme_set(theme_minimal())

# Use beetle data from dobson package
data(beetle)

# Create linear plot
ggplot(beetle, aes(x = x, y = y / n)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab(TeX("$\\log_{10}$(CS_2 mg/l)")) +
  ylab("Proportion of dead beetles") 

# Make plot of design in relation to Lack-of-Fit test.
plot(DD(~ x + factor(x), data = beetle))


# Perform probit regression
m1 <- glm(cbind(y, n - y) ~ x,
  data = beetle,
  family = binomial(link = "probit")
)

# Construct probit plot
p1<-ggplot(beetle, aes(x = x, y = qnorm(y / n))) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab(TeX("$\\log_{10}$(CS_2 mg/l)")) +
  ylab(TeX("$\\Phi^{-1}$(proportion of dead beetles)"))+
  scale_x_continuous(limits = c(1.6,1.9))+
  scale_y_continuous(limits = c(-3,3))
p2<-ggplot(beetle, aes(x = x, y = qnorm(y / n, mean =1.771 ,sd = 0.05068954))) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab(TeX("$\\log_{10}$(CS_2 mg/l)")) +
  ylab(TeX("$\\Phi^{-1}$(proportion of dead beetles)"))+
  scale_x_continuous(limits = c(1.6,1.9))+
  scale_y_continuous(limits = c(-3,3))

ggsave("tmp.png", width=3, height=3)

# Create residual plot
residual_plot_df <- beetle
residual_plot_df$residuals <- qnorm(beetle$y / beetle$n) - predict(m1)

ggplot(residual_plot_df, aes(x = x, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, lty = 2) +
  xlab(TeX("$\\log_{10}$(CS_2 mg/l)"))


# Compute cumulative residuals
par(mfrow = c(2, 1))
plot(cumres(m1, R = 10000)) # R specifies number of simulations
par(mfrow = c(1, 1))

# Construct a model where dose is used as a categorical factor
m0 <- glm(cbind(y, n - y) ~ factor(x),
  data = beetle,
  family = binomial(link = "probit")
)

# Lack-of-Fit test: Test m1 as a hypothesis in m0
anova(m1, m0, test = "Chisq")

# Hypothesis test
drop1(m1, test = "Chisq")

# Parameter estimates and confidence intervals
cbind(estimate = coef(m1), confint(m1))

# Scale parameter in the tolerance distribution
1 / cbind(estimate = coef(m1), confint(m1))[2, c(1, 3, 2)]

# Mean parameter in the tolerance distribution
# This may also be used for other quantiles than 50pct
emmeans_ED(m1, p = 0.5, tran = "log10")

# Alternatively via the car-package
car::deltaMethod(m1, "-alpha/beta", parameterNames = c("alpha", "beta"))


# ----------------------------------------------------------------
# Plot of predicted probabilities (with 95pct confidence limits)
# ----------------------------------------------------------------

# Predictions with approximate 95pct confidence limits
new_x <- seq(1.6, 1.9, 0.005)
bHat <- predict(m1, newdata = data.frame(x = new_x), se.fit = TRUE)
pHat <- pnorm(bHat$fit)
lowerCI <- pnorm(bHat$fit - 1.96 * bHat$se.fit)
upperCI <- pnorm(bHat$fit + 1.96 * bHat$se.fit)


pHat <- pnorm(bHat$fit,mean =1.771 ,sd = 0.05068954)
lowerCI <- pnorm(bHat$fit - 1.96 * bHat$se.fit,mean =1.771 ,sd = 0.05068954)
upperCI <- pnorm(bHat$fit + 1.96 * bHat$se.fit,mean =1.771 ,sd = 0.05068954)

colors <- c(
  "data" = "black", "model" = "blue",
  "95% confidence interval" = "lightblue"
)
ggplot() +
  geom_ribbon(aes(
    x = new_x, ymin = lowerCI, ymax = upperCI,
    fill = "95% confidence interval"
  ), col = "lightblue") +
  geom_line(aes(x = new_x, y = pHat, col = "model")) +
  geom_point(aes(x = x, y = y / n, col = "data"), data = beetle) +
  labs(
    y = "Probability for death",
    x = TeX("$\\log_{10}(CS_2$ mg/l)"),
    col = "",
    fill = ""
  ) +
  scale_color_manual(values = colors[1:2], limits = names(colors[1:2])) +
  scale_fill_manual(values = colors[3]) 
