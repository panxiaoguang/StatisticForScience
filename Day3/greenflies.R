# -------------------------------------------------------------
# Applied Statistics / Statistical methods for SCIENCE
# Day 3: Categorical regression: Greenflies example
# Anton Rask Lundborg
# December 3, 2023
# -----------------------------------------------

# Load libraries we will be using
library(gof)
library(emmeans)

# Read data from txt-file
greenflies <- read.delim("greenflies.txt")
greenflies

# Fit saturated Poisson regresion
m1 <- glm(number ~ system * week * leaf, data = greenflies, family = poisson())

# Automated model selection
step(m1, direction = "both")

# Investigation of selected model
m2 <- glm(number ~ system + week + leaf + system:week + week:leaf,
  data = greenflies, family = poisson()
)
drop1(m2, test = "Chisq")
plot(cumres(m2, R=10000)) # R controls the number of simulations

# The parameters in the final model are
cbind(logRR = coef(m2), confint(m2))
exp(cbind(RR = coef(m2), confint(m2)))

# em-means
pairs(emmeans(m2, ~ system | week), reverse = TRUE)
confint(pairs(emmeans(m2, ~ system | week), reverse = TRUE), type = "response")
