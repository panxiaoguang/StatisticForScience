# -------------------------------------------------------------
# Applied Statistics / Statistical methods for SCIENCE
# Day 3: Categorical regression: Hypertension example
# Anton Rask Lundborg
# December 3, 2023
# -------------------------------------------------------------

# Load libraries we will be using
library(gof)
library(emmeans)


# Read data from txt-file
hypertension <- read.delim("hypertension.txt")
hypertension

# Fit saturated logistic regresion
m1 <- glm(cbind(yes, no) ~ snoring * obese * smoking,
  data = hypertension, family = binomial
)

# Automated model selection. (AIC-based)
step(m1, direction = "both")

# Investigation of selected model
m2 <- glm(cbind(yes, no) ~ snoring + obese,
  data = hypertension, family = binomial
)
drop1(m2, test = "Chisq")
plot(cumres(m2, R = 10000)) # R controls number of simulations
exp(cbind(OR = coef(m2), confint(m2)))


# Model reporting using the em-means methodology -------

# probabilities
emmeans(m2, ~ snoring * obese, type = "response")


# pairwise comparisons (=odds ratios) with simultaneous confidence intervals
confint(pairs(emmeans(m2, ~ snoring * obese, type = "response"),
  reverse = TRUE
))

# pairwise comparisons (=odds ratios) with marginal confidence intervals
confint(pairs(emmeans(m2, ~ snoring * obese, type = "response"),
  reverse = TRUE, adjust = "none"
))

# pairwise comparisons (=odds ratios) within(!) obesity classes
# Questions to challenge your understanding
#  (if you cannot answer, do not despair - we will return to this again later in
# the course)
# 1) Why are the confidence intervals from the call below identical to
#    the "marginal confidence intervals" from above?
# 2) Why are the odds ratio for hypertension between snoring and non-snoring 
# identical for "obese = no" and "obese = yes"?
confint(pairs(emmeans(m2, ~ snoring | obese, type = "response"),
  reverse = TRUE
))
