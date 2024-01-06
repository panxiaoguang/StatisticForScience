# -------------------------------------------------------------
# Applied Statistics / Statistical methods for SCIENCE
# Day 3: Categorical regression: Cheese example
# Anton Rask Lundborg
# December 3, 2023
# -------------------------------------------------------------

# Load libraries we will be using
library(ordinal)

# Read data from txt-file
cheese <- read.delim("cheese.txt")
summary(cheese)
str(cheese)

# The response variable (taste) should be recoded as a factor for clm() to work
cheese$taste <- factor(cheese$taste)
str(cheese)

# Fit multinomial and proportional odds model
m0 <- clm(taste ~ 1,
  nominal = ~cheese, data = cheese, weights = count,
  control = list(method = "nlminb")
)
m1 <- clm(taste ~ cheese, data = cheese, weights = count)

# Lack-of-Fit test for proportional odds assumption
anova(m1, m0)

# After the proportional odds assumption has been validated
# we test the hypothesis that the cheeses are equally tasty
drop1(m1, test = "Chisq")

# Effect of cheese is highly significant.
# We proceed by giving estimates for odds ratios.
# Remarks:
# 1) clm() uses a parametrization implying that
#    odds ratios are for having a larger response,
#    i.e. here for being more tasty.
# 2) The first 8 parameters listed in coef(m1)
#    describe the contrasts between the 9 taste scores.
#    Below we extract the last 3 parameters, which describes
#    the log(OR's) between the cheeses.
round(exp(cbind("OR vs cheese A" = coef(m1)[9:11], confint(m1))), 4)

# emmeans-package can be used for clm-objects, but it appears that
# automatic backtransformation is not available!?
library(emmeans)
confint(pairs(emmeans(m1, ~cheese), reverse = TRUE))
