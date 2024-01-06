# --------------------------------------------------------------
# Applied Statistics / Statistical methods for SCIENCE
# Day 5: Random effects
# Germination of Orobanche seeds
# Anton Rask Lundborg
# December 14, 2023
# --------------------------------------------------------

# Load libraries
library(lme4)

# Read dataset: Recode some of the variables as factors
germination <- read.delim("orobanche.txt")
germination$batch <- factor(germination$batch)
str(germination)


# ---------------------------------------
# Fit model allowing for overdispersion
# ---------------------------------------

# Fit GLMM (generalized linear mixed effects model)
m1 <- glmer(cbind(yes, no) ~ variety * root + (1 | batch),
  family = binomial(link = "logit"), data = germination
)

# Validate normality of random effects
qqnorm(ranef(m1)$batch[, 1])

# Conclusion: Model is valid!

# Remark: Although the predicted random effects are not perfectly normal,
# you still might want to use the GLMM. The reason is, that allowing for
# overdispersion is more important than having a perfect model.

# Remark: Since the explanatory variables with fixed effects are categorical,
# and since we have used the saturated model (i.e. we have a 2-way ANOVA design,
# and we included all interactions, then this part of the model is valid by
# construction).

# Remark: If the model also included a continuous explanatory variable, say,
# then you could validate this part of the model using the cumulated residuals
# from the gof-package. Since the gof-package only works on models without
# random effects, then you could simply validate the corresponding glm-model
# where you do not include the random effects. If this simplified glm-model is
# ok, then you should be safe. However, if the simplified glm-model is not valid
# then the glmm model may still be valid.


# ----------------------------------
# Test for overdispersion
# ----------------------------------

# One possibility to test for overdispersion is to make a test
# for the null hypothesis that there is no random effect.

# To do this we fit the model without random effect, and compare using anova().
# It is important that the glmer-model appears as the first argument to anova()!
m0 <- glm(cbind(yes, no) ~ variety * root,
  family = binomial,
  data = germination
)
anova(m1, m0)

# While this test does suggest that the random effect is unnecessary, the 
# p-value is relatively small, so it may still be that the random effects model
# is a better representation of the real world.

# ---------------------------------------
# Is there an effect?
# ---------------------------------------

drop1(m1, test = "Chisq")

# p(variety:root,df=1) = 0.04125

# ---------------------------------------
# compare with other approaches
# ---------------------------------------

# Generalized Estimation Equations (GEE): p(variety:root,df=1) = 0.03736
library(gee)
m2 <- gee(cbind(yes, no) ~ variety * root,
  id = batch, family = binomial,
  data = germination, corstr = "independence"
)
summary(m2)

# There are no p-values by default but we get a z-score that we can convert
# to a p-value:
2 * (1 - pnorm(2.081777))

# Quasi binomial: p(variety:root, df=1) = 0.06357
m3 <- glm(cbind(yes, no) ~ variety * root,
  family = quasibinomial,
  data = germination
)
drop1(m3, test = "Chisq")
summary(m3)

# Plain logistic regression, that we did in m0, is questionably due to
# indication of overdispersion.
# It gives a (presumably) too small p-value: p(variety:root, df=1) = 0.01136
drop1(m0, test = "Chisq")
