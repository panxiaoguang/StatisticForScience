# --------------------------------------------------------------
# Applied Statistics / Statistical methods for SCIENCE
# Day 5: Random effects
# Data example: Redness of pork chops
# Anton Rask Lundborg
# December 14, 2023
# --------------------------------------------------------

# Load libraries
library(LabApplStat)
library(lme4)

# Set theme
theme_set(theme_minimal())

# Read dataset: Recode some of the variables as factors
redness <- read.delim("redness.txt")
redness$pig <- factor(redness$pig)
redness$time <- factor(redness$time)
str(redness)

# ---------------------------------------
# Fit valid model
# ---------------------------------------

# Fit random effects model
m0 <- lmer(redness ~ breed * storage * time + (1 | pig), data = redness)

# Residual plot
plot(m0)

# Normal quantile plot for residuals
qqnorm(residuals(m0))

# Normal quantile plot for random effects
qqnorm(ranef(m0)$pig[, 1])

# Conclusion: This model does not appear valid, probably due to observation 44.
# Thus, we remove this outlier and revalidate the model

# Fit random effects model
m1 <- lmer(redness ~ breed * storage * time + (1 | pig), data = redness[-44, ])

# Model validation
plot(m1)
qqnorm(residuals(m1))
qqnorm(ranef(m1)$pig[, 1])

# Conclusion: Validation plots look good! Model is valid.

# Design Diagram
plot(
  DD(redness ~ breed * storage * time, random = ~pig, data = redness[-44, ]),
  "MSS"
)

# ---------------------------------------
# Is there an effect? : Model reduction
# ---------------------------------------

# Remark: drop1 refits using maximum likelihood.
# This may be seen comparing AIC(m1) to drop1(m1)
AIC(m1)

# Test and reduce model
drop1(m1, test = "Chisq")
m2 <- update(m1, . ~ . - breed:storage:time)
drop1(m2, test = "Chisq")
m3 <- update(m2, . ~ . - breed:time)
drop1(m3, test = "Chisq")
m4 <- update(m3, . ~ . - breed:storage)
drop1(m4, test = "Chisq")

# Model validation of selected model
plot(m4)
qqnorm(residuals(m4))
qqnorm(ranef(m4)$pig[, 1])

# Model looks good!

# p(breed)        = 0.0141, df=1
# p(storage:time) = 0.0001, df=2

# Remark:
# 1) The usual step() function for doing automated model selection
# does not work for lmer-models. However, you may use the step()
# function from the lmerTest-package.
# 2) The default behavior for lmerTest::step() is that it also tries to
# remove random effects. This is usually not a good idea. So to keep all random 
# effects set the option reduce.random=FALSE.
# 3) Another thing is that lmerTest::step() uses p-values instead of AIC to 
# select models. It can be useful to set a higher significance level (e.g. 10%)
# when using this function.
# 4) Finally, a technicality: The lmerTest-package has its own version
# of the lmer() function and the model has to be fitted using this version
# in order for the associated step() function to work.
# 5) If you load the full lmerTest-package (with library(lmerTest)), it will
# override the lmer function from the lme4 package with the lmer function from
# the lmerTest function. This is rarely an issue but can cause confusion and we
# therefore avoid loading the package below.
#
# As an example:

m5 <- lmerTest::lmer(redness ~ breed * storage * time + (1 | pig),
  data = redness[-44, ]
)
lmerTest::step(m5, reduce.random = FALSE, alpha.fixed = 0.1)


# ---------------------------------------
# Where is the effect? : emmeans
# ---------------------------------------

# breed
pairs(emmeans(m4, ~breed), reverse = TRUE)
confint(pairs(emmeans(m4, ~breed), reverse = TRUE))

# storage:time
multcomp::cld(emmeans(m4, ~ storage:time))

# visualization of effect of time within storage types
plot(emmeans(m4, ~ time | storage), int.adjust = "tukey", horizontal = FALSE) +
  ylab("Redness of meat") +
  ggtitle("95pct simultaneous confidence intervals within storage")


# ----------------------------------------
# Quantification of sources of variation
# ----------------------------------------

summary(m4)

cat("Total variance=", 0.200 + 1.923, "\n")
cat(
  "  percentage from variation between pigs=",
  round(100 * 0.200 / (0.200 + 1.923)), "\n"
)
cat(
  "  percentage from other sources=",
  round(100 * 1.923 / (0.200 + 1.923)), "\n"
)
