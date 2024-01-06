# --------------------------------------------------------------
# Applied Statistics / Statistical methods for SCIENCE
# Day 3: Categorical regression
# Company default example
# Anton Rask Lundborg
# December 3, 2023
# -----------------------------------------

# Install gof-package from github:
# > install.packages("devtools")
# > devtools::install_github("kkholst/gof")

# Load used packages
library(readr)
library(gof)
library(LabApplStat)
library(tidyverse)

# Set theme
theme_set(theme_minimal())

# Read dataset
company_default <- read_csv("Company_default.csv")

# Only use companies that are less than 1 year old
young <- company_default %>%
  filter(Age < 366) %>%
  select("Age", "Equity", "Equity_sqrt", "Default_nextyear")


# The variable Equity_sqrt contains the signed square root of
# the Equity variable. Let's check that graphically

ggplot(young, aes(x = sqrt(abs(Equity)), y = Equity_sqrt)) +
  geom_point()

# It turns out that a logistic regression models works well
# against the Equity_sqrt variable. Why is unclear, but this is
# what we will do in this example.

# Remark: There are 4 young companies with a surprising large
# equity, that is, with equity 100,000,000 DKK or more. None of
# of these default within the next year. These companies do not
# pose any modelling problems, but they are the cause of a
# warning message when we later fit a logistic regression model,
# namely this one:
#
# Warning message:
# glm.fit: fitted probabilities numerically 0 or 1 occurred
#
# If you are interested in this technicality, then ask for
# clarification.

# For the first introduction of the example in the lectures we want
# the explanatory variable to be categorical. So let us define
# some quantiles to split the data according to Equity_sqrt.
# Remark: These were found by trail-and-error in order to
#         serve the story of the lecture.
(my_breaks <- seq(-1000, 1000, length.out = 5))

# We create the corresponding cross-tabulation table using some
# fancy R code. Ask if you want to know more details
# about this.
young <- mutate(young, group = sapply(Equity_sqrt, function(x) {
  1 + sum(x > my_breaks)
}))
my_table <- table(young$Default_nextyear, young$group)
rownames(my_table) <- c(" No", "Yes")
colnames(my_table) <- c(my_breaks, "more")
my_table <- my_table[2:1, ]
addmargins(my_table)

# Probit analysis ----
mydata <- data.frame(
  default = my_table["Yes", ],
  no_default = my_table[" No", ], group = as.numeric(1:6)
)
m1 <- glm(cbind(default, no_default) ~ group,
  data = mydata,
  family = binomial(link = "probit")
)
summary(m1)

# Estimates for parameters in tolerance distribution
# 1. Standard deviation in tolerance distribution = -1/slope = -1/parameter(group).
#    Since the parameter is negative (!) we change the sign on both parameters in
#    the model, and have the interpretation for "no-default".
-1 / cbind(estimate = coef(m1), confint(m1))["group", ]

# 2. Mean in tolerance distribution = LD50 = -intercept/slope:
# Remark: Although we changed sign of the slope, the formula
#         for the LD50 is the same!
emmeans_ED(m1, p = 0.5)


# Model validation
# Remark: gof-package does not work! Apparently this is due to counts being too
# large!? This is both very strange as well as very unfortunate!
plot(cumres(m1, R = 1000))

# Let us compute odds and odds ratios by hand
(odds <- my_table[1, ] / my_table[2, ])
(odds_ratios <- odds[-6] / odds[-1])
round(odds_ratios, 3)


# Let us fit and validate the corresponding logistic regression
# Remark: gof-package does not work! Apparently this is due to counts being too
# large!? This is both very strange as well as very unfortunate!
m1 <- glm(Default_nextyear ~ group, data = young, family = binomial())
plot(cumres(m1))

# Lack-of-fit test
m0 <- glm(Default_nextyear ~ factor(group), data = young, family = binomial())
anova(m1, m0, test = "Chisq")

# table-of-counts vs. logistic regression slide ----
prop_pred <- t(apply(my_table, 2, function(y) {
  tmp <- prop.test(x = y[1], n = sum(y))
  return(c(tmp$estimate, tmp$conf.int))
}))
colnames(prop_pred) <- c("prob", "lower.CI", "upper.CI")
(prop_pred <- cbind(data.frame(group = 1:6), prop_pred))
#
tmp <- predict(m1,
  newdata = data.frame(group = seq(1, 6, 0.01)),
  type = "response", se.fit = TRUE
)
glm_pred <- data.frame(
  group = seq(1, 6, 0.01), prob = tmp$fit,
  lower.CI = tmp$fit - 1.96 * tmp$se.fit, upper.CI = tmp$fit + 1.96 * tmp$se.fit
)
#
colors <- c("Table of counts" = "black", "Logistic regression" = "blue")
ggplot() +
  geom_ribbon(aes(x = group, ymin = lower.CI, ymax = upper.CI),
    glm_pred,
    fill = "lightblue"
  ) +
  geom_line(aes(x = group, y = prob, col = "Logistic regression"), glm_pred) +
  geom_point(aes(x = group, y = prob, col = "Table of counts"), prop_pred) +
  geom_errorbar(aes(
    x = group, ymin = lower.CI, ymax = upper.CI,
    col = "Table of counts"
  ), prop_pred) +
  scale_color_manual(values = colors) +
  labs(
    y = "Probability of default within next year",
    x = "Equity group",
    col = "Statistical model"
  ) +
  ylim(0, 1) +
  ggtitle("Observed proportions and predicted probabilities")
