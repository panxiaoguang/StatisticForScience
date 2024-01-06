# --------------------------------------------------------------
# Applied Statistics / Statistical methods for SCIENCE
# Repeated measurements: Growth of Baobab trees
# Anton Rask Lundborg
# December 14, 2023
# -----------------------------------------------

# Load libraries ----
library(readxl)
library(nlme)
library(MASS)
library(emmeans)
library(tidyverse)


# Set theme
theme_set(theme_minimal())

# Read Excel sheet ----
baobab <- read_excel("baobab_growth.xlsx")
baobab$Plant <- factor(baobab$Plant)
baobab$Block <- factor(baobab$Block)
baobab$Treatment <- factor(baobab$Treatment)
baobab

# We choose to exclude non-harvested trees
# In particular, the analysis is conditional on survival
# until harvest.
baobab <- subset(baobab, !is.na(HarvestDate))
baobab$Plant <- droplevels(baobab$Plant)
baobab

# Transform to long form ----
# Remarks:
# 1) Here done using the pivot_longer() from the tidyverse.
#    Since we transform both the diameter (=Dia) and
#    the height(=Hei) in one go the syntax is somewhat complicated.
# 2) In the first mutate() step we define plant age as months since
#    January, 2009. The as.numeric() is necessary to make the
#    conversion from characters to numbers.
# 3) In the rename() step we change variable names from Dia and Hei
#    to diameter and height, respectively.
long <- baobab %>%
  pivot_longer(
    cols = Dia0209:Hei0110,
    names_to = c(".value", "month", "year"),
    names_sep = c(3, 5),
    values_drop_na = TRUE
  ) %>%
  mutate(age = (as.numeric(month) - 1) + 12 * (as.numeric(year) - 09)) %>%
  mutate(Treatment = factor(Treatment)) %>%
  rename(diameter = Dia, height = Hei) %>%
  filter(diameter > 0) %>%
  filter(height > 0)


# Find optimal Box-Cox transformation of the two responses ----

# Conclusion: use diameter^(2/3)
tmp <- MASS::boxcox(
  lm(diameter ~ Treatment * Block + Treatment * factor(age) * Origin,
    data = long
  )
)
tmp$x[which.max(tmp$y)]

# Conclusion: use height^(1/2)
tmp <- MASS::boxcox(
  lm(height ~ Treatment * Block + Treatment * factor(age) * Origin,
    data = long
  )
)
tmp$x[which.max(tmp$y)]


# Visualization of raw data ----

# Individual profiles

ggplot(long) +
  geom_line(aes(x = age, y = diameter^(2 / 3), group = Plant)) +
  facet_grid(. ~ Country) +
  scale_x_continuous(breaks = seq(2, 12, 2))

ggplot(long) +
  geom_line(aes(x = age, y = height^(1 / 2), group = Plant)) +
  facet_grid(. ~ Country) +
  scale_x_continuous(breaks = seq(2, 12, 2))


# Average profiles

ggplot(long) +
  geom_smooth(aes(x = age, y = diameter^(2 / 3), group = Treatment,
                  col = Treatment)) +
  facet_grid(. ~ Country) +
  scale_x_continuous(breaks = seq(2, 12, 2))


# Fit three repeated measurements models ----

mRI <- lme(
  diameter^(2 / 3) ~ Treatment * Block + Treatment * factor(age) * Origin,
  random = ~ 1 | Plant,
  data = long, na.action = na.omit
)

mExp <- lme(
  diameter^(2 / 3) ~ Treatment * Block + Treatment * factor(age) * Origin,
  random = ~ 1 | Plant,
  corr = corExp(form = ~ age | Plant, nugget = TRUE),
  data = long, na.action = na.omit
)

mGauss <- lme(
  diameter^(2 / 3) ~ Treatment * Block + Treatment * factor(age) * Origin,
  random = ~ 1 | Plant,
  corr = corGaus(form = ~ age | Plant, nugget = TRUE),
  data = long, na.action = na.omit
)

# Selection and validation of initial model ----

# Extract AIC: The AIC suggests that Exponential decrease model should be used
anova(mRI, mExp, mGauss)

# Validation of Exponential decrease model ----
plot(Variogram(mExp), ylim = c(0, 0.7), main = "Exponential decrease model")
plot(mExp, main = "Exponential decrease model")
qqnorm(resid(mExp), main = "Exponential decrease model")


# Validation of Diggle model
plot(Variogram(mGauss), ylim = c(0, 1.1), main = "Diggle model")
plot(mGauss, main = "Diggle model")
qqnorm(resid(mGauss), main = "Diggle model")


# Automatic model reduction using AIC ----

# Using stepAIC() from the MASS-package.
# Remember to refit the model using method="ML" first!

mExp_ML <- lme(
  diameter^(2 / 3) ~ Treatment * Block + Treatment * factor(age) * Origin,
  random = ~ 1 | Plant,
  corr = corExp(form = ~ age | Plant, nugget = TRUE),
   data = long, na.action = na.omit,
  method = "ML"
)
stepAIC(mExp_ML, direction = "both", trace = 6)

# Remark: The computations take time! So be patient!
# Remark: The trace = 6 option allows you to see the function at work.
#         In particular, you can see that it is doing something :-).


# Refit selected model using REML ----
mExp_final <- lme(
  diameter^(2 / 3) ~ Treatment + Block + factor(age) + Origin +
    Treatment:factor(age) + Treatment:Origin + factor(age):Origin,
  random = ~ 1 | Plant, corr = corExp(form = ~ age | Plant, nugget = TRUE),
  data = long, na.action = na.omit
)

# Visualization of selected model ----
my_emm <- as.data.frame(emmeans(mExp_final, ~ factor(age) | Treatment:Origin))

# We backtransform by hand
ggplot(my_emm) +
  geom_line(aes(x = age, y = emmean^(3 / 2), group = Treatment,
                col = Treatment)) +
  geom_errorbar(aes(x = age, ymin = lower.CL^(3 / 2), ymax = upper.CL^(3 / 2),
                    col = Treatment)) +
  facet_wrap(. ~ Origin, ncol = 4) +
  scale_x_continuous(breaks = seq(2, 12, 2)) +
  ylab("Diameter")




# Construction of summary measures ----

# As emphasized in the lectures good summary measures are chosen based
# on scientific consideration about the "biologically" relevant features
# of the response profiles.

# This being said, there are also technicalities to do with creating
# the desired summary measures in practice. Below fancy
# code is used to fit a quadratic regression separately to each of the response
# profiles, and we extract the curvature as a summary measure. Please note
# that this is not a sensible and biologically meaningful summary measure
# for this data example. We will still do it for the sake of showing the
# R code.


# Fitting individual quadratic regressions of each of the Plants
# Remark: This is also done on the long format data!

summary_measures <- long %>%
  group_by(Plant) %>%
  nest() %>%
  mutate(
    param = map(
      data,
      ~ coef(lm(diameter^(2 / 3) ~ 1 + age + I(age^2), data = .))
    )
  ) %>%
  unnest_wider(param) %>%
  rename(slope = age, curvature = "I(age^2)")

# Combine the summary measures with the data on the wide format,
# and select the relevant variables for the further analysis
mydata <- full_join(baobab, summary_measures) %>%
  select(Country, Origin, Plant, Block, Treatment, slope, curvature)
mydata

# Finally, we do a 3-way ANOVA with random effect of block ----

# Fit model and validate model
library(lme4)
m0 <- lmer(
  curvature ~ Origin * Treatment + Country * Treatment + (Treatment | Block),
  data = mydata
)
plot(m0)
qqnorm(residuals(m0))

# Try random transformation (which is completely arbitrary)
m1 <- lmer(
  sqrt(0.1 + curvature) ~ Origin * Treatment + Country * Treatment +
    (Treatment | Block), data = mydata
)
plot(m1)
qqnorm(residuals(m1))

# The model does look better (but perhaps still not good). We proceed anyway:

# Model reduction
drop1(m1, test = "Chisq")
m1 <- update(m1, . ~ . - Origin:Treatment)

drop1(m1, test = "Chisq")
m1 <- update(m1, . ~ . - Country:Treatment)

drop1(m1, test = "Chisq")
m1 <- update(m1, . ~ . - Treatment)

drop1(m1, test = "Chisq")
m1 <- update(m1, . ~ . - Origin)

drop1(m1, test = "Chisq")
m1 <- update(m1, . ~ . - Country)

summary(m1)
# Remark: Nothing is found! This is not a surprise given the bad choice of
# summary measure.

# So let us try with a more natural summary measure, namely the slope.

m2 <- lmer(
  slope ~ Origin * Treatment + Country * Treatment + (Treatment | Block),
  data = mydata
)
plot(m2)
qqnorm(residuals(m2))
# Looks okay!

# Model reduction
drop1(m2, test = "Chisq")
m2 <- update(m2, . ~ . - Origin:Treatment)

drop1(m2, test = "Chisq")
m2 <- update(m2, . ~ . - Country:Treatment)

drop1(m2, test = "Chisq")
m2 <- update(m2, . ~ . - Origin)

drop1(m2, test = "Chisq")
m2 <- update(m2, . ~ . - Country)

drop1(m2, test = "Chisq")

# Here we indeed find a (borderline) significant effect of
# treatment on the slope!

plot(emmeans(m2, ~Treatment))
