# --------------------------------------------------------------
# Applied Statistics / Statistical methods for SCIENCE
# Day 4: Linear normal models
# Wage example
# Anton Rask Lundborg
# December 12, 2023
# --------------------------------------------------------------


# We load the data
wage <- read.delim("wage.txt")

# Select the subset of female (sex == 1) workers (occup == 5)
wage_subset <- subset(wage, (sex == 1) & (occup == 5))

# Fit a model
m1 <- lm(wage ~ edu + exper + age, data = wage_subset)

# Model diagnostics
par(mfrow = c(2, 2))
plot(m1)
par(mfrow = c(1, 1))

# The residuals do not look mean zero and the QQ plot also looks off.
# To fix this, we can try to transform the outcome variable!
# We use a Box-Cox transformation:
library(MASS)
bc <- boxcox(m1)
bc$x[which.max(bc$y)]

# The optimal lambda is -0.4242 so we could use -0.4; this corresponds to
# wage^{-0.4} = edu+exper+age
# or
# wage = (edu+exper+age)^{-2.5}
# This is a tricky model to interpret, so we instead try a log-transform:

m2 <- lm(log(wage) ~ edu + exper + age, data = wage_subset)

# Model diagnostics
par(mfrow = c(2, 2))
plot(m2)
par(mfrow = c(1, 1))

# These plots look much nicer than before!