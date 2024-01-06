# -------------------------------------------------------------
# Applied Statistics / Statistical methods for SCIENCE
# Design of experiments:
#   1) Half fraction factorial design
#   2) Plackett-Burman design
# Anton Rask Lundborg
# December 15, 2023
# -------------------------------------------------------------

# Example from lecture ----

# load libraries
library(AlgDesign)

# Create full factorial design for 5 factors on 2 levels each
full_factorial <- gen.factorial(
  levels = 2, nVars = 5,
  varNames = c("FeedRate", "Catalyst", "AgitRate", "Temp", "Conc")
)
full_factorial

# Create a design with 16 units
# for main effects and 2-way interactions between 5 factors on 2 levels
optFederov(~ (FeedRate + Catalyst + AgitRate + Temp + Conc)^2,
  data = full_factorial, nTrials = 16
)$design

# An error message is given if you try to create a design with too few units
optFederov(~ (FeedRate + Catalyst + AgitRate + Temp + Conc)^2,
  data = full_factorial, nTrials = 15
)$design


# Plackett-Burman design with 12 experimental runs ----

library(FrF2)
pb(12)
