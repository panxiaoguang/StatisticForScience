# -------------------------------------------------------------
# Applied Statistics / Statistical methods for SCIENCE
# Day 2
# Comparing two proportions: Avadex example
# Anton Rask Lundborg
# November 26, 2023
# -------------------------------------------------------------

# First we try the chi-squared test(with and without correction). For
# applications it is recommended that you always use the correction (which is
# also the default).
prop.test(c(4, 5), n = c(16, 79))
prop.test(c(4, 5), n = c(16, 79), correct = FALSE)

# For this particular data the decision on whether to reject the null hypothesis
# depends on whether the continuity correction was used or not.

# In general the continuity correction will give more valid p-values. However,
# in this special case R issues a warning that both tests are unreliable. So
# instead we simulate the p-value. This is done conditioning on the row
# marginals as these have presumably been fixed by design!
library(LabApplStat)
chisq.test.simulate(matrix(c(4, 5, 12, 74), 2, 2), "row")

# Since the simulated p-value is significant (p = 0.02) we estimate the
# proportion in each of the treatment groups. The following code is an easy way 
# of getting the confidence intervals.
prop.test(4, n = 16)
prop.test(5, n = 79)
