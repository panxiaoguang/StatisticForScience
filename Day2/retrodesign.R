# Post hoc power computation
#
# Arguments:
# A         = numeric vector with effect size.
# SE        = numeric with standard error (default=1).
# sig.level = significance level (default=0.05).
# df        = degrees of freedom (default=infinity).
# B         = number of simulations used for typeM computation (default=10000).
#
# Value: return data frame with columns
# power        = power of test
# typeS        = risk of Type S error
# exaggeration = average exaggeration ratio
#
# References: R code adapted from
# Gelman & Carlin: "Beyond Power Calculations: Assessing Type S (Sign) and
# Type M (Magnitude) Errors", Perspectives on Psychological Science, 1-11, 2014.
#
# Author: Anton Rask Lundborg
# Date: November 28, 2023

retrodesign <- function(A, SE = 1, sig_level = 0.05, df = Inf, B = 10000) {
  # A         = effect size (may be a vector)
  # SE        = standard error (default=1)
  # sig.level = significance level (default=5%)
  # df        = degrees of freedom (default=infinity)
  # B         = number of simulations used for typeM computation (default=10000)
  if (any(A < 0)) stop("effect size must be non-negative")
  z <- qt(1 - sig_level / 2, df)
  p_hi <- 1 - pt(z - A / SE, df)
  p_lo <- pt(-z - A / SE, df)
  power <- p_hi + p_lo
  type_S <- p_lo / power
  mysims <- rt(B, df)
  type_M <- vapply(A, function(a) {
    estimate <- a + SE * mysims
    mean(abs(estimate[abs(estimate) > SE * z])) / a
  }, FUN.VALUE = 1)
  return(data.frame(effect = A, SE = SE, power = power,
                    type_S = type_S, exaggeratio = type_M))
}
