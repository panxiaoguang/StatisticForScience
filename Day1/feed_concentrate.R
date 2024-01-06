# -------------------------------------------------------------
# Applied Statistics / Statistical methods for SCIENCE
# Day 1
# Example on feed concentrate
# Anton Rask Lundborg
# November 2023
# -------------------------------------------------------------

# Set ggplot options
library(ggplot2)
theme_set(theme_minimal(base_size = 14))

# Set seed (this ensures reproducibility when randomness is involved)
set.seed(111123)

# Read data
low <- c(4132, 3672, 3664, 4292, 4881, 4287, 4087, 4551)
high <- c(3860, 4130, 5531, 4259, 4908, 4695, 4920, 4727)

# Reformat data
yields <- c(low, high)
concentrates <- c(rep("low", 8), rep("high", 8))
df <- data.frame(yield = yields, concentrate = concentrates)

# Plot data (including manually setting labels)
p <- ggplot(df, aes(x = yield)) +
  geom_histogram(binwidth = 500, fill = "gray", color = "black") +
  facet_grid(
    rows = vars(concentrate),
    labeller = as_labeller(c(
      "low" = "Milk yield in low group",
      "high" = "Milk yield in high group"
    ))
  ) +
  scale_x_continuous(name = "Yield") +
  scale_y_continuous(name = "Count")
p

# Compute test statistic
T_obs <- mean(high) - mean(low)

# Resample test statistic and compute p-values
N <- 10000
T_resamples <- replicate(N, {
  permuted_cows <- yields[sample(1:16)]
  group_1 <- permuted_cows[1:8]
  group_2 <- permuted_cows[9:16]
  mean(group_1) - mean(group_2)
})
p_value_onesided <- mean(T_resamples >= T_obs)
p_value_twosided <- mean(abs(T_resamples) >= abs(T_obs))

# Print results
cat("Difference in mean milk yield =", T_obs, "\n")
cat("One-sided p-value =", p_value_onesided, "\n")
cat("  alternative hypothesis: yield in high group is at least as large as in low group)\n")
cat("Two-sided p-value=", p_value_twosided, "\n")
cat("  alternative hypothesis: yield is different for the two groups)\n")

# Visualization of distribution of test statistic
test_df <- data.frame(T_resample = T_resamples)

p <- ggplot(test_df, aes(x = T_resample)) +
  geom_histogram(aes(y = after_stat(density)), fill = "gray", color = "black") +
  scale_x_continuous(name = "Difference of shuffled group averages") +
  scale_y_continuous(name = "Density") +
  ggtitle("Test statistic under null distribution") +
  geom_vline(xintercept = T_obs, color = "red", linetype = "dashed") +
  geom_vline(xintercept = -T_obs, color = "red", linetype = "dashed") +
  geom_text(
    x = T_obs + 150, y = 0.001, color = "red",
    label = paste0("T_obs = ", T_obs)
  ) +
  geom_text(
    x = -T_obs - 150, y = 0.001, color = "red",
    label = paste0("-T_obs = ", -T_obs)
  ) +
  geom_text(
    x = T_obs + 150, y = 0.001 / 2, color = "black",
    label = paste0("Probability = ", mean(T_resample >= T_obs))
  ) +
  geom_text(
    x = -T_obs - 150, y = 0.001 / 2, color = "black",
    label = paste0("Probability = ", mean(T_resample <= -T_obs))
  ) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("tmp.png", p, height = 5, width = 5)


# Built-in tests

wilcox.test(low, high)
t.test(low, high)
anova(lmPerm::lmp(yield ~ concentrate, data = df))
