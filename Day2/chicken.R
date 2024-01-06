# -------------------------------------------------------------
# Applied Statistics / Statistical methods in the Biosciences
# Day 2
# r x k table: Chicken gait example
# Anton Rask Lundborg
# November 26, 2023
# -------------------------------------------------------------

# We insert the data in a matrix. The byrow-option does this row-wise.
# To enchance the interpretation we add names to the rows and the columns.
activity <- matrix(c(
  12, 26, 20, 12,
  13, 27, 22, 13,
  25, 25, 18, 8,
  28, 23, 21, 3
), 4, 4, byrow = TRUE)
rownames(activity) <- paste("Treatment", c("A", "B", "C", "D"))
colnames(activity) <- c("0", "1", "2", "3.5")

# For illustration we print the matrix to the console.
print(activity)

# We also construct both an association and mosaic plot:
assocplot(activity)
mosaicplot(activity)

# The chi-squared test is only borderline significant (p-value = 0.03909)
chisq.test(activity)

# The following code shows how the ordering of the response (gait score), and
# possibly also the explanatory variable (treatment), may be used to increase
# the statistical power.

# First we 'unfold' the data, such that each chicken is represented by a row in
# a data frame. The following tidyverse code does this. Although this code is
# advanced, you might still be able to decipher what it is doing by only
# executing part of the pipe-stream (where the pipes are the '%>%').
library(tidyverse)
activity_long <- activity %>%
  as_tibble(rownames = "treat") %>%
  pivot_longer(-"treat", names_to = "gait", values_to = "n") %>%
  uncount(n) %>%
  mutate(treat = factor(treat), gait = factor(gait))

# Let us have a look at these data, and check that they encode the same dataset.
activity_long
table(activity_long)

# We can now easily use ggplot to summarize the data differently:
activity_long %>% ggplot(aes(x = treat, fill = gait)) +
  geom_bar(position = position_fill()) +
  ylab("Proportion of chicken") + theme_minimal(base_size = 14)

# When data is in long format we may use kruskal.test() to use the ordering of
# the response and cor.test() to use the ordering of both variables. Before we
# do that we, however, should check that the levels of the factors have the same
# ordering as we want them to have.
levels(activity.long$gait)
levels(activity.long$treat)

# This was the case here. If not, then you may use the factor() function to
# reorder the levels.

# Using the ordering of the response we have:
kruskal.test(gait ~ treat, data = activity_long)

# Using the ordering of both the response and the explanatory variable we have:
with(activity_long,
     cor.test(as.numeric(gait), as.numeric(treat), method = "spearman"))

# In conclusion we did the following tests:
# chi-squared test:             p-value = 0.03909,     df=3*3=9
# Kruskal-Wallis:               p-value = 0.004578,    df=3*1=1
# Spearman rank correlation:    p-value = 0.0006596,   df=1*1=1
# Using the ordering of the variables increases the statistical power with 1
# order of magnitude!