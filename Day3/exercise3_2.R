# --------------------------------------------------------------------
# Applied Statistics / Statistical methods for SCIENCE
# Preparation for Exercise 3.2
# k x r table vs. ordinal regression: Chicken gait example from Day 2
# Anton Rask Lundborg
# December 3, 2023
# --------------------------------------------------------------------

# Load packages that will be used ----
library(ordinal)
library(tidyverse)

# Set theme
theme_set(theme_minimal())

# Insert data as a table of counts ----

# We insert the data in a matrix. The byrow-option allows
# us to do this row-wise. To enhance the interpretation
# we add names to the rows and the columns.
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


# Unfold table to long format ----

# In order to use the ordinal package later on we 'unfold'
# the data, such that each chicken is represented by a row
# in a tibble (data frame). The following tidyverse code does
# this. Although you cannot come up with this code without quite
# a bit of experience, you might be able to decipher what it is
# doing by only executing part of the pipe-stream (the pipes
# are the '%>%').
activity_long <- activity %>%
  as_tibble(rownames = "treat") %>%
  pivot_longer(-"treat", names_to = "gait", values_to = "n") %>%
  uncount(n) %>%
  mutate(treat = factor(treat), gait = factor(gait))

# Let us have a look at these data, and check that they indeed encode
# the dataset.
activity_long
table(activity_long)


# Graphical overivew using the long format ----
ggplot(activity_long, aes(x = treat, fill = gait)) +
  geom_bar(position = position_fill()) +
  ylab("Proportion of chicken")


# The 3 tests done on Day 2 ----

# The chi-squared test is only borderline significant (p-value = 0.03909)
chisq.test(activity)

# The following code shows how the ordering of the response
# (gait), and possibly also the explanatory variable (treat),
# may be used to increase the statistical power.

# Having data on the long format we may use kruskal.test()
# to invoke the ordering of the response, and cor.test() to
# invoke the ordering of both variables. Before we do that
# we, however, should check that the levels of the factors
# have the ordering that we want them to have.
levels(activity_long$gait)
levels(activity_long$treat)

# This was the case here. If not, then you may use the
# factor() function to reorder the levels.

# Using the ordering of the response we have:
kruskal.test(gait ~ treat, data = activity_long)

# Using the ordering of both the response and the explanatory variable we have:
cor.test(as.numeric(activity_long$gait), as.numeric(activity_long$treat),
  method = "spearman"
)

# In conclusion we did the following tests:
# Please note, that using the ordering of the variables
# increases the statistical power with 1 order of magnitude!
#
# chi-squared test:             p-value = 0.03909,     df=3*3=9
# Kruskal-Wallis:               p-value = 0.004578,    df=3*1=1
# Spearman rank correlation:    p-value = 0.0006596,   df=1*1=1


# Now it is time for you to work! ----

# 1. Fit a multinomial regression to 'gait' using 'treat' as
#    the explanatory variable.
#    Hint: See code for m0 on lecture slide 39, but without
#          the weights-option and the control-option.

# 2. Fit a proportional odds model on 'gait' using 'treat' as
#    the explanatory variable.
#    Hint: See code for m1 on lecture slide 39.

# 3. Do a lack-of-fit test for the proportional odds assumption.
#    Hint: See code on lecture slide 40

# 4. Test for effect of treatment on gait score, and compare
#    the p-value to the three tests done on Day 2.

# 5. Test if 'treat' can be used as a numerical explanatory
#    variable (with values A=1, B=2, C=3, D=4) in the
#    proportional odds model for 'gait'.
#    Hint: You can use 'as.numeric(treat)' to recode 'treat'
#          as a numerical variable.

# 6. Test for effect of treatment (as a numerical variable)
#    on gait score, and compare the p-value to the three tests
#    done on Day 2.

# 7. Return to the proportional odds model from question 2.
#    Use the functions pairs() and emmeans() from the emmeans-package
#    to do pairwise comparisons of the 4 treatments.
#    Consider the pros and cons of using 'treat' as a categorical
#    and a numerical explanatory variable.

activity_long <- activity %>%
  as_tibble(rownames = "treat") %>%
  pivot_longer(-"treat", names_to = "gait", values_to = "n") %>%
  uncount(n) %>%
  mutate(treat = factor(treat), gait = factor(gait))
