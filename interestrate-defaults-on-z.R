library(tidyverse)
library(ggplot2)

B <- 10000
n <- 1000
av_loan <- 250000
default_rate <- 0.02
loss_per_default <- -(av_loan / 3 * 2)

losses <- replicate(B, {
  defaults <- sample(c(0,1), n, prob = c(1-default_rate, default_rate), replace = TRUE)
  sum(defaults * loss_per_default)
})


expected_losses <- n * (default_rate * loss_per_default + (1 - default_rate) * 0)
stddev_losses <- sqrt(n) * abs(loss_per_default) * sqrt(default_rate * (1 - default_rate))

expected_losses
stddev_losses


data.frame(losses_in_millions = losses / 10^6) %>% ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth = 0.1, color = "blue", fill = "blue") + xlab("Default Loss in MEUR") + ylab("Occurrences")


z <- qnorm(0.01)

break_even_margin <- (-loss_per_default *
  (n * default_rate - z * sqrt(n * default_rate * (1 - default_rate))) / 
  ( n * (1 - default_rate) + z * sqrt(n * default_rate * (1 - default_rate)))) /
  av_loan

break_even_margin

av_earnings_per_loan <- (loss_per_default * default_rate) +
  ((break_even_margin * av_loan) * (1 - default_rate))

av_earnings_per_loan

contribution <- replicate(B, {
  contribution_per_sample <- sample(c(break_even_margin * av_loan, loss_per_default), n,
                            prob = c(1 - default_rate, default_rate), replace = TRUE)
  sum(contribution_per_sample)
})

mean(contribution)
mean(contribution < 0)


data.frame(contribution_in_millions = contribution / 10^6) %>%
  ggplot(aes(contribution_in_millions)) +
  geom_histogram(binwidth = 0.1, color = "green", fill = "green") +
  geom_vline(xintercept = 0, color = "black") +
  xlab("Margin Contribution in MEUR") +
  ylab("Occurrences")




contribution_new <- replicate(B, {
  default_rate_new <- default_rate + sample(seq(-0.02, 0.02, length = 100), 1)
  contribution_per_sample <- sample(c(break_even_margin * av_loan, loss_per_default), n,
                              prob = c(1 - default_rate_new, default_rate_new), replace = TRUE)
  sum(contribution_per_sample)
})

mean(contribution_new)
mean(contribution_new) - mean(contribution)
mean(contribution_new < 0)


data.frame(contribution_new_in_millions = contribution_new / 10^6) %>%
  ggplot(aes(contribution_new_in_millions)) +
  geom_histogram(binwidth = 0.1, color = "red", fill = "red") +
  geom_vline(xintercept = 0, color = "black") +
  xlab("Margin Contribution in MEUR") +
  ylab("Occurrences")

