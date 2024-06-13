library(tidyverse)

# Plot the power of a one-sided test as a function of mu 
n <- 100
s <- sqrt(25)
alpha <- 0.05
crit <- qnorm(1 - alpha)
tibble(mu = seq(0, 2, 0.01)) |> 
  mutate(kappa = sqrt(n) * mu / s,
         power = 1 - pnorm(crit - kappa)) |> 
  ggplot(aes(x = mu, y = power)) +
  geom_line()

# Plot the power of a one-sided test as a function of n 
mu_over_sigma <- 0.2
tibble(n = 1:500) |> 
  mutate(kappa = sqrt(n) * mu_over_sigma,
         power = 1 - pnorm(crit - kappa)) |> 
  ggplot(aes(x = n, y = power)) +
  geom_line()