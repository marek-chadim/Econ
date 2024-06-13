library(tidyverse)

myplogis <- function(z) {
  1 / (1 + exp(-z))
}

z_seq <- seq(from = -4, to = 4, length.out = 400)

all.equal(myplogis(z_seq), plogis(z_seq))

plot_me <- tibble(z = z_seq, p = myplogis(z_seq))

ggplot(plot_me, aes(x = z, y = p)) +
  geom_line()

x_seq <- seq(-4, 4, length.out = 400)
tibble(x = x_seq, y = dlogis(x_seq)) |> 
  ggplot(aes(x, y)) + 
  geom_line()
dlogis(0)

# These lines are identical to those from above so we get the same x-values:
set.seed(1234)
n <- 500
alpha <- 0.5
beta <- 1
x <- rnorm(n, mean = 1.5, sd = 2)
# Here's the only thing that changes:
y <- rbinom(n, size = 1, prob = plogis(alpha + beta * x))
mydat2 <- tibble(x, y)

# Setup code pasted from lecture slides
set.seed(1234)
n <- 500
x <- rnorm(n, mean = 1.5, sd = 2) # Generate X
ystar <- 0.5 + 1 * x + rlogis(n) # Generate "latent variable"
y <- 1 * (ystar > 0) # Transform to 0/1
mydat <- tibble(x, y)
lreg <- glm(y ~ x, family = binomial(link = 'logit'), mydat)
summary(lreg)

# They work as expected!
library(broom)
library(modelsummary)

tidy(lreg)
glance(lreg)
modelsummary(lreg)

# Divide by 4 rule
alpha_hat <- coef(lreg)[1]
beta_hat <- coef(lreg)[2]
beta_hat / 4
# Partial effect at average x
beta_hat * dlogis(alpha_hat + beta_hat * mean(x))
# Average partial effect
beta_hat * mean(dlogis(alpha_hat + beta_hat * x))


# two truths and a lie experiment
data_url <- 'https://ditraglia.com/data/two-truths-and-a-lie-2022-cleaned.csv'
two_truths <- read_csv(data_url)
two_truths_reg <- glm(guessed_right ~ certainty, 
                      family = binomial(link = 'logit'),
                      data = two_truths)
library(modelsummary)
modelsummary(two_truths_reg)
two_truths |> 
  ggplot(aes(x = certainty, y = guessed_right)) +
  stat_smooth(method = 'glm', method.args = list(family = 'binomial'),
              formula = y ~ x) +
  geom_jitter(width = 0.1, height = 0.05)