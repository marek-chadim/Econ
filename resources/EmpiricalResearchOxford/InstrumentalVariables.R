set.seed(1234)
n <- 5000
z <- rnorm(n)

library(mvtnorm)
Rho <- matrix(c(1, 0.5, 
                0.5, 1), 2, 2, byrow = TRUE)
errors <- rmvnorm(n, sigma = Rho)
colMeans(errors)
u <- errors[,1]
v <- errors[,2]
x <- 0.5 + 0.8 * z + v
y <- -0.3 + x + u

cov(x, y) / var(x)
cov(z, y) / var(z)

c(truth = 1,
  OLS = cov(x, y) / var(x), 
  IV = cov(z, y) / cov(z, x), 
  OLS_x_z = unname(coef(lm(y ~ x + z))[2]))

alpha <- -0.3 
beta <- 1
pi0 <- 0.5
pi1 <- 0.8
rho <- 0.5
beta_OLS <- beta + rho / (1 + pi1^2)
alpha_OLS <- alpha - rho * pi0 / (1 + pi1^2)
c(alpha = alpha, beta = beta, alpha_OLS = alpha_OLS, beta_OLS = beta_OLS)

coef(AER::ivreg(y ~ x | z))
coef(lm(y ~ x))

c(IV = mean((y - alpha - beta * x)^2), 
  OLS = mean((y - alpha_OLS - beta_OLS * x)^2))

set.seed(1234)
n <- 10000

R_zw <- matrix(c(1, 0.3, 0.3,
                 0.3, 1, 0.3,
                 0.3, 0.3, 1), 3, 3, byrow = TRUE)
zw <- rmvnorm(n, sigma = R_zw)
z1 <- zw[,1]
z2 <- zw[,2]
w <- zw[,3]

R_uv <- matrix(c(1, 0.5, 
                 0.5, 1), 2, 2, byrow = TRUE)
errors <- rmvnorm(n, sigma = R_uv)
u <- errors[,1]
v <- errors[,2]

x <- 0.5 + 0.2 * z1 - 0.15 * z2 + 0.25 * w + v 
y <- -0.3 + x - 0.7 * w + u

# TSLS "by hand"
first_stage <- lm(x ~ z1 + z2 + w) 
xhat <- fitted.values(first_stage)
second_stage <- lm(y ~ xhat + w)

# TSLS using AER::ivreg
tsls <- AER::ivreg(y ~ x + w | z1 + z2 + w)

library(broom)
library(tidyverse)

tidy(second_stage) |> 
  knitr::kable(digits = 2, caption = 'Second Stage')
tidy(tsls) |> 
  knitr::kable(digits = 2, caption = 'TSLS Results')

first_stage <- lm(x ~ z1 + z2)
xhat <- fitted.values(first_stage)
second_stage <- lm(y ~ xhat + w)
coef(second_stage)

AER::ivreg(y ~ x + w | z2 + w) |> 
  tidy() |> 
  knitr::kable(digits = 2)

AER::ivreg(y ~ x | z1 + z2) |> 
  tidy() |> 
  knitr::kable(digits = 2)

data_url <- 'https://ditraglia.com/data/Ginsburgh-van-Ours-2003.csv' 
qe <- read_csv(data_url)
qe <- qe |> 
  mutate(first = order == 1)

tidy_me <- function(results, mytitle) {
  # Helper function for making little tables in this solution 
  results |> 
    tidy() |> 
    select(term, estimate, std.error) |> 
    knitr::kable(digits = 2, caption = mytitle)
}

lm(ranking ~ first, qe) |>  
  tidy_me('First stage')

lm(critics ~ first, qe) |>  
  tidy_me('Reduced Form')

lm(scale(critics) ~ first, qe) |> 
  tidy_me('Reduced Form - Standardized Outcome') 

ols <- lm(critics ~ ranking, qe)
iv <- AER::ivreg(critics ~ ranking | first, data = qe) 

tidy_me(ols, 'OLS results')
tidy_me(iv, 'IV results')
ols_scaled <- lm(scale(critics) ~ ranking, qe) 
iv_scaled <- AER::ivreg(scale(critics) ~ ranking | first, data = qe) 

tidy_me(ols_scaled, 'OLS results - standardized outcome')
tidy_me(iv_scaled, 'IV results - standardized outcome')
