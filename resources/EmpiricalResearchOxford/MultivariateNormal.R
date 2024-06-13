library(tidyverse)
kids <- read_csv('https://ditraglia.com/data/child_test_data.csv')
dat <- kids |> 
  select(mom.iq, kid.score)
colMeans(dat)
ggplot(kids) +
  geom_density(aes(x = mom.iq), fill = 'black', alpha = 0.5)
ggplot(kids) + 
  geom_density(aes(x = kid.score), fill = 'orange', alpha = 0.5)
ggplot(kids) +
  geom_density2d_filled(aes(x = mom.iq, y = kid.score)) 

A <- matrix(c(2, 1,
              1, 4), byrow = TRUE, ncol = 2, nrow = 2)
A %*% t(A)
set.seed(99999)
n <- 1e5
z1 <- rnorm(n)
z2 <- rnorm(n)
z <- cbind(z1, z2)
rm(z1, z2)
x <- cbind(x1 = 2 * z[, 1] +     z[, 2],
           x2 =     z[, 1] + 4 * z[, 2])
var(x)

s1 <- 1
s2 <- 2
r <- 0.4
s12 <- r * (s1 * s2)
A <- matrix(c(s1, 0,
              s12 / s1, sqrt(s2^2 - s12^2 / s1^2)), 
            byrow = TRUE, nrow = 2, ncol = 2)
x <- t(A %*% t(z)) 
colnames(x) <- c('x1', 'x2')
cov(x)
as_tibble(x) |> 
  ggplot(aes(x1, x2)) +
  geom_density2d_filled() +
  coord_fixed()  

A <- matrix(c(1, 2, 3,
              2, 2, 1,
              3, 1, 3), 
            byrow = TRUE, nrow = 3, ncol = 3) 
det(A)
B <- matrix(c(3, 2, 1,
              2, 3, 1,
              1, 1, 3), 
            byrow = TRUE, nrow = 3, ncol = 3)
det(B[1:2, 1:2])
R <- chol(B)
L <- t(R)
n_sims <- 1e5
set.seed(29837)
z <- matrix(rnorm(3 * n_sims), nrow = n_sims, ncol = 3)
x <- t(L %*% t(z))
cov(x)
#install.packages('mvtnorm')
library(mvtnorm)
set.seed(29837)
x_alt <- rmvnorm(n_sims, sigma = B)
cov(x_alt)
