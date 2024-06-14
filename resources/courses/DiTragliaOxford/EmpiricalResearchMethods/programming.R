# Exercise F
# The final statement in this function *stores* the result so it doesn't return
# anything. Either drop the assignment or add return()
skewness <- function(x) {
  mean(((x - mean(x)) / sd(x))^3)
}
my_var <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  sum((x - mean(x))^2) / (n - 1)
}

summary_stats <- function(x) {
  c('mean' = mean(x), 'sd' = sd(x))
}

# Exercise G
# This code fails: the condition inside of if() must evaluate to
# a *single* logical value, but this is a vector.
# The problem is the line break before else. This runs:
if(3 > 5) {
  print('3 is greater than 5') 
} else {
  print('3 is not greater than 5') 
}

mycov <- function(x, y) {
  if(!identical(length(x), length(y))) {
    return('Error: x and y must have the same length')
  }
  (x - mean(x)) * (y - mean(y))
}

myround <- function(x) {
  integer_part <- trunc(x)
  decimal_part <- x - integer_part 
  if(decimal_part <= 0.5) {
    out <- integer_part
  } else {
    out <- integer_part + 1
  }
  out
}
# Exercise H
fib <- function(n) {
  out <- vector(length = n)
  out[2] <- out[1] <- 1
  for(i in 3:n) {
    out[i] <- out[i - 1] + out[i - 2]
  }
  out
}
fib(12)
g <- function(x) {
  (x > 0) * (x^3 + x) + (x <= 0) * (x^2 - x)
}
f(-2:2)

# Exercise I
A <- matrix(rep(1:5, times = 5), 5, 5, TRUE)
A[-3, -2]
B <- rbind(diag(nrow = 4), diag(nrow = 4))
B[7, ]]
get_exchange <- function(n) {
  out <- matrix(0, n, n)
  for(i in 1:n) {
    out[i, n + 1 - i] <- 1
  }
  out
}

# Exercise J
get_exchange <- function(n) {
  out <- matrix(0, n, n)
  anti_diagonal <- cbind(1:n, n:1)
  out[anti_diagonal] <- 1
  out
}
get_exchange2 <- function(n) {
  diag(1, n)[n:1, ]
}
J3 <- get_exchange(3)
J3 * J3
J3 %*% J3
p_XY <- c(0.2, 0.8) %o% c(0.25, 0.5, 0.25)
rownames(p_XY) <- c('x=0', 'x=1')
colnames(p_XY) <- c('y=0', 'y=1', 'y=2')
colSums(p_XY)

# Exercise K
students <- data.frame('name' = c('Xerxes', 'Xanthippe', 'Xanadu'),
                       'age' = c(19, 23, 21),
                       'grade' = c(65, 70, 68),
                       'favorite_color' = c('blue', 'red', 'orange'))

# identical() returns a *scalar* but we need a vector
students[identical(students$name, 'Xerxes'), ] 
is_IT <- employees$department == 'IT'
employees[is_IT, ]
high_salary <- employees$salary >= 60000
employees[is_IT & high_salary, ]