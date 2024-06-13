dice_sum <- \() {
  # Roll a pair of fair, six-sided dice and return their sum
  die1 <- sample(1:6, 1)
  die2 <- sample(1:6, 1)
  die1 + die2
}
sims <- map_dbl(1:10000, \(i) dice_sum())

nreps <- 10000
sims <- rep(NA_real_, nreps)
for(i in seq_along(sims)) {
  sims[i] <- dice_sum()
}

get_avg_after_streak <- function(shots) {
  
  # shots should be a vector of 0 and 1; if not STOP!
  stopifnot(all(shots %in% c(0, 1)))  
  
  n <- length(shots)
  after_streak <- rep(NA, n) # Empty vector of length n
  
  # The first 3 elements of shots by definition cannot 
  # follow a streak
  after_streak[1:3] <- FALSE 
  
  # Loop over the remaining elements of shots
  for(i in 4:n) {
    # Extract the 3 shots that precede shot i
    prev_three_shots <- shots[(i - 3):(i - 1)]
    # Are all three of the preceding shots equal to 1? 
    # (TRUE/FALSE)
    after_streak[i] <- all(prev_three_shots == 1)
  }
  
  # shots[after_streak] extracts all elements of shots
  # for which after_streak is TRUE. Taking the mean of
  # these is the same as calculating the prop. of ones
  mean(shots[after_streak])
}

get_avg_after_streak(c(0, 1, 1, 1, 1, 0, 0, 0))

draw_shots <- function(n_shots, prob_success) {
  rbinom(n_shots, 1, prob_success)
}
set.seed(420508570)
mean(draw_shots(1e4, 0.5))

library(tidyverse)
nreps <- 1e4
sim_datasets <- map(1:nreps, \(i) draw_shots(100, 0.5))
sim_estimates <- map_dbl(sim_datasets, get_avg_after_streak)
mean_T <- mean(sim_estimates)
mean_T

mean_T + c(-1, 1) * 3 / (2 * nreps)

sim_params <- expand_grid(n_shots = c(50, 100, 200), 
                          prob_success = c(0.4, 0.5, 0.6))

run_sim <- \(n_shots, prob_success, nreps = 1e4) {
  map(1:nreps, \(i) draw_shots(n_shots, prob_success)) |> 
    map_dbl(get_avg_after_streak)
}

sim_results <- pmap(sim_params, run_sim)
# In very short sequences there may be no streaks, leading to an NaN
# Drop these, so we effectively condition on their being at least one
# streak in the sequence 
summary_stats <- map_dbl(sim_results, mean, na.rm = TRUE) 
summary_stats <- sim_params |> 
  bind_cols(mean_T = summary_stats) |> 
  mutate(bias = mean_T - prob_success) 
summary_stats |> 
  knitr::kable(digits = 2)

sim_params <- expand_grid(n_shots = c(50, 100, 200), 
                          prob_success = c(0.4, 0.5, 0.6))

run_sim <- \(n_shots, prob_success, nreps = 1e4) {
  map(1:nreps, \(i) draw_shots(n_shots, prob_success)) |> 
    map_dbl(get_avg_after_streak)
}

sim_results <- pmap(sim_params, run_sim)

# In very short sequences there may be no streaks, leading to an NaN
# Drop these, so we effectively condition on their being at least one
# streak in the sequence 
summary_stats <- map_dbl(sim_results, mean, na.rm = TRUE) 
summary_stats <- sim_params |> 
  bind_cols(mean_T = summary_stats) |> 
  mutate(bias = mean_T - prob_success) 
summary_stats |> 
  knitr::kable(digits = 2)

