library(tidyverse)
set.seed(92815)

gradebook <- tibble(
  student_id = c(192297, 291857, 500286, 449192, 372152, 627561), 
  name = c('Alice', 'Bob', 'Charlotte', 'Dante', 
           'Ethelburga', 'Felix'),
  quiz1 = round(rnorm(6, 65, 15)), quiz2 = round(rnorm(6, 88, 5)),
  quiz3 = round(rnorm(6, 75, 10)), midterm1 = round(rnorm(6, 75, 10)),
  midterm2 = round(rnorm(6, 80, 8)), final = round(rnorm(6, 78, 11)))

gradebook

emails <- tibble(
  student_id = c(101198, 192297, 372152, 918276, 291857), 
  email = c('unclejoe@whitehouse.gov', 'alice.liddell@chch.ox.ac.uk',
            'ethelburga@lyminge.org', 'mzuckerberg@gmail.com',
            'microsoftbob@hotmail.com'))
emails

quiz_scores <- gradebook |> 
  pivot_longer(starts_with('quiz'), 
               names_to = 'quiz', 
               names_prefix = 'quiz',
               names_transform = list(quiz = as.numeric),
               values_to = 'score') |>  
  select(student_id, name, quiz, score)

quiz_scores

# A
# Part 1
# The result contains students whose ids are in emails. Those with ids
# in gradebook who are *not* in gradebook are dropped.
right_join(gradebook, emails)
# Part 2
# The result contains everyone whose id appears in *either* dataset. This
# requires lots of padding out with missing values.
full_join(gradebook, emails)
# Part 3
# The result contains only those whose id appears in *both* datasets. Everyone
# else is dropped.
# Part 4
gradebook |>  
  left_join(emails)
# Part 5
emails$name <- c('Joe', 'Alice', 'Ethelburga', 'Mark', 'Bob')
left_join(gradebook, emails)

# B
# Part 1
quiz_scores |> 
  pivot_wider(names_from = quiz, values_from = score)
# Part 2
long_billboard <- billboard |> 
  pivot_longer(cols = starts_with('wk'), 
               names_to = 'week',  
               values_to = 'rank', 
               names_prefix = 'wk')
long_billboard
# Part 3
long_billboard |> 
  pivot_wider(names_from = week, 
              values_from = rank,
              names_prefix = 'wk')
# Part 4
read_csv('https://ditraglia.com/data/child_test_data.csv') |> 
  select(kid.score, mom.iq) |> 
  rename(`kid score` = kid.score, `mom iq` = mom.iq) |> 
  pivot_longer(c(`kid score`, `mom iq`),
               values_to = 'score', 
               names_to = 'test') |> 
  ggplot(aes(x = score, col = test)) +
  geom_density()
# Part 5
drop1_avg <- function(x){
  # Calculate the mean of x dropping the lowest value
  x <- sort(x)
  mean(x[-1]) 
}
gradebook |>  
  pivot_longer(starts_with('quiz'), names_to = 'quiz', values_to = 'score') |> 
  group_by(name) |> 
  mutate(quiz_avg = drop1_avg(score)) |> 
  pivot_wider(names_from = 'quiz', values_from = 'score')

# C
library(furrr)
library(mvtnorm)
library(tidyr) # for expand_grid

draw_sim_data <- function(n, r) {
  var_mat <- matrix(c(1, r,
                      r, 1), 2, 2, byrow = TRUE)
  rmvnorm(n, sigma = var_mat)
}

get_estimate <- function(dat) {
  stopifnot(ncol(dat) == 2)
  x <- dat[,1]
  y <- dat[,2]
  mean((x - mean(x)) * (y - mean(y)))
}

run_sim <- function(n, r, nreps = 5000) {
  map(1:nreps, \(i) draw_sim_data(n, r)) |> 
    map_dbl(get_estimate)
}

sim_params <- expand_grid(n = c(5, 10, 15, 20, 25),
                          r = c(-0.5, -0.25, 0, 0.25, 0.5))

plan(multisession, workers = 4)
my_options <- furrr_options(seed = 4321)
sim_results <- future_pmap(sim_params, run_sim, .options = my_options)

sim_bias <- sim_params |> 
  mutate(sim_mean = map_dbl(sim_results, mean),
         bias = sim_mean - r)

sim_bias |> 
  select(n, r, bias) |> 
  ggplot(aes(x = n, y = bias)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ r)