library(tidyverse)
library(haven)

# Part 1
star <- read_csv('https://ditraglia.com/data/STAR.csv')

# Part 2
final5 <- read_dta('https://ditraglia.com/data/final5.dta', 
                   encoding = 'latin1')
set.seed(92815)
gradebook <- tibble(
  student_id = c(192297, 291857, 500286, 449192, 372152, 627561), 
  name = c('Alice', 'Bob', 'Charlotte', 'Dante', 
           'Ethelburga', 'Felix'),
  quiz1 = round(rnorm(6, 65, 15)),
  quiz2 = round(rnorm(6, 88, 5)),
  quiz3 = round(rnorm(6, 75, 10)),
  midterm1 = round(rnorm(6, 75, 10)),
  midterm2 = round(rnorm(6, 80, 8)), 
  final = round(rnorm(6, 78, 11)))


# Part 1
gradebook |> 
  select(ends_with('2'))
# Part 2
gradebook |> 
  select(contains('erm'))
# Part 3a
starwars |> 
  select(where(is.character))
# Part 3b
starwars |> 
  select(contains('_'))
# Part 3c
starwars |> 
  select(ends_with('color') | where(is.numeric))

# Part 1
gradebook |> 
  summarize(across(starts_with('quiz'), sd, .names = '{.col}_sd'))
# Part 2
starwars |> 
  summarize(across(where(is.character), n_distinct, .names = 'n_{.col}s'))
# Part 3
starwars |> 
  group_by(homeworld) |> 
  filter(n() > 1) |> 
  summarize(across(c(sex, species, eye_color), n_distinct))
starwars |> 
  group_by(homeworld) |> 
  filter(n() > 1) |> 
  summarize(across(c(sex, species, eye_color), n_distinct))
# Part 4
starwars |> 
  group_by(species) |> 
  filter(n() > 1) |> 
  summarize(across(where(is.numeric), \(x) median(x, na.rm = TRUE)))
starwars |> 
  filter(species == 'Kaminoan')
# Part 5
SD_IQR <- list(
  SD = \(x) sd(x, na.rm = TRUE),
  IQR = \(x) IQR(x, na.rm = TRUE)
)
starwars |> 
  summarize(across(where(is.numeric), SD_IQR, .names = '{.col}_{.fn}'))

star <- star |> 
  mutate(classtype = case_match(classtype,
                                1 ~ 'small',
                                2 ~ 'regular',
                                3 ~ 'regular+aid'),
         race = case_match(race,
                           1 ~ 'White', 
                           2 ~ 'Black',
                           3 ~ 'Asian',
                           4 ~ 'Hispanic',
                           5 ~ 'Native American',
                           6 ~ 'Other'),
         hsgrad = if_else(hsgrad == 1, 'graduate', 'non-graduate'))
