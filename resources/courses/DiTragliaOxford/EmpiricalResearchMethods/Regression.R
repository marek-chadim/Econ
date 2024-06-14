library(tidyverse)
library(janitor)
kids <- read_csv('https://ditraglia.com/data/child_test_data.csv')
kids <- clean_names(kids) 
kids <- kids |> 
  mutate(mom_education = if_else(mom_hs == 1, 'HS', 'NoHS')) |> 
  mutate(mom_education = fct_relevel(mom_education, 'NoHS'))

lm(kid_score ~ mom_iq, kids)

kids |> 
  mutate(mom_iq = mom_iq - mean(mom_iq)) |> 
  lm(kid_score ~ mom_iq, data = _)

lm(kid_score ~ I(mom_iq - mean(mom_iq)), kids)
mean(kids$kid_score)

myplot <- kids |> 
  ggplot(aes(x = mom_age, y = kid_score)) + 
  geom_point()

myplot + 
  geom_smooth(method = 'lm')
myplot + 
  geom_smooth()

reg <- lm(kid_score ~ mom_iq, kids)
tibble(u_hat = resid(reg)) |> 
  ggplot(aes(x = u_hat)) + 
  geom_histogram(binwidth = 5)

alpha_hat <- coef(reg)[1]
beta_hat <- coef(reg)[2]
kids_with_residuals <- kids |> 
  mutate(residuals = kid_score - alpha_hat - beta_hat * mom_iq) 

all.equal(kids_with_residuals$residuals, resid(reg), 
          check.attributes = FALSE)
kids_with_residuals |> 
  summarize(mean(residuals), cor(residuals, mom_iq))

# Part 1
library(broom)
reg_reverse <- lm(mom_iq ~ kid_score + mom_hs, kids)
reg_reverse |> 
  tidy() |> 
  knitr::kable(digits = 2)

reg_reverse |> 
  glance() |> 
  knitr::kable(digits = 2)

# Part 2
tidy(reg_reverse) |> 
  filter(term == 'kid_score') |> 
  select(estimate, std.error, statistic, p.value) |> 
  knitr::kable(digits = 2)

kids_augmented <- augment(reg_reverse, kids)
kids_augmented |>  
  ggplot(aes(x = .fitted, y = mom_iq)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  xlab('Fitted Values') +
  ylab('Mom IQ')

reg_y_vs_fitted <- lm(mom_iq ~ .fitted, kids_augmented)  
reg_y_vs_fitted |> 
  tidy() |> 
  knitr::kable()

c(glance(reg_reverse)$r.squared, glance(reg_y_vs_fitted)$r.squared)

# 3a
lm(kid_score ~ mom_iq + mom_education, kids) |> 
  tidy() |> 
  knitr::kable(digits = 2, col.names = c('', 'Estimate', 'SE', 't-stat', 'p-value'))

# 3b
lm(kid_score ~ mom_iq * mom_education, kids) |> 
  tidy() |> 
  knitr::kable(digits = 2, col.names = c('', 'Estimate', 'SE', 't-stat', 'p-value'))

kids |> 
  ggplot(aes(x = mom_iq, y = kid_score, color = mom_education)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) + 
  theme_minimal()

# Part 1
library(car)

reg_interact <- lm(kid_score ~ mom_iq * mom_education, kids)

reg_interact |> 
  tidy() |> 
  knitr::kable(digits = 2)

restrictions1 <- c('mom_educationHS = 0', 'mom_iq:mom_educationHS = 0')
linearHypothesis(reg_interact, restrictions1)
linearHypothesis(reg_interact, test = 'Chisq', restrictions1)

# Part 2
set.seed(1693)
reg_augmented <- kids |> 
  mutate(x = rnorm(nrow(kids)), z = mom_iq + rnorm(nrow(kids))) |> 
  lm(kid_score ~ mom_iq * mom_education + x + z, data = _)

reg_augmented |> 
  tidy() |> 
  knitr::kable(digits = 2)
restrictions2 <- c('x = 0', 'z = 0')
linearHypothesis(reg_augmented, restrictions2)
