# Download fake-panel-data.csv from https://ditraglia.com/data. This dataset was simulated according to the one-way error components model described above. It contains six columns: person is a unique person identifier (name), year is a year index (1-5), x and y are the regressor and outcome variable, and epsilon and eta are the error terms. (In real data you wouldn’t have the errors, but this is a simulation!)
# Use lm to regress y on x with “classical” standard errors. Repeat with standard errors clustered by person using lm_robust(). Discuss your results.
# Plot y against x along with the regression line from part 1.
# Repeat 2, but use a different color for the points that correspond to each person in the dataset and plot a separate regression line for each person.
# What does the plot you made in part 3 suggest? Use the columns epsilon and eta to check your conjecture.
# Finally, use lm_robust() to regress y on x and a dummy variable for each person, clustering the standard errors by person. Discuss your results.
library(tidyverse)
library(estimatr)
library(modelsummary)
fake_panel <- read_csv('https://ditraglia.com/data/fake-panel-data.csv')

reg_classical <- lm(y~ x, fake_panel)
reg_cluster <- lm_robust(y ~ x, fake_panel, clusters = person)

modelsummary(list(Classical = reg_classical, 
                  Clustered = reg_cluster), 
             gof_omit = 'AIC|BIC|F|RMSE|R2|Log.Lik.')

fake_panel |>
  ggplot(aes(x, y)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

fake_panel |>
  ggplot(aes(x, y, color = person)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

fake_panel |> 
  summarize(cor(x, eta))

reg_cluster_dummies <- lm_robust(y ~ x + person - 1, fake_panel, 
                                 clusters = person)

modelsummary(list(Classical = reg_classical, 
                  Clustered = reg_cluster,
                  `Clustered/Dummies` = reg_cluster_dummies), 
             gof_omit = 'AIC|BIC|F|RMSE|R2|Log.Lik.',
             coef_omit = 'person')

# Use dplyr to subtract the individual time averages from x and y in the simulated dataset from above. Then run OLS on the demeaned dataset with classical SEs.
# Compare the point estimates and standard errors from 1 to those from an OLS regression of y on x and a full set of person dummies, again with classical SEs.
# Consult ?lm_robust() to find out how to use the fixed_effects option. Use what you learn to regress y on x with person fixed effects, clustering by person.
# Compare your results from 3 to mine computed using feols() above and to your calculations with lm_robust() and clustered standard errors from Exercise A above.
library(fixest)

reg_demeaned <- fake_panel |> 
  group_by(person) |> 
  mutate(x = x - mean(x), y = y - mean(y)) |> 
  ungroup() |> 
  lm(y ~ x, data = _) 

reg_dummies <- lm(y ~ x + person - 1, fake_panel)

reg_robust_FE <- lm_robust(y ~ x, data = fake_panel, 
                           clusters = person, fixed_effects = ~ person) 

reg_robust_dummies <- lm_robust(y ~ x + person - 1, data = fake_panel, 
                                clusters = person) 

reg_FE <- feols(y ~ x | person, fake_panel)

modelsummary(list(Demeaned = reg_demeaned, 
                  `lm` = reg_dummies, 
                  `lm_robust/FE` = reg_robust_FE,
                  `lm_robust/dummies` = reg_robust_dummies,
                  `feols` = reg_FE),
             gof_omit = 'AIC|BIC|F|RMSE|R2|Log.Lik.',
             coef_omit = 'person')

# Install the wooldridge package and read the help file for wagepan.
# Run an OLS regression of lwage on educ, black, hisp, exper, exper squared, married, union, and year. Use classical standard errors.
# Repeat 2, but use plm() to estimate a random effects specification of the same model.
# Repeat 3, but use feols() to estimate a fixed-effects specification with clustered standard errors. Can you include the same variables as in parts 2 and 3? Explain.
# How do your estimates and standard errors of the effects of union membership vary across these three specifications? Discuss briefly.

library(wooldridge)
library(fixest)
library(plm)
library(modelsummary)

wagepan <- wagepan |> 
  mutate(year = factor(year))

ols_formula <- lwage ~ educ + black + hisp + exper + I(exper^2) + married + 
  union + year 

pooled_ols <- lm(ols_formula, wagepan)

random_effects <- plm(ols_formula, data = wagepan, 
                      index = c('nr', 'year'),
                      model = 'random')

# removed time-invariant regressors (married varies over time)
fe_formula <- lwage ~ exper + I(exper^2) + married + union + year | nr

# person id is `nr`
fixed_effects <- feols(fe_formula, wagepan) # Defaults to clustering by nf 

modelsummary(list(OLS = pooled_ols, RE = random_effects, FE = fixed_effects), 
             coef_omit = 'year|Intercept', 
             gof_omit = 'AIC|BIC|F|RMSE|R2|Log.Lik.')
