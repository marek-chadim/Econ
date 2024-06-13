#In this question you’ll partially replicate a well-known paper on racial bias in the labor market: “Are Emily and Greg More Employable Than Lakisha and Jamal? A Field Experiment on Labor Market Discrimination” by Marianne Bertrand and Sendhil Mullainathan. The paper, which I’ll refer to as BM for short, appears in Volume 94, Issue #4 of the American Economic Review. You will need to consult this paper to complete this problem.
library(tidyverse)
bm <- read_csv('https://ditraglia.com/data/lakisha_aer.csv')

bm <- bm |> 
  mutate(female = (sex == 'f'), 
         black = (race == 'b')) 
bm |>  
  group_by(black) |> 
  summarize(n_female = sum(female))

bm |>  
  group_by(black) |> 
  summarize(avg_computerskills = mean(computerskills))

bm |> 
  group_by(black) |> 
  summarize(avg_numjobs = mean(ofjobs), avg_educ = mean(education))

bm |>  
  group_by(black) |> 
  summarize(avg_exp = mean(yearsexp), sd_exp = sd(yearsexp))

bm |>  
  group_by(female) |> 
  summarize(avg_computerskills = mean(computerskills),
            avg_educ = mean(education))

bm |>  
  summarize(avg_callback = mean(call))

bm |>  
  group_by(black) |> 
  summarize(avg_callback = mean(call))

bm |>  
  group_by(female, black) |> 
  summarize(avg_callback = mean(call))

call_black <- bm |> 
  filter(race == 'b') |> 
  pull(call)
call_white <- bm |> 
  filter(race == 'w') |> 
  pull(call)
t.test(call_black, call_white)