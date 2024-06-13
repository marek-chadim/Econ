install.packages('dplyr')
install.packages('gapminder')
library(dplyr)
library(gapminder)
gapminder |> 
  filter(year == 2002)
gapminder |> 
  filter(year == 2005)
gapminder_asia <- gapminder |>  
  filter(continent == 'Asia')
gapminder_asia
gapminder |>  
  filter(year == 1977, country %in% c('Ireland', 'Brazil'))
gapminder |>  
  arrange(lifeExp)
gapminder |>  
  arrange(desc(lifeExp))
# Part 1
gapminder |>  
  select(year, lifeExp, country)
# Part 2
gapminder |>  
  select(-year, -lifeExp, -country)
# Part 1
gapminder |> 
  filter(year == 1977) |> 
  summarize(median(lifeExp))
# Part 2
gapminder |> 
  filter(year == 1977, continent == 'Asia') |> 
  summarize(median(lifeExp))
gapminder |> 
  group_by(continent) |> 
  summarize(median(lifeExp))
gapminder |>
  group_by(year) |> 
  summarize(meanLifeExp = mean(lifeExp)) 
gapminder |> 
  mutate(lifeExpMonths = 12 * lifeExp)
# Part 1
c(4, 1, 5, NA, 3) |> 
  var(na.rm = TRUE)
# Part 2
TRUE |> 
  var(c(4, 1, 5, NA, 3), na.rm = _)
# Part 3
arrange(gapminder, desc(lifeExp))
gapminder |> 
  filter(year > 1997) |> 
  group_by(continent, year) |> 
  summarize(mean_GDPc = mean(gdpPercap), sd_GDPc = sd(gdpPercap)) |> 
  arrange(sd_GDPc)
