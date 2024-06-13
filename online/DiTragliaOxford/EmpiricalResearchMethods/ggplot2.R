# Prep
library(gapminder)
library(tidyverse)

gapminder_2007 <- gapminder %>% 
  filter(year == 2007)

# Part 1
gapminder_2007 |> 
  ggplot(aes(x = gdpPercap, y = lifeExp)) + 
  geom_point()
# Part 2
gapminder_2007 |> 
  ggplot(aes(x = pop, y = gdpPercap)) + 
  geom_point()
# Part 1
gapminder_2007 |> 
  ggplot(aes(x = pop, y = lifeExp)) +
  geom_point() +
  scale_x_log10()
# Part 2
gapminder_2007 |> 
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  scale_y_log10()
# Part 3
gapminder_2007 |> 
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point() + 
  scale_x_log10() + 
  scale_y_log10()
gapminder |> 
  filter(year == 1952) |>
  ggplot(aes(x = pop, y = lifeExp, color = continent)) +
  geom_point() +
  scale_x_log10()
gapminder %>% 
  filter(year == 1977) |> 
  ggplot(aes(x = gdpPercap, y = lifeExp, size = pop)) +
  geom_point() +
  scale_x_log10() +
  facet_wrap(~ continent)
gapminder |> 
  group_by(year) |> 
  summarize(meanGDPc = mean(gdpPercap)) |> 
  ggplot(aes(x = year, y = meanGDPc)) +
  geom_point() 
gapminder |> 
  group_by(year, continent) |> 
  summarize(meanGDPc = mean(gdpPercap)) |>  
  ggplot(aes(x = year, y = meanGDPc, color = continent)) +
  geom_point() +
  scale_y_log10()
gapminder |> 
  group_by(year, continent) |> 
  summarize(meanGDPc = mean(gdpPercap)) |>  
  ggplot(aes(x = year, y = meanGDPc, color = continent)) +
  geom_point() +
  geom_line() +
  scale_y_log10()
gapminder |>  
  filter(year == 1977) |> 
  ggplot(aes(x = gdpPercap)) +
  geom_histogram(binwidth = 5000)
gapminder |>  
  filter(year == 1977) |> 
  ggplot(aes(x = gdpPercap)) +
  scale_x_log10() +
  geom_histogram(binwidth = 0.2)
gapminder |> 
  ggplot(aes(x = continent, y = gdpPercap)) +
  geom_boxplot() +
  facet_wrap(~ year) +
  scale_y_log10() +
  ggtitle('GDP per Capita by Continent: 1952-2007')
# Part 1
gapminder |> 
  ggplot(aes(x = continent, y = gdpPercap)) +
  geom_boxplot() +
  facet_wrap(~ year) +
  scale_y_log10() +
  coord_flip() +
  ggtitle('GDP per Capita by Continent: 1952-2007')
# Part 2
gapminder |>  
  group_by(year, continent) |> 
  summarize(meanGDPc = mean(gdpPercap)) |>
  ggplot(aes(x = continent, y = meanGDPc)) +
  geom_col() +
  facet_wrap(~ year) +
  coord_flip()
gapminder %>%
  filter(continent == 'Europe', year == 2007) %>%
  mutate(country = fct_reorder(country, gdpPercap)) %>%
  ggplot(aes(x = gdpPercap, y = country)) +
  geom_point()
