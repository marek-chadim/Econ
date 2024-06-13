#In this exercise, you will carry out a simple growth accounting exercise to calculate total factor productivity (TFP) for a number of countries, using data from the Penn World Table. (Who said there was no macroeconomics in core ERM?) Before you begin, read this extract from Aghion & Howitt (2009).
# Summarize the approach to calculating TFP described in Aghion & Howitt (2009).
# Install and load the pwt10 R package and read the help file ?pwt10.0. Provide a brief summary of this dataset, including an explanation of the variables country, year, isocode, rgdpna, rkna, and emp.
# Extract the variables listed in the preceding part from pwt10.0 for the countries in Table 5.1 of Aghion & Howitt. Include all observations from 1959 onwards. Using this information, calculate output per worker and capital per worker. Append them to your dataset.
# Consult ?scale_y_continuous() and then make a line plot of output per worker over time on the natural log scale for each country in your dataset.
# For a series that doesn’t grow too fast, the difference of natural logs of successive values provides a good approximation of the geometric growth rate. (This gives a percentage expressed as a decimal.) Consult ?lag() from dplyr. Using what you learn, compute the log growth rate of capital per worker and output per worker and append them to your dataset.
# Use Aghion and Howitt’s assumed value for the capital share 
# to compute the “Solow residual” in each country and year and append it to your dataset.
# Based on your calculations from above, attempt to replicate the first two columns of Table 5.1 from Aghion and Howitt. Make sure to use the same time periods as they do. Your results will probably differ somewhat from Aghion and Howitt’s, but they should generally be in the ballpark.
# Repeat the preceding, but use data from after 2000. Broadly speaking, how do the TFP figures compare over time?

library(tidyverse)
library(pwt10)

countries <- c('Australia', 'Austria', 'Belgium', 'Canada', 'Denmark',
               'Finland', 'France', 'Germany', 'Greece', 'Iceland',
               'Ireland', 'Italy', 'Japan', 'Netherlands', 'New Zealand',
               'Norway', 'Portugal', 'Spain', 'Sweden', 'Switzerland',
               'United Kingdom', 'United States of America')  

dat <- pwt10.0 |> 
  filter(country %in% countries, year >= 1959) |> 
  select(country, year, isocode, rgdpna, rkna, emp) |> 
  mutate(Y_pc = rgdpna / emp, K_pc = rkna / emp)

dat |> 
  ggplot(aes(x = year, y = Y_pc)) +
  geom_line() +
  facet_wrap(~ country) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_y_continuous(trans = 'log') +
  labs(y = 'Ouput / worker (log scale)', x = 'Year')

dat <- dat |> 
  arrange(year) |> 
  group_by(country) |> 
  mutate(g_K_pc = log(K_pc) - log(lag(K_pc)),
         g_Y_pc = log(Y_pc) - log(lag(Y_pc)),
         solow_residual = g_Y_pc - 0.3 * g_K_pc) # Assuming alpha = 0.3

dat |> 
  filter(year >= 1960, year <= 2000) |> 
  select(country, g_Y_pc, g_K_pc, solow_residual) |> 
  group_by(country) |> 
  summarize(`Growth Rate` = 100 * mean(g_Y_pc),
            `TFP Growth` = 100 * mean(solow_residual)) |> 
  arrange(as.character(country)) |> 
  knitr::kable(digits = 2, caption = '1960-2000')

dat |> 
  filter(year >= 2001) |> 
  select(country, g_Y_pc, g_K_pc, solow_residual) |> 
  group_by(country) |> 
  summarize(`Growth Rate` = 100 * mean(g_Y_pc),
            `TFP Growth` = 100 * mean(solow_residual)) |> 
  arrange(as.character(country)) |> 
  knitr::kable(digits = 2, caption = '2001-2019')
