library(fivethirtyeight)
library(tidyverse)

some_days <- US_births_2000_2014 %>%
  filter(date_of_month %in% c(6, 13, 20)) %>% 
  mutate(friday = ifelse(day_of_week == "Fri", TRUE, FALSE))

mean_days <- some_days %>% 
  group_by(date_of_month) %>% 
  summarize(mean_births = mean(births))

mean_fri_days <- some_days %>% 
  mutate(thirteen = ifelse(date_of_month == 13, TRUE, FALSE)) %>% 
  group_by(day_of_week, thirteen) %>% 
  summarize(mean_births = mean(births))

ggplot(mean_fri_days, aes(x = day_of_week, y = mean_births)) +
  geom_col() +
  facet_grid(thirteen~.)
