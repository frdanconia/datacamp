library(fivethirtyeight)
library(readr)
library(dplyr)
library(tidyr)
data(police_locals)
ideology <- read_csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_3085/datasets/ideology.csv")
police_join <- inner_join(x = police_locals, y = ideology, by = "city")
cost_of_living <- read_csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_3085/datasets/cost_of_living.csv")
police_join_cost <- inner_join(x = police_join, y = cost_of_living, by = "state")
police_tidy <- police_join_cost %>% 
  gather(key = "race", value = "reside_perc", all:asian) %>% 
  filter(race %in% c("white", "non_white")) %>% 
  mutate(col_group = factor(col_group, levels = c("high", "mid", "low")))
