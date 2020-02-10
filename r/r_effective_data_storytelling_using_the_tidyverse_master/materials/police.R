library(fivethirtyeight)
library(tidyverse)

police_deaths_2015 <- police_deaths %>%
  filter(year == 2015)

police_killings %>% group_by(raceethnicity) %>% 
  summarize(unarmed = mean(armed == "No", na.rm = TRUE),
            count = n())

police_killings %>% mutate(gunshot = ifelse(cause == "Gunshot", TRUE, FALSE)) %>% 
  group_by(raceethnicity, gunshot) %>% 
  summarize(unarmed = mean(armed == "No", na.rm = TRUE),
            count = n()) %>% 
  na.omit()
