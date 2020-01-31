library(fivethirtyeight)
library(tidyverse)
library(ggplot2movies)

bechdel_movies <- bechdel %>%
  inner_join(y = movies, by = c("year", "title"))

bechdel_movies %>% group_by(mpaa) %>% 
  summarize(prop_pass = mean(binary == "PASS"),
            count = n())
