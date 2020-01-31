library(fivethirtyeight)
library(tidyverse)
library(forcats)
#year_bins <- paste0(seq(1970, 2010, ),"'s")
year_bins <- c("1970-'74", "1975-'79", "1980-'84", "1985-'89",
               "1990-'94", "1995-'99", "2000-'04", "2005-'09",
               "2010-'13")
bechdel2 <- bechdel %>%
  mutate(decade = cut(year, seq(1969, 2014, 5), labels = year_bins)) %>% 
  mutate(clean_test = factor(clean_test, 
                             levels = c("nowomen", "notalk", "men", "dubious", "ok")))
ggplot(data = bechdel2, mapping = aes(x = fct_rev(decade), fill = clean_test)) + 
  geom_bar(position = "fill", color = "white") + 
  coord_flip() +
  scale_fill_manual(values = c("#FF2603", "#FF937F", "#FEC9C3", "#6AB4DA", "#018ED5"))
# http://imagecolorpicker.com/en