library(fivethirtyeight)
library(tidyverse)
levels(flying$height)

flying2 <- na.omit(flying) %>%
  mutate(over_6feet = ifelse(height %in% c("6'1\"", "6'2\"","6'3\"","6'4\"", "6'5\"","6'6\" and above"),
                        TRUE,
                        FALSE)) %>% 
  mutate(shorter = ifelse(height %in% c("Under 5 ft.", "5'0\"", "5'1\"", "5'2\"", "5'3\"", "5'4\""),
                          TRUE,
                          FALSE)) %>% 
  mutate(height_group = ifelse(over_6feet, "taller", 
         ifelse(shorter, "shorter",
         "middle"))) %>% 
  mutate(height_group = factor(height_group, levels = c("shorter", "middle", "taller")))

ggplot(data = flying2, aes(x = recline_rude, fill = over_6feet)) +
  geom_bar(position = "fill") +
  facet_wrap(~age)

ggplot(data = flying2, aes(x = over_6feet, fill = recline_rude)) +
  geom_bar(position = "fill", color = "white")

ggplot(data = flying2, aes(x = recline_frequency, fill = over_6feet)) +
  geom_bar(position = "fill", color = "white") +
  coord_flip()

ggplot(data = flying2, aes(x = shorter, fill = recline_rude)) +
  geom_bar(position = "fill", color = "white")

ggplot(data = flying2, aes(x = over_6feet, fill = recline_frequency)) +
  geom_bar(position = "fill", color = "white")

ggplot(data = flying2, aes(x = shorter, fill = recline_frequency)) +
  geom_bar(position = "fill", color = "white")

ggplot(data = flying2, aes(x = recline_frequency, fill = shorter)) +
  geom_bar(position = "fill", color = "white") +
  coord_flip()

ggplot(data = flying2, aes(x = height_group, fill = recline_frequency)) +
  geom_bar(position = "fill", color = "white")

ggplot(data = flying2, aes(x = height_group, fill = recline_rude)) +
  geom_bar(position = "fill", color = "white")
