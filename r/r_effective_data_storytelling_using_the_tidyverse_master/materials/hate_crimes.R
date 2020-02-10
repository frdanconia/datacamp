library(tidyverse)
library(fivethirtyeight)
library(ggrepel)
library(plotly)
library(forcats)

state_lookup <- read_csv("datasets/division_region.csv")

hate_join <- inner_join(x = hate_crimes,
  y = state_lookup,
  by = "state")

hate_join %>% group_by(region, division) %>% 
  summarize(median_house_inc = median(median_house_inc))

library(hrbrthemes)
hate_join %>% group_by(region, division) %>% 
  summarize(median_hate_crimes = median(avg_hatecrimes_per_100k_fbi, na.rm = TRUE)) %>% 
  ggplot(aes(x = fct_reorder(division, median_hate_crimes), y = median_hate_crimes)) +
  geom_col(aes(fill = region)) +
  labs(y = "Median Hate Crimes per 100K", x = "Division of US") +
  coord_flip() + theme_ipsum()

hate_join %>% 
  ggplot(aes(x = gini_index, y = avg_hatecrimes_per_100k_fbi)) +
  geom_point(aes(color = region))

ggplot(data = hate_join, mapping = aes(x = share_vote_trump, y = avg_hatecrimes_per_100k_fbi)) +
  geom_point(aes(color = region)) +
  geom_label_repel(aes(label = state))

ggplot(data = hate_join %>% filter(state != "District of Columbia"), 
  mapping = aes(x = share_vote_trump, y = avg_hatecrimes_per_100k_fbi)) +
  geom_point(aes(color = region)) +
  geom_text_repel(aes(label = state))

ggplot(data = hate_join, mapping = aes(x = share_white_poverty, y = hate_crimes_per_100k_splc)) +
  geom_point(aes(size = share_vote_trump, color = region), alpha = 0.35) +
  geom_text(aes(label = state), alpha = 0) 

ggplotly(tooltip = "state")


###
library(maps)
states_map <- map_data("state")
ggplot(data = hate_join, mapping = aes(map_id = tolower(state))) +
  geom_map(aes(fill = region), map = states_map) +
  expand_limits(x = states_map$long, y = states_map$lat) +
  coord_map("stereographic") +
  theme_void()

ggplot(data = hate_join, mapping = aes(map_id = tolower(state))) +
  geom_map(aes(fill = division), map = states_map) +
  expand_limits(x = states_map$long, y = states_map$lat) +
  coord_map("stereographic") +
  theme_void()

ggplot(data = hate_join, mapping = aes(map_id = tolower(state))) +
  geom_map(aes(fill = share_vote_trump), map = states_map) +
  expand_limits(x = states_map$long, y = states_map$lat) +
  coord_map("stereographic") +
  theme_void()

ggplot(data = hate_join, mapping = aes(map_id = tolower(state))) +
  geom_map(aes(fill = gini_index), map = states_map) +
  expand_limits(x = states_map$long, y = states_map$lat) +
  coord_map("stereographic") +
  theme_void()

ggplot(data = hate_join, mapping = aes(map_id = tolower(state))) +
  geom_map(aes(fill = share_pop_hs), map = states_map) +
  expand_limits(x = states_map$long, y = states_map$lat) +
  coord_map("stereographic") +
  labs(title = "Share of adults 25 and older with a high-school diploma, 2009") +
  theme_void()

library(ggalt)
library(hrbrthemes)
ggplot(data = hate_join, mapping = aes(map_id = tolower(state))) +
  geom_map(aes(fill = share_pop_hs), map = states_map) +
  expand_limits(x = states_map$long, y = states_map$lat) +
  coord_map("stereographic") +
  labs(title = "Share of adults 25 and older with a high-school diploma, 2009") +
#  theme_void() +
  theme_ipsum()


