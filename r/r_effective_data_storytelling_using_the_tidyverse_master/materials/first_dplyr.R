library(fivethirtyeight)
library(dplyr)
library(ggplot2)

# What is wrong with this code?  Fix it
US_births_2013 <- US_births_2000_2014 %>% 
  filter(year = 2013)

# Explain %in%, |, !=, ==

# filter to include only the weekdays
US_births_weekdays_2013 <- US_births_2000_2014 %>% 
  filter(year == 2013) %>% 
  filter(!(day_of_week %in% c("Sat", "Sun")))

# Make plot based on filter
## Make an appropriate plot to analyze the trend in births across 2013
ggplot(data = US_births_weekdays_2013, 
       mapping = aes(x = date, y = births)) +
  geom_line(aes(color = day_of_week))


# summarize without group_by
## Calculate the mean and the median births by day_of_week
US_births_weekdays_2013 %>% 
  summarize(mean_births = mean(births),
    median_births = median(births))

# group_by / summarize
birth_day_summ <- US_births_weekdays_2013 %>% group_by(day_of_week) %>% 
  summarize(mean_births = mean(births),
    median_births = median(births))

# Same thing for all years and all days
#avg_by_day <- US_births_2000_2014 %>% group_by(day_of_week) %>% 
#  summarize(mean_births = mean(births),
#    median_births = median(births))
## Think about it How do they compare?

# Make plot after summarizing
# Use `geom_col`
ggplot(data = birth_day_summ, mapping = aes(x = day_of_week, y = median_births)) +
  geom_col()

# group_by multiple things and summarize
# Calculate proportion using mean(var == ) & summarize
flying %>% na.omit() %>% group_by(age, gender) %>% summarize(prop_very = mean(recline_rude == "Very"))

########################

# mutate

# Make plot after mutate