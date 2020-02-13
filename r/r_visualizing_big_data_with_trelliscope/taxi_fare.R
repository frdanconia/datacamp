
# Visualizing big data with help of R and packaeg Trelliscope 
# The datasets being used in the course of this the bike data, Pokemon data and the taxi data
# Firstly the combined use of package Tidyverse and Trelliscope is needed. 
# Further the ggplot will also be used. In general, ggplot can't being ignored. Being one of
#  the very versatile package being used all the R community for visualization. 

# Getting the data available in the environment for the analysis. Then the further analysis 
# follows. 

load("/Users/deepajha/Documents/Big_Data_Trelliscope/tx_sub.Rdata")

# The packages dplyr and ggplot being used over here. 
library(dplyr)
library(ggplot2)

# Creating a summary for the rides

daily_summary <- tx %>%
  group_by(pick_day) %>%
  summarise(n_rides= n())
# Plotting the pickup_date versus number of rides

ggplot(aes(x = pick_day, y = n_rides), data = daily_summary) + geom_line()
###########

# Trying to find the total distribution with the histogram

ggplot(aes(total_amount),data = tx) + geom_histogram() + scale_x_log10()

## Bar chart distribution for the payment_type 

ggplot(data = tx, aes(payment_type)) + geom_bar()
+ scale_x_log10()

# Finding the relation between the trip duration and total fare 
# load the package hexbin in the workspace otherwise the command geom_hex doesn't work alone
library(hexbin)
ggplot(tx, aes(x = trip_duration, y = total_amount)) + geom_hex()
+ scale_x_log10() + scale_y_log10()


# Daily_counts and filtering the payment type for "cash" and "card" 

daily_counts <- tx %>%
  filter(payment_type %in% c("cash", "card")) %>%
  group_by(pick_day,pick_wkday,payment_type) %>%
  summarise(n_rides = n())
  






