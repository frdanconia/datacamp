
# Visualization the Big data with help of ggplot and trelliscopejs package 
# Load the Trelliscope and ggplot and gapminder for the analysis 

# #######################
library(trelliscopejs)
library(ggplot2)
library(gapminder)
library(dplyr)

#life_expectancy <- gapminder$country

#life_expectancy_oceania <- gapminder %>%
#  group_by(continent = "Oceania")

# If alread used to ggplot facet then it wont be very different but it better to 
# have trelliscope. Keep on checking the github for the latest update. Till this 
# project it was in beta version. 

ggplot(gapminder, aes(x = year, y = lifeExp)) + geom_line() + 
  facet_trelliscope(~ country + continent, path = "/Users/deepajha/Documents/Big_Data_Trelliscope")

##################################
# qplot and ggplot gives the almost similar plots for the database. 

qplot(x = year, y = lifeExp,data = gapminder) + 
  facet_trelliscope(~ country + continent,nrow = 4,ncol = 2, path = "/Users/deepajha/Documents/Big_Data_Trelliscope")

# Can be used with the plots but specifying the path is very important which is
# generally not allowed from default through RStudio

# Very amazing plots can be given by facet trelliscope 
# similar can be done by ggplot facet but trelliscope preffered over it. 
# As the execution takes some time so better to restrict to the rows and columns 
# and don't use for big data application on the local machine. 




