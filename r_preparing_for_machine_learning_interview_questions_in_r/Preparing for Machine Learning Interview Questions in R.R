library(data.table)
library(dplyr)

DM.result = 3

fifa_sample <- fread("fifa_sample.csv")
colnames(fifa_sample) <- c("SP","RA")

car_fuel_consumption <- fread("car_fuel_consumption.csv")
googleplaystore <- fread("googleplaystore.csv")


# Apply min-max and standardization: fifa_normalized
fifa_normalized <- fifa_sample %>% 
  mutate(SP_MinMax = (SP - min(SP)) / (max(SP) - min(SP)), 
         RA_MinMax = (RA - min(RA)) / (max(RA) - min(RA)),
         SP_ZScore = (SP - mean(SP)) / sd(SP),
         RA_ZScore = (RA - mean(RA)) / sd(RA)
  )

# Check for missing values using base R and naniar functions
any(is.na(bands))
any_na(bands)

# Visualize overall missingness
vis_miss(bands)

# Visualize overall missingness, clustered
vis_miss(bands, cluster = TRUE)

# Impute with the mean
imp_mean <- bands %>%
  bind_shadow(only_miss = TRUE) %>% 
  add_label_shadow() %>% 
  impute_mean_all()