getwd()
setwd("/Users/Nick/Desktop/R/groupProject")
getwd()

df <- read.table("trainData.txt", header = TRUE, sep = ",")

#Dataframe for training dataset
df["Hours"] <- format(strptime(df$Time,"%H:%M:%S"),'%H')
df["Hours"] <- as.numeric(df["Hours"][,])
df["Minutes"] <- format(strptime(df$Time,"%H:%M:%S"),'%M')
df["Minutes"] <- as.numeric(df["Minutes"][,])
df["Months"] <- format(as.Date(df$Date, format="%d/%m/%Y"), "%m")
df["Months"] <- as.numeric(df["Months"][,])
df["Days"] <- format(as.Date(df$Date, format="%d/%m/%Y"), "%d")
df["Days"] <- as.numeric(df["Days"][,])

dfMornings <- df[df$Hours == 7 | df$Hours == 8 | df$Hours == 9 | df$Hours == 10,]
dfNights <- df[df$Hours == 18 | df$Hours == 19 | df$Hours == 20 | df$Hours == 21,]

dfFallSpringMornings <- dfMornings[dfMornings$Months == 9 | dfMornings$Months == 10 | dfMornings$Months == 11 | dfMornings$Months == 3 | dfMornings$Months == 4 | dfMornings$Months == 5,]
dfFallSpringNights <- dfNights[dfNights$Months == 9 | dfNights$Months == 10 | dfNights$Months == 11 | dfNights$Months == 3 | dfNights$Months == 4 | dfNights$Months == 5,]

dfFallSpringMornings$Date <- as.POSIXlt(dfFallSpringMornings$Date, format = "%d/%m/%Y")
dfFallSpringMornings$Date <-weekdays(dfFallSpringMornings$Date)
dfFallSpringNights$Date <-as.POSIXlt(dfFallSpringNights$Date, format = "%d/%m/%Y")
dfFallSpringNights$Date <-weekdays(dfFallSpringNights$Date)
dfFallSpringFridayMornings <-dfFallSpringMornings[dfFallSpringMornings$Date == "Friday",]
dfFallSpringFridayNights <-dfFallSpringNights[dfFallSpringNights$Date == "Friday",]


nTrainTimes <- c(78)
i <- 1
while(i <= 78) {
  nTrainTimes[[i]] <- 240
  i <- i + 1
}
nrow(dfFallSpringFridayMornings)
nrow(dfFallSpringFridayNights)
sum(nTrainTimes)
# Design Hidden Markov Models
library("depmixS4")
set.seed(1)

#HMModel1
#spring fall friday mornings / test spring fall mornings
model_1 <- depmix(response = Global_active_power ~ 1, data = dfFallSpringFridayMornings, nstates = 20, ntimes = nTrainTimes)
fitted_model1 <- fit(model_1)
print(fitted_model1)


#HMMModel2
#spring fall friday nights / test spring fall nights
model_2 <- depmix(response = Global_active_power ~ 1, data = dfFallSpringFridayNights, nstates = 20, ntimes = nTrainTimes)
fitted_model2 <- fit(model_2)
print(fitted_model2)

#Data frame for test1 dataset
dfTest <- read.table("test3.txt", header = TRUE, sep = ",")

dfTest["Hours"] <- format(strptime(dfTest$Time,"%H:%M:%S"),'%H')
dfTest["Hours"] <- as.numeric(dfTest["Hours"][,])
dfTest["Minutes"] <- format(strptime(dfTest$Time,"%H:%M:%S"),'%M')
dfTest["Minutes"] <- as.numeric(dfTest["Minutes"][,])
dfTest["Months"] <- format(as.Date(dfTest$Date, format="%d/%m/%Y"), "%m")
dfTest["Months"] <- as.numeric(dfTest["Months"][,])
dfTest["Days"] <- format(as.Date(dfTest$Date, format="%d/%m/%Y"), "%d")
dfTest["Days"] <- as.numeric(dfTest["Days"][,])

dfTestMornings <- dfTest[dfTest$Hours == 7 | dfTest$Hours == 8 | dfTest$Hours == 9 | dfTest$Hours == 10,]
dfTestNights <- dfTest[dfTest$Hours == 18 | dfTest$Hours == 19 | dfTest$Hours == 20 | dfTest$Hours == 21,]

dfTestFallSpringMornings <- dfTestMornings[dfTestMornings$Months == 9 | dfTestMornings$Months == 10 | dfTestMornings$Months == 11 | dfTestMornings$Months == 3 | dfTestMornings$Months == 4 | dfTestMornings$Months == 5,]
dfTestFallSpringNights <- dfTestNights[dfTestNights$Months == 9 | dfTestNights$Months == 10 | dfTestNights$Months == 11 | dfTestNights$Months == 3 | dfTestNights$Months == 4 | dfTestNights$Months == 5,]

nTrainTimesMorning <- c(179)
i <- 1
while(i <= 179) {
  nTrainTimesMorning[[i]] <- 240
  i <- i + 1
}


test1Morning <- depmix(response = Global_active_power ~ 1, data = dfTestFallSpringMornings, nstates = 20, ntimes = nTrainTimesMorning)
test1Morning <- setpars(test1Morning,getpars(fitted_model1))



logLik(fitted_model1)
BIC(fitted_model1)
logLik(test1Morning)
BIC(test1Morning)
