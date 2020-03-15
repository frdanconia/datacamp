getwd()
#setwd("/Users/kelvin/Documents/R_codes/Project/")

# Loading Dataset
df = read.table("TrainData.txt", header = TRUE, sep = ",")
#df["timeSeries"] <- as.POSIXct(paste(df$Date, df$Time), format="%d/%m/%Y %H:%M:%S")
## Extract and Format Date
df["Weekdays"] <- weekdays(as.Date(df["Date"][,], format = "%d/%m/%Y"))
df["Days"] <- format(as.Date(df$Date, format="%d/%m/%Y"),"%d")
df["Days"] <- as.numeric(df["Days"][,])
df["Months"] <- format(as.Date(df$Date, format="%d/%m/%Y"),"%m")
df["Months"] <- as.numeric(df["Months"][,])
df["Years"] <- format(as.Date(df$Date, format="%d/%m/%Y"),"%Y")
df["Years"] <- as.numeric(df["Years"][,])
## Extract and Format Time
df["Hours"] <- format(strptime(df$Time,"%H:%M:%S"),'%H')
df["Hours"] <- as.numeric(df["Hours"][,])
df["Minutes"] <- format(strptime(df$Time,"%H:%M:%S"),'%M')
df["Minutes"] <- as.numeric(df["Minutes"][,])

# Anomaly Detection Approach 1
marchMorning <- df[df["Months"] == 3 & (df["Hours"] == 7 | df["Hours"] == 8 | df["Hours"] == 9 | df["Hours"] == 10),]
marchMorningAvg <- c()
i <- 1
year <- 2007
while (year <= 2009) {
  day <- 1
  while (day <= 31) {
    df_temp <- marchMorning[marchMorning["Days"] == day & marchMorning["Years"] == year,]
    marchMorningAvg[[i]] <- mean(df_temp["Global_active_power"][,], na.rm = TRUE)
    i <- i + 1
    day <- day + 1
  }
  year <- year + 1
}

aprilMorning <- df[df["Months"] == 4 & (df["Hours"] == 7 | df["Hours"] == 8 | df["Hours"] == 9 | df["Hours"] == 10),]
aprilMorningAvg <- c()
i <- 1
year <- 2007
while (year <= 2009) {
  day <- 1
  while (day <= 30) {
    df_temp <- aprilMorning[aprilMorning["Days"] == day & aprilMorning["Years"] == year,]
    aprilMorningAvg[[i]] <- mean(df_temp["Global_active_power"][,], na.rm = TRUE)
    i <- i + 1
    day <- day + 1
  }
  year <- year + 1
}

mayMorning <- df[df["Months"] == 5 & (df["Hours"] == 7 | df["Hours"] == 8 | df["Hours"] == 9 | df["Hours"] == 10),]
mayMorningAvg <- c()
i <- 1
year <- 2007
while (year <= 2009) {
  day <- 1
  while (day <= 31) {
    df_temp <- mayMorning[mayMorning["Days"] == day & mayMorning["Years"] == year,]
    mayMorningAvg[[i]] <- mean(df_temp["Global_active_power"][,], na.rm = TRUE)
    i <- i + 1
    day <- day + 1
  }
  year <- year + 1
}

springMorningAvg <- c(marchMorningAvg, aprilMorningAvg, mayMorningAvg)
springMorningMax <- max(springMorningAvg)
springMorningMin <- min(springMorningAvg)

marchNight <- df[df["Months"] == 3 & (df["Hours"] == 18 | df["Hours"] == 19 | df["Hours"] == 20 | df["Hours"] == 21),]
marchNightAvg <- c()
i <- 1
year <- 2007
while (year <= 2009) {
  day <- 1
  while (day <= 31) {
    df_temp <- marchNight[marchNight["Days"] == day & marchNight["Years"] == year,]
    marchNightAvg[[i]] <- mean(df_temp["Global_active_power"][,], na.rm = TRUE)
    i <- i + 1
    day <- day + 1
  }
  year <- year + 1
}

aprilNight <- df[df["Months"] == 4 & (df["Hours"] == 18 | df["Hours"] == 19 | df["Hours"] == 20 | df["Hours"] == 21),]
aprilNightAvg <- c()
i <- 1
year <- 2007
while (year <= 2009) {
  day <- 1
  while (day <= 30) {
    df_temp <- aprilNight[aprilNight["Days"] == day & aprilNight["Years"] == year,]
    aprilNightAvg[[i]] <- mean(df_temp["Global_active_power"][,], na.rm = TRUE)
    i <- i + 1
    day <- day + 1
  }
  year <- year + 1
}

mayNight <- df[df["Months"] == 5 & (df["Hours"] == 18 | df["Hours"] == 19 | df["Hours"] == 20 | df["Hours"] == 21),]
mayNightAvg <- c()
i <- 1
year <- 2007
while (year <= 2009) {
  day <- 1
  while (day <= 31) {
    df_temp <- mayNight[mayNight["Days"] == day & mayNight["Years"] == year,]
    mayNightAvg[[i]] <- mean(df_temp["Global_active_power"][,], na.rm = TRUE)
    i <- i + 1
    day <- day + 1
  }
  year <- year + 1
}

springNightAvg <- c(marchNightAvg, aprilNightAvg, mayNightAvg)
springNightMax <- max(springNightAvg)
springNightMin <- min(springNightAvg)

sepMorning <- df[df["Months"] == 9 & (df["Hours"] == 7 | df["Hours"] == 8 | df["Hours"] == 9 | df["Hours"] == 10),]
sepMorningAvg <- c()
i <- 1
year <- 2007
while (year <= 2009) {
  day <- 1
  while (day <= 30) {
    df_temp <- sepMorning[sepMorning["Days"] == day & sepMorning["Years"] == year,]
    sepMorningAvg[[i]] <- mean(df_temp["Global_active_power"][,], na.rm = TRUE)
    i <- i + 1
    day <- day + 1
  }
  year <- year + 1
}

octMorning <- df[df["Months"] == 10 & (df["Hours"] == 7 | df["Hours"] == 8 | df["Hours"] == 9 | df["Hours"] == 10),]
octMorningAvg <- c()
i <- 1
year <- 2007
while (year <= 2009) {
  day <- 1
  while (day <= 31) {
    df_temp <- octMorning[octMorning["Days"] == day & octMorning["Years"] == year,]
    octMorningAvg[[i]] <- mean(df_temp["Global_active_power"][,], na.rm = TRUE)
    i <- i + 1
    day <- day + 1
  }
  year <- year + 1
}

novMorning <- df[df["Months"] == 11 & (df["Hours"] == 7 | df["Hours"] == 8 | df["Hours"] == 9 | df["Hours"] == 10),]
novMorningAvg <- c()
i <- 1
year <- 2007
while (year <= 2009) {
  day <- 1
  while (day <= 30) {
    df_temp <- novMorning[novMorning["Days"] == day & novMorning["Years"] == year,]
    novMorningAvg[[i]] <- mean(df_temp["Global_active_power"][,], na.rm = TRUE)
    i <- i + 1
    day <- day + 1
  }
  year <- year + 1
}

fallMorningAvg <- c(sepMorningAvg, octMorningAvg, novMorningAvg)
fallMorningMax <- max(fallMorningAvg)
fallMorningMin <- min(fallMorningAvg)

sepNight <- df[df["Months"] == 9 & (df["Hours"] == 18 | df["Hours"] == 19 | df["Hours"] == 20 | df["Hours"] == 21),]
sepNightAvg <- c()
i <- 1
year <- 2007
while (year <= 2009) {
  day <- 1
  while (day <= 30) {
    df_temp <- sepNight[sepNight["Days"] == day & sepNight["Years"] == year,]
    sepNightAvg[[i]] <- mean(df_temp["Global_active_power"][,], na.rm = TRUE)
    i <- i + 1
    day <- day + 1
  }
  year <- year + 1
}

octNight <- df[df["Months"] == 10 & (df["Hours"] == 18 | df["Hours"] == 19 | df["Hours"] == 20 | df["Hours"] == 21),]
octNightAvg <- c()
i <- 1
year <- 2007
while (year <= 2009) {
  day <- 1
  while (day <= 31) {
    df_temp <- octNight[octNight["Days"] == day & octNight["Years"] == year,]
    octNightAvg[[i]] <- mean(df_temp["Global_active_power"][,], na.rm = TRUE)
    i <- i + 1
    day <- day + 1
  }
  year <- year + 1
}

novNight <- df[df["Months"] == 11 & (df["Hours"] == 18 | df["Hours"] == 19 | df["Hours"] == 20 | df["Hours"] == 21),]
novNightAvg <- c()
i <- 1
year <- 2007
while (year <= 2009) {
  day <- 1
  while (day <= 30) {
    df_temp <- novNight[novNight["Days"] == day & novNight["Years"] == year,]
    novNightAvg[[i]] <- mean(df_temp["Global_active_power"][,], na.rm = TRUE)
    i <- i + 1
    day <- day + 1
  }
  year <- year + 1
}

fallNightAvg <- c(sepNightAvg, octNightAvg, novNightAvg)
fallNightMax <- max(fallNightAvg)
fallNightMin <- min(fallNightAvg)

test = read.table("test5.txt", header = TRUE, sep = ",")
test["Weekdays"] <- weekdays(as.Date(test["Date"][,], format = "%d/%m/%Y"))
test["Days"] <- format(as.Date(test$Date, format="%d/%m/%Y"),"%d")
test["Days"] <- as.numeric(test["Days"][,])
test["Months"] <- format(as.Date(test$Date, format="%d/%m/%Y"),"%m")
test["Months"] <- as.numeric(test["Months"][,])
test["Years"] <- format(as.Date(test$Date, format="%d/%m/%Y"),"%Y")
test["Years"] <- as.numeric(test["Years"][,])
test["Hours"] <- format(strptime(test$Time,"%H:%M:%S"),'%H')
test["Hours"] <- as.numeric(test["Hours"][,])
test["Minutes"] <- format(strptime(test$Time,"%H:%M:%S"),'%M')
test["Minutes"] <- as.numeric(test["Minutes"][,])

marchMorning <- test[test["Months"] == 3 & (test["Hours"] == 7 | test["Hours"] == 8 | test["Hours"] == 9 | test["Hours"] == 10),]
marchMorningAvg <- c()
day <- 1
while (day <= 31) {
  df_temp <- marchMorning[marchMorning["Days"] == day,]
  marchMorningAvg[[day]] <- mean(df_temp["Global_active_power"][,], na.rm = TRUE)
  day <- day + 1
}
# View(marchMorningAvg)
plot(1:31, marchMorningAvg, type = "l", main = "Test 5 March Mornings", xlab = "Days", ylab = "Global_active_power", col = "black", ylim = c(0.1, 10))
abline(h = springMorningMax, col = "red")
abline(h = springMorningMin, col = "red")

marchNight <- test[test["Months"] == 3 & (test["Hours"] == 18 | test["Hours"] == 19 | test["Hours"] == 20 | test["Hours"] == 21),]
marchNightAvg <- c()
day <- 1
while (day <= 31) {
  df_temp <- marchNight[marchNight["Days"] == day,]
  marchNightAvg[[day]] <- mean(df_temp["Global_active_power"][,], na.rm = TRUE)
  day <- day + 1
}
# View(marchNightAvg)
plot(1:31, marchNightAvg, type = "l", main = "Test 5 March Nights", xlab = "Days", ylab = "Global_active_power", col = "black", ylim = c(0.1, 9))
abline(h = springNightMax, col = "red")
abline(h = springNightMin, col = "red")

aprilMorning <- test[test["Months"] == 4 & (test["Hours"] == 7 | test["Hours"] == 8 | test["Hours"] == 9 | test["Hours"] == 10),]
aprilMorningAvg <- c()
day <- 1
while (day <= 30) {
  df_temp <- aprilMorning[aprilMorning["Days"] == day,]
  aprilMorningAvg[[day]] <- mean(df_temp["Global_active_power"][,], na.rm = TRUE)
  day <- day + 1
}
# View(aprilMorningAvg)
plot(1:30, aprilMorningAvg, type = "l", main = "Test 5 April Mornings", xlab = "Days", ylab = "Global_active_power", col = "black", ylim = c(0.1, 8))
abline(h = springMorningMax, col = "red")
abline(h = springMorningMin, col = "red")

aprilNight <- test[test["Months"] == 4 & (test["Hours"] == 18 | test["Hours"] == 19 | test["Hours"] == 20 | test["Hours"] == 21),]
aprilNightAvg <- c()
day <- 1
while (day <= 30) {
  df_temp <- aprilNight[aprilNight["Days"] == day,]
  aprilNightAvg[[day]] <- mean(df_temp["Global_active_power"][,], na.rm = TRUE)
  day <- day + 1
}
# View(aprilNightAvg)
plot(1:30, aprilNightAvg, type = "l", main = "Test 5 April Nights", xlab = "Days", ylab = "Global_active_power", col = "black", ylim = c(0.1, 11))
abline(h = springNightMax, col = "red")
abline(h = springNightMin, col = "red")

mayMorning <- test[test["Months"] == 5 & (test["Hours"] == 7 | test["Hours"] == 8 | test["Hours"] == 9 | test["Hours"] == 10),]
mayMorningAvg <- c()
day <- 1
while (day <= 31) {
  df_temp <- mayMorning[mayMorning["Days"] == day,]
  mayMorningAvg[[day]] <- mean(df_temp["Global_active_power"][,], na.rm = TRUE)
  day <- day + 1
}
# View(mayMorningAvg)
plot(1:31, mayMorningAvg, type = "l", main = "Test 5 May Mornings", xlab = "Days", ylab = "Global_active_power", col = "black", ylim = c(0.1, 7))
abline(h = springMorningMax, col = "red")
abline(h = springMorningMin, col = "red")

mayNight <- test[test["Months"] == 5 & (test["Hours"] == 18 | test["Hours"] == 19 | test["Hours"] == 20 | test["Hours"] == 21),]
mayNightAvg <- c()
day <- 1
while (day <= 31) {
  df_temp <- mayNight[mayNight["Days"] == day,]
  mayNightAvg[[day]] <- mean(df_temp["Global_active_power"][,], na.rm = TRUE)
  day <- day + 1
}
# View(mayNightAvg)
plot(1:31, mayNightAvg, type = "l", main = "Test 5 May Nights", xlab = "Days", ylab = "Global_active_power", col = "black", ylim = c(0.1, 8))
abline(h = springNightMax, col = "red")
abline(h = springNightMin, col = "red")

sepMorning <- test[test["Months"] == 9 & (test["Hours"] == 7 | test["Hours"] == 8 | test["Hours"] == 9 | test["Hours"] == 10),]
sepMorningAvg <- c()
day <- 1
while (day <= 30) {
  df_temp <- sepMorning[sepMorning["Days"] == day,]
  sepMorningAvg[[day]] <- mean(df_temp["Global_active_power"][,], na.rm = TRUE)
  day <- day + 1
}
# View(sepMorningAvg)
plot(1:30, sepMorningAvg, type = "l", main = "Test 5 Sep Mornings", xlab = "Days", ylab = "Global_active_power", col = "black", ylim = c(0.1, 6))
abline(h = fallMorningMax, col = "red")
abline(h = fallMorningMin, col = "red")

sepNight <- test[test["Months"] == 9 & (test["Hours"] == 18 | test["Hours"] == 19 | test["Hours"] == 20 | test["Hours"] == 21),]
sepNightAvg <- c()
day <- 1
while (day <= 30) {
  df_temp <- sepNight[sepNight["Days"] == day,]
  sepNightAvg[[day]] <- mean(df_temp["Global_active_power"][,], na.rm = TRUE)
  day <- day + 1
}
# View(sepNightAvg)
plot(1:30, sepNightAvg, type = "l", main = "Test 5 Sep Nights", xlab = "Days", ylab = "Global_active_power", col = "black", ylim = c(0.1, 8))
abline(h = fallNightMax, col = "red")
abline(h = fallNightMin, col = "red")

octMorning <- test[test["Months"] == 10 & (test["Hours"] == 7 | test["Hours"] == 8 | test["Hours"] == 9 | test["Hours"] == 10),]
octMorningAvg <- c()
day <- 1
while (day <= 31) {
  df_temp <- octMorning[octMorning["Days"] == day,]
  octMorningAvg[[day]] <- mean(df_temp["Global_active_power"][,], na.rm = TRUE)
  day <- day + 1
}
# View(octMorningAvg)
plot(1:31, octMorningAvg, type = "l", main = "Test 5 Oct Mornings", xlab = "Days", ylab = "Global_active_power", col = "black", ylim = c(0.1, 8))
abline(h = fallMorningMax, col = "red")
abline(h = fallMorningMin, col = "red")

octNight <- test[test["Months"] == 10 & (test["Hours"] == 18 | test["Hours"] == 19 | test["Hours"] == 20 | test["Hours"] == 21),]
octNightAvg <- c()
day <- 1
while (day <= 31) {
  df_temp <- octNight[octNight["Days"] == day,]
  octNightAvg[[day]] <- mean(df_temp["Global_active_power"][,], na.rm = TRUE)
  day <- day + 1
}
# View(octNightAvg)
plot(1:31, octNightAvg, type = "l", main = "Test 5 Oct Nights", xlab = "Days", ylab = "Global_active_power", col = "black", ylim = c(0.1, 10))
abline(h = fallNightMax, col = "red")
abline(h = fallNightMin, col = "red")

novMorning <- test[test["Months"] == 11 & (test["Hours"] == 7 | test["Hours"] == 8 | test["Hours"] == 9 | test["Hours"] == 10),]
novMorningAvg <- c()
day <- 1
while (day <= 30) {
  df_temp <- novMorning[novMorning["Days"] == day,]
  novMorningAvg[[day]] <- mean(df_temp["Global_active_power"][,], na.rm = TRUE)
  day <- day + 1
}
# View(novMorningAvg)
plot(1:30, novMorningAvg, type = "l", main = "Test 5 Nov Mornings", xlab = "Days", ylab = "Global_active_power", col = "black", ylim = c(0.1, 9))
abline(h = fallMorningMax, col = "red")
abline(h = fallMorningMin, col = "red")

novNight <- test[test["Months"] == 11 & (test["Hours"] == 18 | test["Hours"] == 19 | test["Hours"] == 20 | test["Hours"] == 21),]
novNightAvg <- c()
day <- 1
while (day <= 30) {
  df_temp <- novNight[novNight["Days"] == day,]
  novNightAvg[[day]] <- mean(df_temp["Global_active_power"][,], na.rm = TRUE)
  day <- day + 1
}
# View(octNightAvg)
plot(1:30, novNightAvg, type = "l", main = "Test 5 Nov Nights", xlab = "Days", ylab = "Global_active_power", col = "black", ylim = c(0.1, 10))
abline(h = fallNightMax, col = "red")
abline(h = fallNightMin, col = "red")

