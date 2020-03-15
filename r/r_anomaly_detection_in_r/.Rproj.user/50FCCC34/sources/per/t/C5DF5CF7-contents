setwd("/Users/nour/Desktop/Coursework/CMPT318/course_project")
library(astsa)
df = read.table("test1.txt", header = TRUE, sep = ",")

df["Hours"] <- format(strptime(df$Time,"%H:%M:%S"),'%H')
df["Hours"] <- as.numeric(df["Hours"][,])
df["Minutes"] <- format(strptime(df$Time,"%H:%M:%S"),'%M')
df["Minutes"] <- as.numeric(df["Minutes"][,])
# Creating Day Number from Time Series
df["Days"] <- weekdays(as.Date(df["Date"][,], format = "%d/%m/%Y"))
df["Months"] <- months(as.Date(df["Date"][,], format = "%d/%m/%Y"))

df_morn <- df[(df$Hours == 7 | df$Hours == 8 | df$Hours == 9 | df$Hours == 11),]
df_night <- df[(df$Hours == 18 | df$Hours == 19 | df$Hours == 20 | df$Hours == 21),]


#Seasonal Morning
df_winter_morn<- df_morn[df_morn$Months == "November" | df_morn$Months == "December" | df_morn$Months == "January" | df_morn$Months == "February", ]
df_spring_morn<- df_morn[df_morn$Months == "March" | df_morn$Months == "April" | df_morn$Months == "May", ]
df_summer_morn <- df_morn[df_morn$Months == "June" | df_morn$Months == "July" | df_morn$Months == "August", ]
df_fall_morn <- df_morn[df_morn$Months == "September" | df_morn$Months == "October", ]


####Winter Morning#######
wint_morn_glb_act <- df_winter_morn["Global_active_power"]
min_wint_glb_act <- min(wint_morn_glb_act,na.rm = TRUE)
max_wint_glb_act <- max(wint_morn_glb_act,na.rm = TRUE)
mean_wint_glb_act <- mean(wint_morn_glb_act[,], na.rm = TRUE)
sd_wint_glb_act <- sd(wint_morn_glb_act[,],na.rm = TRUE)

print("Winter Morning Minimum: ")
print(min_wint_glb_act)
print("Winter Morning Maximum: ")
print(max_wint_glb_act)
print("Winter Morning Mean: ")
print(mean_wint_glb_act)
print("Winter Morning Standard Dev: ")
print(sd_wint_glb_act)


####Spring Morning#######
spr_morn_glb_act <- df_spring_morn["Global_active_power"]
min_spr_glb_act <- min(spr_morn_glb_act,na.rm = TRUE)
max_spr_glb_act <- max(spr_morn_glb_act,na.rm = TRUE)
mean_spr_glb_act <- mean(spr_morn_glb_act[,], na.rm = TRUE)
sd_spr_glb_act <- sd(spr_morn_glb_act[,],na.rm = TRUE)

print("Spring Morning Minimum: ")
print(min_spr_glb_act)
print("Spring Morning Maximum: ")
print(max_spr_glb_act)
print("Spring Morning Mean: ")
print(mean_spr_glb_act)
print("Spring Morning Standard Dev: ")
print(sd_spr_glb_act)

####Fall Morning#######
fall_morn_glb_act <- df_fall_morn["Global_active_power"]
min_fall_glb_act <- min(fall_morn_glb_act,na.rm = TRUE)
max_fall_glb_act <- max(fall_morn_glb_act,na.rm = TRUE)
mean_fall_glb_act <- mean(fall_morn_glb_act[,], na.rm = TRUE)
sd_fall_glb_act <- sd(fall_morn_glb_act[,],na.rm = TRUE)

print("Fall Morning Minimum: ")
print(min_fall_glb_act)
print("Fall Morning Maximum: ")
print(max_fall_glb_act)
print("Fall Morning Mean: ")
print(mean_fall_glb_act)
print("Fall Morning Standard Dev: ")
print(sd_fall_glb_act)

####Summer Morning#######
sum_morn_glb_act <- df_summer_morn["Global_active_power"]
min_sum_glb_act <- min(sum_morn_glb_act,na.rm = TRUE)
max_sum_glb_act <- max(sum_morn_glb_act,na.rm = TRUE)
mean_sum_glb_act <- mean(sum_morn_glb_act[,], na.rm = TRUE)
sd_sum_glb_act <- sd(sum_morn_glb_act[,],na.rm = TRUE)

print("Summer Morning Minimum: ")
print(min_sum_glb_act)
print("Summer Morning Maximum: ")
print(max_sum_glb_act)
print("Summer Morning Mean: ")
print(mean_sum_glb_act)
print("Summer Morning Standard Dev: ")
print(sd_sum_glb_act)


#Seasonal Night
df_winter_night<- df_night[df_night$Months == "November" | df_night$Months == "December" | df_night$Months == "January" | df_night$Months == "February", ]
df_spring_night<- df_night[df_night$Months == "March" | df_night$Months == "April" | df_night$Months == "May", ]
df_summer_night <- df_night[df_night$Months == "June" | df_night$Months == "July" | df_night$Months == "August", ]
df_fall_night <- df_night[df_night$Months == "September" | df_night$Months == "October", ]

####Winter Night#######
wint_night_glb_act <- df_winter_night["Global_active_power"]
min_wint_night_glb_act <- min(wint_night_glb_act,na.rm = TRUE)
max_wint_night_glb_act <- max(wint_night_glb_act,na.rm = TRUE)
mean_wint_night_glb_act <- mean(wint_night_glb_act[,], na.rm = TRUE)
sd_wint_night_glb_act <- sd(wint_night_glb_act[,],na.rm = TRUE)
print("Winter Night Minimum: ")
print(min_wint_night_glb_act)
print("Winter Night Maximum: ")
print(max_wint_night_glb_act)
print("Winter Night Mean: ")
print(mean_wint_night_glb_act)
print("Winter Night Standard Dev: ")
print(sd_wint_night_glb_act)

####Spring Night#######
spr_night_glb_act <- df_spring_night["Global_active_power"]
min_spr_night_glb_act <- min(spr_night_glb_act,na.rm = TRUE)
max_spr_night_glb_act <- max(spr_night_glb_act,na.rm = TRUE)
mean_spr_night_glb_act <- mean(spr_night_glb_act[,], na.rm = TRUE)
sd_spr_night_glb_act <- sd(spr_night_glb_act[,],na.rm = TRUE)
print("Spring Night Minimum: ")
print(min_spr_night_glb_act)
print("Spring Night Maximum: ")
print(max_spr_night_glb_act)
print("Spring Night Mean: ")
print(mean_spr_night_glb_act)
print("Spring Night Standard Dev: ")
print(sd_spr_night_glb_act)

####Fall Night#######
fall_night_glb_act <- df_fall_night["Global_active_power"]
min_fall_night_glb_act <- min(fall_night_glb_act,na.rm = TRUE)
max_fall_night_glb_act <- max(fall_night_glb_act,na.rm = TRUE)
mean_fall_night_glb_act <- mean(fall_night_glb_act[,], na.rm = TRUE)
sd_fall_night_glb_act <- sd(fall_night_glb_act[,],na.rm = TRUE)
print("Fall Night Minimum: ")
print(min_fall_night_glb_act)
print("Fall Night Maximum: ")
print(max_fall_night_glb_act)
print("Fall Night Mean: ")
print(mean_fall_night_glb_act)
print("Fall Night Standard Dev: ")
print(sd_fall_night_glb_act)

####Summer Night#######
sum_night_glb_act <- df_summer_night["Global_active_power"]
min_sum_night_glb_act <- min(sum_night_glb_act,na.rm = TRUE)
max_sum_night_glb_act <- max(sum_night_glb_act,na.rm = TRUE)
mean_sum_night_glb_act <- mean(sum_night_glb_act[,], na.rm = TRUE)
sd_sum_night_glb_act <- sd(sum_night_glb_act[,],na.rm = TRUE)
print("Summer Night Minimum: ")
print(min_sum_night_glb_act)
print("Summer Night Maximum: ")
print(max_sum_night_glb_act)
print("Summer Night Mean: ")
print(mean_sum_night_glb_act)
print("Summer Night Standard Dev: ")
print(sd_sum_night_glb_act)


# wint_morn_glb_act <- df_winter_morn["Global_active_power"]
# wint_morn_time <- df_winter_morn["Time"]
# 
# summ_morn_glb_act <- df_summer_morn["Global_active_power"]
# summ_morn_time <- df_summer_morn["Time"]
# 
# fall_morn_glb_act <- df_fall_morn["Global_active_power"]
# fall_morn_time <- df_fall_morn["Time"]
# 
# spr_morn_glb_act <- df_spring_morn["Global_active_power"]
# spr_morn_time <- df_spring_morn["Time"]

write.table( wint_morn_time, file = "text.txt", sep = ",")


y_wint <- df_winter_morn$Global_active_power
x_wint <- df_winter_morn$Time

y_summ <- df_summer_morn$Global_active_power
x_summ  <- df_summer_morn$Time

y_fall <- df_fall_morn$Global_active_power
x_fall <- df_fall_morn$Time

y_spr <- df_spring_morn$Global_active_power
x_spr <- df_spring_morn$Time

# create_ts <- ts(data = df_morn$Time,frequency = 1)
# print(create_ts)
# ts_data <- decompose(create_ts, type="additive")
# print(ts_data)
# plot(ts_data)
# stlRes <- stl(df_morn$Time, s.window = "periodic")

linearMod <- lm(Global_active_power ~ Time, data=df_winter_morn)
summary(linearMod)
#xL <- as.POSIXct(c("09:00:00","14:00:00"))
plot(x_wint,y_wint,main = "Winter Morning Linear Regression",
     xlab = "Time", ylab = "Global_active_power")
abline(linearMod)


linearMod2 <- lm(Global_active_power ~ Time, data=df_fall_morn)
summary(linearMod2)
plot(x_fall,y_fall,main = "Fall Morning Linear Regression",
     xlab = "Time", ylab = "Global_active_power",ylim=c(0.8,4))
abline(linearMod2)

linearMod3 <- lm(Global_active_power ~ Time, data=df_spring_morn)
summary(linearMod3)
plot(x_spr,y_spr,main = "Spring Morning Linear Regression",
     xlab = "Time", ylab = "Global_active_power")
abline(linearMod3)

linearMod4 <- lm(Global_active_power ~ Time, data=df_summer_morn)
summary(linearMod4)
plot(x_summ,y_summ,main = "Summer Morning Linear Regression",
     xlab = "Time", ylab = "Global_active_power")
abline(linearMod4)

#Seasonal Nights

y_wint_nt <- df_winter_night$Global_active_power
x_wint_nt <- df_winter_night$Time

y_summ_nt <- df_summer_night$Global_active_power
x_summ_nt  <- df_summer_night$Time

y_fall_nt <- df_fall_night$Global_active_power
x_fall_nt <- df_fall_night$Time

y_spr_nt <- df_spring_night$Global_active_power
x_spr_nt <- df_spring_night$Time

linearMod_nt <- lm(Global_active_power ~ Time, data=df_winter_night)
summary(linearMod_nt)
#xL <- as.POSIXct(c("18:00:00","23:00:00"))
plot(x_wint_nt,y_wint_nt,main = "Winter Night Linear Regression",
     xlab = "Time", ylab = "Global_active_power")
abline(linearMod_nt)

linearMod2_nt <- lm(Global_active_power ~ Time, data=df_fall_night)
summary(linearMod2_nt)
plot(x_fall_nt,y_fall_nt,main = "Fall Night Linear Regression",
     xlab = "Time", ylab = "Global_active_power")
abline(linearMod2_nt)

linearMod3_nt <- lm(Global_active_power ~ Time, data=df_spring_night)
summary(linearMod3_nt)
plot(x_spr_nt,y_spr_nt,main = "Spring Night Linear Regression",
     xlab = "Time", ylab = "Global_active_power")
abline(linearMod3_nt)

linearMod4_nt <- lm(Global_active_power ~ Time, data=df_summer_night)
summary(linearMod4_nt)
plot(x_summ_nt,y_summ_nt,main = "Summer Night Linear Regression",
     xlab = "Time", ylab = "Global_active_power")
abline(linearMod4_nt)







