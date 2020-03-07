set.seed(567) #setting seed for reproducability
library(plyr)
library(dplyr)
library(glmnet)
library(gmodels)
library(caret)
library(pROC)
library(openxlsx)

#Defining help function I'll use later

#Mode Function: returns the most freuquent level of a variable
Mode = function(x){ 
  ta = table(x)
  tam = max(ta)
  if (all(ta == tam))
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  return(mod)
}

#evaluation function: evaluation function to for the test set prediction (Like in problem 3, just scaled by the number of rows)
eval <- function(default, pred_prob)
{
  LL <- 0 
  for (i in 1:length(default))
  {  
    LL = LL + (default[i] * log(pred_prob[i]) + ((1 - default[i]) * log(1 - pred_prob[i])))
  }
  return(as.numeric(LL/length(default)))
}


#Loading Data into R
load('assignment.Rdata') 

#creating subset of obs_id of the test set so I can merge it with the predicted probabilities later
pred_prob <- dat_test %>% select(obs_id)


#################################################################
#                     Problem 1                                 #
#################################################################

#Deleting obs_id as it will not be used in the model 
dat_train <- dat_train[,!(names(dat_train) %in% 'obs_id')]
dat_test <- dat_test[,!(names(dat_test) %in% 'obs_id')]


#checking how much percent of the loaners defaulted depending on their employment length

unique(dat_train$emp_length)
emp_length <- table(dat_train$default, dat_train$emp_length) 
emp_length1 <- table(dat_train$default, dat_train$emp_length)[2,] 
emp_length2 <- margin.table(emp_length, 2) 
emp_length3 <- emp_length1/emp_length2

par(mar = c(6.5, 5, 2.5, 2.5), mgp = c(4,0.5,0)) #moving the axis label
barplot(emp_length3, col = 'darkgrey', xlab = 'Employment length', ylab = '%', ylim = c(0, 0.25), las = 2, 
        main = 'Default percentage depending on the employment length')#bar graph which represents data

#counting for the number of missing values in the emp_length variable
dat_train$emp_length <- ifelse(dat_train$emp_length == 'n/a', NA, dat_train$emp_length) #change character 'n/a' to NA so that it's countable
dat_test$emp_length <- ifelse(dat_test$emp_length == 'n/a', NA, dat_test$emp_length) #same for the test set
sum(is.na(dat_train$emp_length))/50000 #percentage of missing values in the employment length column of the training set
sum(is.na(dat_test$emp_length))/50000 #percentage of missing values in the employment length column of the test set



#deleting the emp_length column from the dataframes since it has too many NAs
dat_train <- dat_train[, !(names(dat_train) %in% 'emp_length')]
dat_test <- dat_test[, !(names(dat_test) %in% 'emp_length')]


#checking how much percent of the loaners defaulted in each state
state <- table(dat_train$default, dat_train$addr_state)
state1 <- table(dat_train$default, dat_train$addr_state)[2,] #number of defaulters per state
state2 <- margin.table(state, 2) #number of total loaners per state
state3 <- state1/state2 #percentage of defaulters per state

barplot(state3, las = 2, col = 'darkgrey', xlab = 'US-State', ylab = '%', ylim = c(0,0.25),
        main = 'Default Percentage per State') #bar graph which represents data

#dropping the 'addr_state'-variable from the data frames, since irrelevant
dat_train <- dat_train[, !(names(dat_train) %in% 'addr_state')]
dat_test <- dat_test[, !(names(dat_test) %in% 'addr_state')]



#checking how much percent of the loaners defaulted with a non-verified status vs. a sourced verified status vs. a verified status 

ver <- table(dat_train$default, dat_train$verif_status) 
ver1 <- table(dat_train$default, dat_train$verif_status)[2,] 
ver2 <- margin.table(ver, 2) 
ver3 <- ver1/ver2

barplot(ver3, col = 'darkgrey', xlab = 'Verification status', ylab = '%', ylim = c(0, 0.25),
        main = 'Default percentage depending on income verification status') #bar graph which represents data 

#dropping the 'verif_status'-variable from the data frames
dat_train <- dat_train[, !(names(dat_train) %in% 'verif_status')] 
dat_test <- dat_test[, !(names(dat_test) %in% 'verif_status')] 

#checking how much percent of the loaners defaulted depending on their grade

grade <- table(dat_train$default, dat_train$grade) 
grade1 <- table(dat_train$default, dat_train$grade)[2,] 
grade2 <- margin.table(grade, 2) 
grade3 <- grade1/grade2

barplot(grade3, col = 'darkgrey', xlab = 'Loan grade', ylab = '%', ylim = c(0, 0.5),
        main = 'Default percentage depending on Loan Grade') #bar graph which represents data

#calculating default percentage depending on the interest rate
#plotting int_rate against the Probability of Default
plot(dat_train$int_rate, dat_train$default, xlab = "Interest rate", ylab = "Probability of Default", ylim = c(-0.2, 1.0), col = "darkorange")

#add logistic regression
reg1 <- glm(default ~ int_rate, data = dat_train, family = binomial('logit')) #regressing int_rate against default 
xv1 <- seq(min(dat_train$int_rate), max(dat_train$int_rate), 0.2)
yv1 <- predict(reg1, list(int_rate=xv1), type = "response") #predicting
lines(xv1, yv1) #adding regression line to the graph

####################################################################
#                         Problem 2                               #
###################################################################



#Creating my own training and test sets to find a model
#Using the sample function to create 3 random set of indices to conduct the training and test samples
# Store row numbers for training set 1: index_train
index_train <- sample(1:nrow(dat_train), 2/3 * nrow(dat_train))
# Store row numbers for training set 2: index_train_1
index_train_1 <- sample(1:nrow(dat_train), 2/3 * nrow(dat_train))
# Store row numbers for training set 3: index_train_2
index_train_2 <- sample(1:nrow(dat_train), 2/3 * nrow(dat_train))


#Own Train and Test Set 1
# Create training set 1: my_train_11_1
my_train_11_1 <- dat_train[index_train, ] 
# Create test set 1: my_test_11_1
my_test_11_1 <- dat_train[-index_train,] %>% 
                mutate(purpose = ifelse(purpose == 'wedding', Mode(purpose), purpose)) #so that test and training set have the same levels

#Own Train and Test Set 2 
# Create training set 2: my_train_11_2
my_train_11_2 <- dat_train[-index_train_1,] 
# Create test set 2: my_test_11_2
my_test_11_2 <- dat_train[index_train_1,] %>% 
             mutate(purpose = ifelse(purpose == 'wedding', Mode(purpose), purpose)) #so that test and training set have the same levels

#Own Train and Test Set 3
# Create training set 3: my_train_11_3
my_train_11_3 <- dat_train[index_train_2,] 
# Create test set 3: my_test_11_3
my_test_11_3 <- dat_train[-index_train_2,] %>% 
                mutate(purpose = ifelse(purpose == 'wedding', Mode(purpose), purpose)) #so that test and training set have the same levels



#Using all 11 remaing Variables: 

#first set
s1_logit11 <- glm(default ~., family = binomial, data = my_train_11_1) #using the logit (default version)
s1_p_logit11 <- predict(s1_logit11, newdata = my_test_11_1, type = "response")
s1_probit11 <- glm(default ~., family = binomial(link = probit), data = my_train_11_1)  #using the probit version
s1_p_probit11 <- predict(s1_probit11, newdata = my_test_11_1, type = "response")
s1_clog11 <- glm(default ~., family = binomial(link = cloglog), data = my_train_11_1)  #using the cloglog version
s1_p_clog11 <- predict(s1_clog11, newdata = my_test_11_1, type = "response")


#second set
s2_logit11 <- glm(default ~., family = binomial, data = my_train_11_2) #using the logit (default version)
s2_p_logit11 <- predict(s2_logit11, newdata = my_test_11_2, type = "response") 
s2_probit11 <- glm(default ~., family = binomial(link = probit), data = my_train_11_2) #using the probit version
s2_p_probit11 <- predict(s2_probit11, newdata = my_test_11_2, type = "response")
s2_clog11 <- glm(default ~., family = binomial(link = cloglog), data = my_train_11_2) #using the cloglog version
s2_p_clog11 <- predict(s2_clog11, newdata = my_test_11_2, type = "response")


#third set
s3_logit11 <- glm(default ~., family = binomial, data = my_train_11_3) #using the logit (default version)
s3_p_logit11 <- predict(s3_logit11, newdata = my_test_11_3, type = "response")
s3_probit11 <- glm(default ~., family = binomial(link = probit), data = my_train_11_3) #using the probit version
s3_p_probit11 <- predict(s3_probit11, newdata = my_test_11_3, type = "response")
s3_clog11 <- glm(default ~., family = binomial(link = cloglog), data = my_train_11_3) #using the cloglog version
s3_p_clog11 <- predict(s3_clog11, newdata = my_test_11_3, type = "response")


#evaluating the results
s1_e_logit11 <- round(eval(my_test_11_1$default, s1_p_logit11), digits = 6) #test set 1 logit
s1_e_probit11 <- round(eval(my_test_11_1$default, s1_p_probit11), digits = 6) #test set 1 probit
s1_e_clog11 <- round(eval(my_test_11_1$default, s1_p_clog11), digits = 6) #test set 1 cloglog
s2_e_logit11 <-round(eval(my_test_11_2$default, s2_p_logit11), digits = 6) #test set 2 logit
s2_e_probit11 <- round(eval(my_test_11_2$default, s2_p_probit11), digits = 6) #test set 2 probit
s2_e_clog11 <- round(eval(my_test_11_2$default, s2_p_clog11), digits = 6) #test set 2 cloglog
s3_e_logit11 <- round(eval(my_test_11_3$default, s3_p_logit11), digits = 6) #test set 3 logit
s3_e_probit11 <- round(eval(my_test_11_3$default, s3_p_probit11), digits = 6) #test set 3 probit
s3_e_clog11 <- round(eval(my_test_11_3$default, s3_p_clog11), digits = 6) #test set 3 cloglog

mean_clog11 <- round(mean(c(s1_e_clog11, s2_e_clog11, s3_e_clog11)), digits = 6) #creating the mean of the cloglog version of all 3 sets

#the following three lines each give back the model that performed best for each of the three test sets
max_s1_11 <- c('clog', 'logit', 'probit')[which.max(c(s1_e_clog11, s1_e_logit11, s1_e_probit11))] #set 1
max_s2_11 <- c('clog', 'logit', 'probit')[which.max(c(s2_e_clog11, s2_e_logit11, s2_e_probit11))] #set 2
max_s3_11 <- c('clog', 'logit', 'probit')[which.max(c(s3_e_clog11, s3_e_logit11, s3_e_probit11))] #set 3

# calculating confusion matrix with a threshold of 0.5
pred_clog11 <- ifelse(s1_p_clog11 > 0.5, TRUE, FALSE)
cm_clog11 <- table(my_test_11_1$default, pred_clog11)
mat_clog11 <- confusionMatrix(cm_clog11)

# calculating the ROC-curve so that the AUC can be calculated 
ROC_clog11 <- roc(my_test_11_1$default, s1_p_clog11) 


#creating dataframe with results 
#first column
model <- c('','Set 1: logit', 'Set 1: probit', 'Set 1: clog', 'Set 2: logit', 'Set 2: probit', 'Set 2: clog', 'Set 3: logit', 'Set3: probit',
           'Set3: clog','Maximum Set 1', 'Maximum Set 2', 'Maximum Set 3', 'mean: clog', 'Accuracy: clog', 'AUC: clog')
#second column (first data column)
all_11 <- c('All 11 Var.', s1_e_logit11, s1_e_probit11, s1_e_clog11, s2_e_logit11, s2_e_probit11, s2_e_clog11, s3_e_logit11, s3_e_probit11,
            s3_e_clog11, max_s1_11, max_s2_11, max_s3_11, mean_clog11, round(mat_clog11$overall['Accuracy'], digits = 6), round(auc(ROC_clog11), digits = 6))

#creating dataframe
find_model <- data.frame(model, all_11)



###############################################
#            Using  10 Variables              #
###############################################

#Leave each of the 11 Variables out of the model one after another and compare the LL-values (the value that 
#the evaluation function gives back).Find the highest LL-value of the coming models and then stick with that model. 

#########################
#Leaving out 'loan_amnt'#
#########################
my_train_10_1_1 <- my_train_11_1 %>% select(-loan_amnt) 
my_test_10_1_1 <- my_test_11_1 %>% select(-loan_amnt) 

my_train_10_1_2 <- my_train_11_2 %>% select(-loan_amnt) 
my_test_10_1_2 <- my_test_11_2 %>% select(-loan_amnt)
  
my_train_10_1_3 <- my_train_11_3 %>% select(-loan_amnt) 
my_test_10_1_3 <- my_test_11_2 %>% select(-loan_amnt)

#calculation and evaluation is the same as for the 11 variables. Therefore I have left out the comments
#first set
s1_logit10_1 <- glm(default ~., family = binomial, data = my_train_10_1_1) 
s1_p_logit10_1 <- predict(s1_logit10_1, newdata = my_test_10_1_1, type = "response")
s1_probit10_1 <- glm(default ~., family = binomial(link = probit), data = my_train_10_1_1) 
s1_p_probit10_1 <- predict(s1_probit10_1, newdata = my_test_10_1_1, type = "response")
s1_clog10_1 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_1_1) 
s1_p_clog10_1 <- predict(s1_clog10_1, newdata = my_test_10_1_1, type = "response")


#second set
s2_logit10_1 <- glm(default ~., family = binomial, data = my_train_10_1_2) 
s2_p_logit10_1 <- predict(s2_logit10_1, newdata = my_test_10_1_2, type = "response")
s2_probit10_1 <- glm(default ~., family = binomial(link = probit), data = my_train_10_1_2) 
s2_p_probit10_1 <- predict(s2_probit10_1, newdata = my_test_10_1_2, type = "response")
s2_clog10_1 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_1_2) 
s2_p_clog10_1 <- predict(s2_clog10_1, newdata = my_test_10_1_2, type = "response")


#third set
s3_logit10_1 <- glm(default ~., family = binomial, data = my_train_10_1_3) 
s3_p_logit10_1 <- predict(s3_logit10_1, newdata = my_test_10_1_3, type = "response")
s3_probit10_1 <- glm(default ~., family = binomial(link = probit), data = my_train_10_1_3) 
s3_p_probit10_1 <- predict(s3_probit10_1, newdata = my_test_10_1_3, type = "response")
s3_clog10_1 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_1_3) 
s3_p_clog10_1 <- predict(s3_clog10_1, newdata = my_test_10_1_3, type = "response")


#evaluating the results
s1_e_logit10_1 <- round(eval(my_test_10_1_1$default, s1_p_logit10_1), digits = 6)
s1_e_probit10_1 <- round(eval(my_test_10_1_1$default, s1_p_probit10_1), digits = 6)
s1_e_clog10_1 <- round(eval(my_test_10_1_1$default, s1_p_clog10_1), digits = 6)
s2_e_logit10_1 <-round(eval(my_test_10_1_2$default, s2_p_logit10_1), digits = 6)
s2_e_probit10_1 <- round(eval(my_test_10_1_2$default, s2_p_probit10_1), digits = 6)
s2_e_clog10_1 <- round(eval(my_test_10_1_2$default, s2_p_clog10_1), digits = 6)
s3_e_logit10_1 <- round(eval(my_test_10_1_3$default, s3_p_logit10_1), digits = 6)
s3_e_probit10_1 <- round(eval(my_test_10_1_3$default, s3_p_probit10_1), digits = 6)
s3_e_clog10_1 <- round(eval(my_test_10_1_3$default, s3_p_clog10_1), digits = 6)

mean_clog10_1 <- round(mean(c(s1_e_clog10_1, s2_e_clog10_1, s3_e_clog10_1)), digits = 6)

max_s1_10_1 <- c('clog', 'logit', 'probit')[which.max(c(s1_e_clog10_1, s1_e_logit10_1, s1_e_probit10_1))] 
max_s2_10_1 <- c('clog', 'logit', 'probit')[which.max(c(s2_e_clog10_1, s2_e_logit10_1, s2_e_probit10_1))]
max_s3_10_1 <- c('clog', 'logit', 'probit')[which.max(c(s3_e_clog10_1, s3_e_logit10_1, s3_e_probit10_1))]

#confusion matrix
pred_clog10_1 <- ifelse(s1_p_clog10_1 > 0.5, TRUE, FALSE) 
cm_clog10_1 <- table(my_test_10_1_1$default, pred_clog10_1)
mat_clog10_1 <- confusionMatrix(cm_clog10_1)

#ROC-curve
ROC_clog10_1 <- roc(my_test_10_1_1$default, s1_p_clog10_1) 


#appending results to Dataframe
all_10_1 <- c('w/o loan_amnt', s1_e_logit10_1, s1_e_probit10_1, s1_e_clog10_1, s2_e_logit10_1, s2_e_probit10_1, s2_e_clog10_1, s3_e_logit10_1, s3_e_probit10_1,
              s3_e_clog10_1, max_s1_10_1, max_s2_10_1, max_s3_10_1, mean_clog10_1, round(mat_clog10_1$overall['Accuracy'], digits = 6), round(auc(ROC_clog10_1), digits = 6))

find_model <- cbind(find_model, all_10_1) 

####################
#leaving out 'term'#
####################
my_train_10_2_1 <- my_train_11_1 %>% select(-term) 
my_test_10_2_1 <- my_test_11_1 %>% select(-term) 

my_train_10_2_2 <- my_train_11_2 %>% select(-term) 
my_test_10_2_2 <- my_test_11_2 %>% select(-term)

my_train_10_2_3 <- my_train_11_3 %>% select(-term) 
my_test_10_2_3 <- my_test_11_2 %>% select(-term)

#calculation and evaluation is the same as for the 11 variables. Therefore I have left out the comments
#first set
s1_logit10_2 <- glm(default ~., family = binomial, data = my_train_10_2_1) 
s1_p_logit10_2 <- predict(s1_logit10_2, newdata = my_test_10_2_1, type = "response")
s1_probit10_2 <- glm(default ~., family = binomial(link = probit), data = my_train_10_2_1) 
s1_p_probit10_2 <- predict(s1_probit10_2, newdata = my_test_10_2_1, type = "response")
s1_clog10_2 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_2_1) 
s1_p_clog10_2 <- predict(s1_clog10_2, newdata = my_test_10_2_1, type = "response")


#second set
s2_logit10_2 <- glm(default ~., family = binomial, data = my_train_10_2_2) 
s2_p_logit10_2 <- predict(s2_logit10_2, newdata = my_test_10_2_2, type = "response")
s2_probit10_2 <- glm(default ~., family = binomial(link = probit), data = my_train_10_2_2) 
s2_p_probit10_2 <- predict(s2_probit10_2, newdata = my_test_10_2_2, type = "response")
s2_clog10_2 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_2_2) 
s2_p_clog10_2 <- predict(s2_clog10_2, newdata = my_test_10_2_2, type = "response")


#third set
s3_logit10_2 <- glm(default ~., family = binomial, data = my_train_10_2_3) 
s3_p_logit10_2 <- predict(s3_logit10_2, newdata = my_test_10_2_3, type = "response")
s3_probit10_2 <- glm(default ~., family = binomial(link = probit), data = my_train_10_2_3) 
s3_p_probit10_2 <- predict(s3_probit10_2, newdata = my_test_10_2_3, type = "response")
s3_clog10_2 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_2_3) 
s3_p_clog10_2 <- predict(s3_clog10_2, newdata = my_test_10_2_3, type = "response")


#evaluating the results
s1_e_logit10_2 <- round(eval(my_test_10_2_1$default, s1_p_logit10_2), digits = 6)
s1_e_probit10_2 <- round(eval(my_test_10_2_1$default, s1_p_probit10_2), digits = 6)
s1_e_clog10_2 <- round(eval(my_test_10_2_1$default, s1_p_clog10_2), digits = 6)
s2_e_logit10_2 <-round(eval(my_test_10_2_2$default, s2_p_logit10_2), digits = 6)
s2_e_probit10_2 <- round(eval(my_test_10_2_2$default, s2_p_probit10_2), digits = 6)
s2_e_clog10_2 <- round(eval(my_test_10_2_2$default, s2_p_clog10_2), digits = 6)
s3_e_logit10_2 <- round(eval(my_test_10_2_3$default, s3_p_logit10_2), digits = 6)
s3_e_probit10_2 <- round(eval(my_test_10_2_3$default, s3_p_probit10_2), digits = 6)
s3_e_clog10_2 <- round(eval(my_test_10_2_3$default, s3_p_clog10_2), digits = 6)

mean_clog10_2 <- round(mean(c(s1_e_clog10_2, s2_e_clog10_2, s3_e_clog10_2)), digits = 6)

max_s1_10_2 <- c('clog', 'logit', 'probit')[which.max(c(s1_e_clog10_2, s1_e_logit10_2, s1_e_probit10_2))] 
max_s2_10_2 <- c('clog', 'logit', 'probit')[which.max(c(s2_e_clog10_2, s2_e_logit10_2, s2_e_probit10_2))]
max_s3_10_2 <- c('clog', 'logit', 'probit')[which.max(c(s3_e_clog10_2, s3_e_logit10_2, s3_e_probit10_2))]

#confusion matrix 
pred_clog10_2 <- ifelse(s1_p_clog10_2 > 0.5, TRUE, FALSE) 
cm_clog10_2 <- table(my_test_10_2_1$default, pred_clog10_2)
mat_clog10_2 <- confusionMatrix(cm_clog10_2)

#ROC-curve
ROC_clog10_2 <- roc(my_test_10_2_1$default, s1_p_clog10_2) 


#appending results to Dataframe
all_10_2 <- c('w/o term', s1_e_logit10_2, s1_e_probit10_2, s1_e_clog10_2, s2_e_logit10_2, s2_e_probit10_2, s2_e_clog10_2, s3_e_logit10_2, s3_e_probit10_2,
              s3_e_clog10_2, max_s1_10_2, max_s2_10_2, max_s3_10_2, mean_clog10_2, round(mat_clog10_2$overall['Accuracy'], digits = 6), round(auc(ROC_clog10_2), digits = 6))

find_model <- cbind(find_model, all_10_2) 


########################
#leaving out 'int_rate'#
########################
my_train_10_3_1 <- my_train_11_1 %>% select(-int_rate) 
my_test_10_3_1 <- my_test_11_1 %>% select(-int_rate) 

my_train_10_3_2 <- my_train_11_2 %>% select(-int_rate) 
my_test_10_3_2 <- my_test_11_2 %>% select(-int_rate)

my_train_10_3_3 <- my_train_11_3 %>% select(-int_rate) 
my_test_10_3_3 <- my_test_11_2 %>% select(-int_rate)

#calculation and evaluation is the same as for the 11 variables. Therefore I have left out the comments
#first set
s1_logit10_3 <- glm(default ~., family = binomial, data = my_train_10_3_1) 
s1_p_logit10_3 <- predict(s1_logit10_3, newdata = my_test_10_3_1, type = "response")
s1_probit10_3 <- glm(default ~., family = binomial(link = probit), data = my_train_10_3_1) 
s1_p_probit10_3 <- predict(s1_probit10_3, newdata = my_test_10_3_1, type = "response")
s1_clog10_3 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_3_1) 
s1_p_clog10_3 <- predict(s1_clog10_3, newdata = my_test_10_3_1, type = "response")


#second set
s2_logit10_3 <- glm(default ~., family = binomial, data = my_train_10_3_2) 
s2_p_logit10_3 <- predict(s2_logit10_3, newdata = my_test_10_3_2, type = "response")
s2_probit10_3 <- glm(default ~., family = binomial(link = probit), data = my_train_10_3_2) 
s2_p_probit10_3 <- predict(s2_probit10_3, newdata = my_test_10_3_2, type = "response")
s2_clog10_3 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_3_2) 
s2_p_clog10_3 <- predict(s2_clog10_3, newdata = my_test_10_3_2, type = "response")


#third set
s3_logit10_3 <- glm(default ~., family = binomial, data = my_train_10_3_3) 
s3_p_logit10_3 <- predict(s3_logit10_3, newdata = my_test_10_3_3, type = "response")
s3_probit10_3 <- glm(default ~., family = binomial(link = probit), data = my_train_10_3_3) 
s3_p_probit10_3 <- predict(s3_probit10_3, newdata = my_test_10_3_3, type = "response")
s3_clog10_3 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_3_3) 
s3_p_clog10_3 <- predict(s3_clog10_3, newdata = my_test_10_3_3, type = "response")


#evaluating the results
s1_e_logit10_3 <- round(eval(my_test_10_3_1$default, s1_p_logit10_3), digits = 6)
s1_e_probit10_3 <- round(eval(my_test_10_3_1$default, s1_p_probit10_3), digits = 6)
s1_e_clog10_3 <- round(eval(my_test_10_3_1$default, s1_p_clog10_3), digits = 6)
s2_e_logit10_3 <-round(eval(my_test_10_3_2$default, s2_p_logit10_3), digits = 6)
s2_e_probit10_3 <- round(eval(my_test_10_3_2$default, s2_p_probit10_3), digits = 6)
s2_e_clog10_3 <- round(eval(my_test_10_3_2$default, s2_p_clog10_3), digits = 6)
s3_e_logit10_3 <- round(eval(my_test_10_3_3$default, s3_p_logit10_3), digits = 6)
s3_e_probit10_3 <- round(eval(my_test_10_3_3$default, s3_p_probit10_3), digits = 6)
s3_e_clog10_3 <- round(eval(my_test_10_3_3$default, s3_p_clog10_3), digits = 6)

mean_clog10_3 <- round(mean(c(s1_e_clog10_3, s2_e_clog10_3, s3_e_clog10_3)), digits = 6)

max_s1_10_3 <- c('clog', 'logit', 'probit')[which.max(c(s1_e_clog10_3, s1_e_logit10_3, s1_e_probit10_3))] 
max_s2_10_3 <- c('clog', 'logit', 'probit')[which.max(c(s2_e_clog10_3, s2_e_logit10_3, s2_e_probit10_3))]
max_s3_10_3 <- c('clog', 'logit', 'probit')[which.max(c(s3_e_clog10_3, s3_e_logit10_3, s3_e_probit10_3))]

#confusion matrix 
pred_clog10_3 <- ifelse(s1_p_clog10_3 > 0.5, TRUE, FALSE) 
cm_clog10_3 <- table(my_test_10_3_1$default, pred_clog10_3)
mat_clog10_3 <- confusionMatrix(cm_clog10_3)

#ROC-curve
ROC_clog10_3 <- roc(my_test_10_3_1$default, s1_p_clog10_3) 


#appending results to Dataframe
all_10_3 <- c('w/o int_rate', s1_e_logit10_3, s1_e_probit10_3, s1_e_clog10_3, s2_e_logit10_3, s2_e_probit10_3, s2_e_clog10_3, s3_e_logit10_3, s3_e_probit10_3,
              s3_e_clog10_3, max_s1_10_3, max_s2_10_3, max_s3_10_3, mean_clog10_3, round(mat_clog10_3$overall['Accuracy'], digits = 6), round(auc(ROC_clog10_3), digits = 6))

find_model <- cbind(find_model, all_10_3)


###########################
#leaving out 'installment'#
###########################
my_train_10_4_1 <- my_train_11_1 %>% select(-installment) 
my_test_10_4_1 <- my_test_11_1 %>% select(-installment) 

my_train_10_4_2 <- my_train_11_2 %>% select(-installment) 
my_test_10_4_2 <- my_test_11_2 %>% select(-installment)

my_train_10_4_3 <- my_train_11_3 %>% select(-installment) 
my_test_10_4_3 <- my_test_11_2 %>% select(-installment)

#calculation and evaluation is the same as for the 11 variables. Therefore I have left out the comments
#first set
s1_logit10_4 <- glm(default ~., family = binomial, data = my_train_10_4_1) 
s1_p_logit10_4 <- predict(s1_logit10_4, newdata = my_test_10_4_1, type = "response")
s1_probit10_4 <- glm(default ~., family = binomial(link = probit), data = my_train_10_4_1) 
s1_p_probit10_4 <- predict(s1_probit10_4, newdata = my_test_10_4_1, type = "response")
s1_clog10_4 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_4_1) 
s1_p_clog10_4 <- predict(s1_clog10_4, newdata = my_test_10_4_1, type = "response")


#second set
s2_logit10_4 <- glm(default ~., family = binomial, data = my_train_10_4_2) 
s2_p_logit10_4 <- predict(s2_logit10_4, newdata = my_test_10_4_2, type = "response")
s2_probit10_4 <- glm(default ~., family = binomial(link = probit), data = my_train_10_4_2) 
s2_p_probit10_4 <- predict(s2_probit10_4, newdata = my_test_10_4_2, type = "response")
s2_clog10_4 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_4_2) 
s2_p_clog10_4 <- predict(s2_clog10_4, newdata = my_test_10_4_2, type = "response")


#third set
s3_logit10_4 <- glm(default ~., family = binomial, data = my_train_10_4_3) 
s3_p_logit10_4 <- predict(s3_logit10_4, newdata = my_test_10_4_3, type = "response")
s3_probit10_4 <- glm(default ~., family = binomial(link = probit), data = my_train_10_4_3) 
s3_p_probit10_4 <- predict(s3_probit10_4, newdata = my_test_10_4_3, type = "response")
s3_clog10_4 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_4_3) 
s3_p_clog10_4 <- predict(s3_clog10_4, newdata = my_test_10_4_3, type = "response")


#evaluating the results
s1_e_logit10_4 <- round(eval(my_test_10_4_1$default, s1_p_logit10_4), digits = 6)
s1_e_probit10_4 <- round(eval(my_test_10_4_1$default, s1_p_probit10_4), digits = 6)
s1_e_clog10_4 <- round(eval(my_test_10_4_1$default, s1_p_clog10_4), digits = 6)
s2_e_logit10_4 <-round(eval(my_test_10_4_2$default, s2_p_logit10_4), digits = 6)
s2_e_probit10_4 <- round(eval(my_test_10_4_2$default, s2_p_probit10_4), digits = 6)
s2_e_clog10_4 <- round(eval(my_test_10_4_2$default, s2_p_clog10_4), digits = 6)
s3_e_logit10_4 <- round(eval(my_test_10_4_3$default, s3_p_logit10_4), digits = 6)
s3_e_probit10_4 <- round(eval(my_test_10_4_3$default, s3_p_probit10_4), digits = 6)
s3_e_clog10_4 <- round(eval(my_test_10_4_3$default, s3_p_clog10_4), digits = 6)

mean_clog10_4 <- round(mean(c(s1_e_clog10_4, s2_e_clog10_4, s3_e_clog10_4)), digits = 6)

max_s1_10_4 <- c('clog', 'logit', 'probit')[which.max(c(s1_e_clog10_4, s1_e_logit10_4, s1_e_probit10_4))] 
max_s2_10_4 <- c('clog', 'logit', 'probit')[which.max(c(s2_e_clog10_4, s2_e_logit10_4, s2_e_probit10_4))]
max_s3_10_4 <- c('clog', 'logit', 'probit')[which.max(c(s3_e_clog10_4, s3_e_logit10_4, s3_e_probit10_4))]

#confusion matrix 
pred_clog10_4 <- ifelse(s1_p_clog10_4 > 0.5, TRUE, FALSE) 
cm_clog10_4 <- table(my_test_10_4_1$default, pred_clog10_4)
mat_clog10_4 <- confusionMatrix(cm_clog10_4)

#ROC-curve
ROC_clog10_4 <- roc(my_test_10_4_1$default, s1_p_clog10_4) 


#appending results to Dataframe
all_10_4 <- c('w/o instment', s1_e_logit10_4, s1_e_probit10_4, s1_e_clog10_4, s2_e_logit10_4, s2_e_probit10_4, s2_e_clog10_4, s3_e_logit10_4, s3_e_probit10_4,
              s3_e_clog10_4, max_s1_10_4, max_s2_10_4, max_s3_10_4, mean_clog10_4, round(mat_clog10_4$overall['Accuracy'], digits = 6), round(auc(ROC_clog10_4), digits = 6))

find_model <- cbind(find_model, all_10_4)




#####################
#leaving out 'grade'#
#####################
my_train_10_5_1 <- my_train_11_1 %>% select(-grade) 
my_test_10_5_1 <- my_test_11_1 %>% select(-grade) 

my_train_10_5_2 <- my_train_11_2 %>% select(-grade) 
my_test_10_5_2 <- my_test_11_2 %>% select(-grade)

my_train_10_5_3 <- my_train_11_3 %>% select(-grade) 
my_test_10_5_3 <- my_test_11_2 %>% select(-grade)

#calculation and evaluation is the same as for the 11 variables. Therefore I have left out the comments
#first set
s1_logit10_5 <- glm(default ~., family = binomial, data = my_train_10_5_1) 
s1_p_logit10_5 <- predict(s1_logit10_5, newdata = my_test_10_5_1, type = "response")
s1_probit10_5 <- glm(default ~., family = binomial(link = probit), data = my_train_10_5_1) 
s1_p_probit10_5 <- predict(s1_probit10_5, newdata = my_test_10_5_1, type = "response")
s1_clog10_5 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_5_1) 
s1_p_clog10_5 <- predict(s1_clog10_5, newdata = my_test_10_5_1, type = "response")


#second set
s2_logit10_5 <- glm(default ~., family = binomial, data = my_train_10_5_2) 
s2_p_logit10_5 <- predict(s2_logit10_5, newdata = my_test_10_5_2, type = "response")
s2_probit10_5 <- glm(default ~., family = binomial(link = probit), data = my_train_10_5_2) 
s2_p_probit10_5 <- predict(s2_probit10_5, newdata = my_test_10_5_2, type = "response")
s2_clog10_5 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_5_2) 
s2_p_clog10_5 <- predict(s2_clog10_5, newdata = my_test_10_5_2, type = "response")


#third set
s3_logit10_5 <- glm(default ~., family = binomial, data = my_train_10_5_3) 
s3_p_logit10_5 <- predict(s3_logit10_5, newdata = my_test_10_5_3, type = "response")
s3_probit10_5 <- glm(default ~., family = binomial(link = probit), data = my_train_10_5_3) 
s3_p_probit10_5 <- predict(s3_probit10_5, newdata = my_test_10_5_3, type = "response")
s3_clog10_5 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_5_3) 
s3_p_clog10_5 <- predict(s3_clog10_5, newdata = my_test_10_5_3, type = "response")


#evaluating the results
s1_e_logit10_5 <- round(eval(my_test_10_5_1$default, s1_p_logit10_5), digits = 6)
s1_e_probit10_5 <- round(eval(my_test_10_5_1$default, s1_p_probit10_5), digits = 6)
s1_e_clog10_5 <- round(eval(my_test_10_5_1$default, s1_p_clog10_5), digits = 6)
s2_e_logit10_5 <-round(eval(my_test_10_5_2$default, s2_p_logit10_5), digits = 6)
s2_e_probit10_5 <- round(eval(my_test_10_5_2$default, s2_p_probit10_5), digits = 6)
s2_e_clog10_5 <- round(eval(my_test_10_5_2$default, s2_p_clog10_5), digits = 6)
s3_e_logit10_5 <- round(eval(my_test_10_5_3$default, s3_p_logit10_5), digits = 6)
s3_e_probit10_5 <- round(eval(my_test_10_5_3$default, s3_p_probit10_5), digits = 6)
s3_e_clog10_5 <- round(eval(my_test_10_5_3$default, s3_p_clog10_5), digits = 6)

mean_clog10_5 <- round(mean(c(s1_e_clog10_5, s2_e_clog10_5, s3_e_clog10_5)), digits = 6)

max_s1_10_5 <- c('clog', 'logit', 'probit')[which.max(c(s1_e_clog10_5, s1_e_logit10_5, s1_e_probit10_5))] 
max_s2_10_5 <- c('clog', 'logit', 'probit')[which.max(c(s2_e_clog10_5, s2_e_logit10_5, s2_e_probit10_5))]
max_s3_10_5 <- c('clog', 'logit', 'probit')[which.max(c(s3_e_clog10_5, s3_e_logit10_5, s3_e_probit10_5))]

#confusion matrix 
pred_clog10_5 <- ifelse(s1_p_clog10_5 > 0.5, TRUE, FALSE) 
cm_clog10_5 <- table(my_test_10_5_1$default, pred_clog10_5)
mat_clog10_5 <- confusionMatrix(cm_clog10_5)

#ROC-curve
ROC_clog10_5 <- roc(my_test_10_5_1$default, s1_p_clog10_5) 


#appending results to Dataframe
all_10_5 <- c('w/o grade', s1_e_logit10_5, s1_e_probit10_5, s1_e_clog10_5, s2_e_logit10_5, s2_e_probit10_5, s2_e_clog10_5, s3_e_logit10_5, s3_e_probit10_5,
              s3_e_clog10_5, max_s1_10_5, max_s2_10_5, max_s3_10_5, mean_clog10_5, round(mat_clog10_5$overall['Accuracy'], digits = 6), round(auc(ROC_clog10_5), digits = 6))

find_model <- cbind(find_model, all_10_5)




##############################
#leaving out 'home_ownership'#
##############################
my_train_10_6_1 <- my_train_11_1 %>% select(-home_ownership) 
my_test_10_6_1 <- my_test_11_1 %>% select(-home_ownership) 

my_train_10_6_2 <- my_train_11_2 %>% select(-home_ownership) 
my_test_10_6_2 <- my_test_11_2 %>% select(-home_ownership)

my_train_10_6_3 <- my_train_11_3 %>% select(-home_ownership) 
my_test_10_6_3 <- my_test_11_2 %>% select(-home_ownership)

#calculation and evaluation is the same as for the 11 variables. Therefore I have left out the comments
#first set
s1_logit10_6 <- glm(default ~., family = binomial, data = my_train_10_6_1) 
s1_p_logit10_6 <- predict(s1_logit10_6, newdata = my_test_10_6_1, type = "response")
s1_probit10_6 <- glm(default ~., family = binomial(link = probit), data = my_train_10_6_1) 
s1_p_probit10_6 <- predict(s1_probit10_6, newdata = my_test_10_6_1, type = "response")
s1_clog10_6 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_6_1) 
s1_p_clog10_6 <- predict(s1_clog10_6, newdata = my_test_10_6_1, type = "response")


#second set
s2_logit10_6 <- glm(default ~., family = binomial, data = my_train_10_6_2) 
s2_p_logit10_6 <- predict(s2_logit10_6, newdata = my_test_10_6_2, type = "response")
s2_probit10_6 <- glm(default ~., family = binomial(link = probit), data = my_train_10_6_2) 
s2_p_probit10_6 <- predict(s2_probit10_6, newdata = my_test_10_6_2, type = "response")
s2_clog10_6 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_6_2) 
s2_p_clog10_6 <- predict(s2_clog10_6, newdata = my_test_10_6_2, type = "response")


#third set
s3_logit10_6 <- glm(default ~., family = binomial, data = my_train_10_6_3) 
s3_p_logit10_6 <- predict(s3_logit10_6, newdata = my_test_10_6_3, type = "response")
s3_probit10_6 <- glm(default ~., family = binomial(link = probit), data = my_train_10_6_3) 
s3_p_probit10_6 <- predict(s3_probit10_6, newdata = my_test_10_6_3, type = "response")
s3_clog10_6 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_6_3) 
s3_p_clog10_6 <- predict(s3_clog10_6, newdata = my_test_10_6_3, type = "response")


#evaluating the results
s1_e_logit10_6 <- round(eval(my_test_10_6_1$default, s1_p_logit10_6), digits = 6)
s1_e_probit10_6 <- round(eval(my_test_10_6_1$default, s1_p_probit10_6), digits = 6)
s1_e_clog10_6 <- round(eval(my_test_10_6_1$default, s1_p_clog10_6), digits = 6)
s2_e_logit10_6 <-round(eval(my_test_10_6_2$default, s2_p_logit10_6), digits = 6)
s2_e_probit10_6 <- round(eval(my_test_10_6_2$default, s2_p_probit10_6), digits = 6)
s2_e_clog10_6 <- round(eval(my_test_10_6_2$default, s2_p_clog10_6), digits = 6)
s3_e_logit10_6 <- round(eval(my_test_10_6_3$default, s3_p_logit10_6), digits = 6)
s3_e_probit10_6 <- round(eval(my_test_10_6_3$default, s3_p_probit10_6), digits = 6)
s3_e_clog10_6 <- round(eval(my_test_10_6_3$default, s3_p_clog10_6), digits = 6)

mean_clog10_6 <- round(mean(c(s1_e_clog10_6, s2_e_clog10_6, s3_e_clog10_6)), digits = 6)

max_s1_10_6 <- c('clog', 'logit', 'probit')[which.max(c(s1_e_clog10_6, s1_e_logit10_6, s1_e_probit10_6))] 
max_s2_10_6 <- c('clog', 'logit', 'probit')[which.max(c(s2_e_clog10_6, s2_e_logit10_6, s2_e_probit10_6))]
max_s3_10_6 <- c('clog', 'logit', 'probit')[which.max(c(s3_e_clog10_6, s3_e_logit10_6, s3_e_probit10_6))]

#confusion matrix 
pred_clog10_6 <- ifelse(s1_p_clog10_6 > 0.5, TRUE, FALSE) 
cm_clog10_6 <- table(my_test_10_6_1$default, pred_clog10_6)
mat_clog10_6 <- confusionMatrix(cm_clog10_6)

#ROC-curve
ROC_clog10_6 <- roc(my_test_10_6_1$default, s1_p_clog10_6) 


#appending results to Dataframe
all_10_6 <- c('w/o h.-ownership', s1_e_logit10_6, s1_e_probit10_6, s1_e_clog10_6, s2_e_logit10_6, s2_e_probit10_6, s2_e_clog10_6, s3_e_logit10_6, s3_e_probit10_6,
              s3_e_clog10_6, max_s1_10_6, max_s2_10_6, max_s3_10_6, mean_clog10_6, round(mat_clog10_6$overall['Accuracy'], digits = 6), round(auc(ROC_clog10_6), digits = 6))

find_model <- cbind(find_model, all_10_6)


##########################
#leaving out 'annual_inc'#
##########################
my_train_10_7_1 <- my_train_11_1 %>% select(-annual_inc) 
my_test_10_7_1 <- my_test_11_1 %>% select(-annual_inc) 

my_train_10_7_2 <- my_train_11_2 %>% select(-annual_inc) 
my_test_10_7_2 <- my_test_11_2 %>% select(-annual_inc)

my_train_10_7_3 <- my_train_11_3 %>% select(-annual_inc) 
my_test_10_7_3 <- my_test_11_2 %>% select(-annual_inc)

#calculation and evaluation is the same as for the 11 variables. Therefore I have left out the comments
#first set
s1_logit10_7 <- glm(default ~., family = binomial, data = my_train_10_7_1) 
s1_p_logit10_7 <- predict(s1_logit10_7, newdata = my_test_10_7_1, type = "response")
s1_probit10_7 <- glm(default ~., family = binomial(link = probit), data = my_train_10_7_1) 
s1_p_probit10_7 <- predict(s1_probit10_7, newdata = my_test_10_7_1, type = "response")
s1_clog10_7 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_7_1) 
s1_p_clog10_7 <- predict(s1_clog10_7, newdata = my_test_10_7_1, type = "response")


#second set
s2_logit10_7 <- glm(default ~., family = binomial, data = my_train_10_7_2) 
s2_p_logit10_7 <- predict(s2_logit10_7, newdata = my_test_10_7_2, type = "response")
s2_probit10_7 <- glm(default ~., family = binomial(link = probit), data = my_train_10_7_2) 
s2_p_probit10_7 <- predict(s2_probit10_7, newdata = my_test_10_7_2, type = "response")
s2_clog10_7 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_7_2) 
s2_p_clog10_7 <- predict(s2_clog10_7, newdata = my_test_10_7_2, type = "response")


#third set
s3_logit10_7 <- glm(default ~., family = binomial, data = my_train_10_7_3) 
s3_p_logit10_7 <- predict(s3_logit10_7, newdata = my_test_10_7_3, type = "response")
s3_probit10_7 <- glm(default ~., family = binomial(link = probit), data = my_train_10_7_3) 
s3_p_probit10_7 <- predict(s3_probit10_7, newdata = my_test_10_7_3, type = "response")
s3_clog10_7 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_7_3) 
s3_p_clog10_7 <- predict(s3_clog10_7, newdata = my_test_10_7_3, type = "response")


#evaluating the results
s1_e_logit10_7 <- round(eval(my_test_10_7_1$default, s1_p_logit10_7), digits = 6)
s1_e_probit10_7 <- round(eval(my_test_10_7_1$default, s1_p_probit10_7), digits = 6)
s1_e_clog10_7 <- round(eval(my_test_10_7_1$default, s1_p_clog10_7), digits = 6)
s2_e_logit10_7 <-round(eval(my_test_10_7_2$default, s2_p_logit10_7), digits = 6)
s2_e_probit10_7 <- round(eval(my_test_10_7_2$default, s2_p_probit10_7), digits = 6)
s2_e_clog10_7 <- round(eval(my_test_10_7_2$default, s2_p_clog10_7), digits = 6)
s3_e_logit10_7 <- round(eval(my_test_10_7_3$default, s3_p_logit10_7), digits = 6)
s3_e_probit10_7 <- round(eval(my_test_10_7_3$default, s3_p_probit10_7), digits = 6)
s3_e_clog10_7 <- round(eval(my_test_10_7_3$default, s3_p_clog10_7), digits = 6)

mean_clog10_7 <- round(mean(c(s1_e_clog10_7, s2_e_clog10_7, s3_e_clog10_7)), digits = 6)

max_s1_10_7 <- c('clog', 'logit', 'probit')[which.max(c(s1_e_clog10_7, s1_e_logit10_7, s1_e_probit10_7))] 
max_s2_10_7 <- c('clog', 'logit', 'probit')[which.max(c(s2_e_clog10_7, s2_e_logit10_7, s2_e_probit10_7))]
max_s3_10_7 <- c('clog', 'logit', 'probit')[which.max(c(s3_e_clog10_7, s3_e_logit10_7, s3_e_probit10_7))]

#confusion matrix 
pred_clog10_7 <- ifelse(s1_p_clog10_7 > 0.5, TRUE, FALSE) 
cm_clog10_7 <- table(my_test_10_7_1$default, pred_clog10_7)
mat_clog10_7 <- confusionMatrix(cm_clog10_7)

#ROC-curve
ROC_clog10_7 <- roc(my_test_10_7_1$default, s1_p_clog10_7) 


#appending results to Dataframe
all_10_7 <- c('w/o ann_inc', s1_e_logit10_7, s1_e_probit10_7, s1_e_clog10_7, s2_e_logit10_7, s2_e_probit10_7, s2_e_clog10_7, s3_e_logit10_7, s3_e_probit10_7,
              s3_e_clog10_7, max_s1_10_7, max_s2_10_7, max_s3_10_7, mean_clog10_7, round(mat_clog10_7$overall['Accuracy'], digits = 6), round(auc(ROC_clog10_7), digits = 6))

find_model <- cbind(find_model, all_10_7)



#######################
#leaving out 'purpose'#
#######################
my_train_10_8_1 <- my_train_11_1 %>% select(-purpose) 
my_test_10_8_1 <- my_test_11_1 %>% select(-purpose) 

my_train_10_8_2 <- my_train_11_2 %>% select(-purpose) 
my_test_10_8_2 <- my_test_11_2 %>% select(-purpose)

my_train_10_8_3 <- my_train_11_3 %>% select(-purpose) 
my_test_10_8_3 <- my_test_11_2 %>% select(-purpose)

#calculation and evaluation is the same as for the 11 variables. Therefore I have left out the comments
#first set
s1_logit10_8 <- glm(default ~., family = binomial, data = my_train_10_8_1) 
s1_p_logit10_8 <- predict(s1_logit10_8, newdata = my_test_10_8_1, type = "response")
s1_probit10_8 <- glm(default ~., family = binomial(link = probit), data = my_train_10_8_1) 
s1_p_probit10_8 <- predict(s1_probit10_8, newdata = my_test_10_8_1, type = "response")
s1_clog10_8 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_8_1) 
s1_p_clog10_8 <- predict(s1_clog10_8, newdata = my_test_10_8_1, type = "response")


#second set
s2_logit10_8 <- glm(default ~., family = binomial, data = my_train_10_8_2) 
s2_p_logit10_8 <- predict(s2_logit10_8, newdata = my_test_10_8_2, type = "response")
s2_probit10_8 <- glm(default ~., family = binomial(link = probit), data = my_train_10_8_2) 
s2_p_probit10_8 <- predict(s2_probit10_8, newdata = my_test_10_8_2, type = "response")
s2_clog10_8 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_8_2) 
s2_p_clog10_8 <- predict(s2_clog10_8, newdata = my_test_10_8_2, type = "response")


#third set
s3_logit10_8 <- glm(default ~., family = binomial, data = my_train_10_8_3) 
s3_p_logit10_8 <- predict(s3_logit10_8, newdata = my_test_10_8_3, type = "response")
s3_probit10_8 <- glm(default ~., family = binomial(link = probit), data = my_train_10_8_3) 
s3_p_probit10_8 <- predict(s3_probit10_8, newdata = my_test_10_8_3, type = "response")
s3_clog10_8 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_8_3) 
s3_p_clog10_8 <- predict(s3_clog10_8, newdata = my_test_10_8_3, type = "response")


#evaluating the results
s1_e_logit10_8 <- round(eval(my_test_10_8_1$default, s1_p_logit10_8), digits = 6)
s1_e_probit10_8 <- round(eval(my_test_10_8_1$default, s1_p_probit10_8), digits = 6)
s1_e_clog10_8 <- round(eval(my_test_10_8_1$default, s1_p_clog10_8), digits = 6)
s2_e_logit10_8 <-round(eval(my_test_10_8_2$default, s2_p_logit10_8), digits = 6)
s2_e_probit10_8 <- round(eval(my_test_10_8_2$default, s2_p_probit10_8), digits = 6)
s2_e_clog10_8 <- round(eval(my_test_10_8_2$default, s2_p_clog10_8), digits = 6)
s3_e_logit10_8 <- round(eval(my_test_10_8_3$default, s3_p_logit10_8), digits = 6)
s3_e_probit10_8 <- round(eval(my_test_10_8_3$default, s3_p_probit10_8), digits = 6)
s3_e_clog10_8 <- round(eval(my_test_10_8_3$default, s3_p_clog10_8), digits = 6)

mean_clog10_8 <- round(mean(c(s1_e_clog10_8, s2_e_clog10_8, s3_e_clog10_8)), digits = 6)

max_s1_10_8 <- c('clog', 'logit', 'probit')[which.max(c(s1_e_clog10_8, s1_e_logit10_8, s1_e_probit10_8))] 
max_s2_10_8 <- c('clog', 'logit', 'probit')[which.max(c(s2_e_clog10_8, s2_e_logit10_8, s2_e_probit10_8))]
max_s3_10_8 <- c('clog', 'logit', 'probit')[which.max(c(s3_e_clog10_8, s3_e_logit10_8, s3_e_probit10_8))]

#confusion matrix 
pred_clog10_8 <- ifelse(s1_p_clog10_8 > 0.5, TRUE, FALSE) 
cm_clog10_8 <- table(my_test_10_8_1$default, pred_clog10_8)
mat_clog10_8 <- confusionMatrix(cm_clog10_8)

#ROC-curve
ROC_clog10_8 <- roc(my_test_10_8_1$default, s1_p_clog10_8) 


#appending results to Dataframe
all_10_8 <- c('w/o purpose', s1_e_logit10_8, s1_e_probit10_8, s1_e_clog10_8, s2_e_logit10_8, s2_e_probit10_8, s2_e_clog10_8, s3_e_logit10_8, s3_e_probit10_8,
              s3_e_clog10_8, max_s1_10_8, max_s2_10_8, max_s3_10_8, mean_clog10_8, round(mat_clog10_8$overall['Accuracy'], digits = 6), round(auc(ROC_clog10_8), digits = 6))

find_model <- cbind(find_model, all_10_8)




####################
#leaving out 'dti'#
####################
my_train_10_9_1 <- my_train_11_1 %>% select(-dti) 
my_test_10_9_1 <- my_test_11_1 %>% select(-dti) 

my_train_10_9_2 <- my_train_11_2 %>% select(-dti) 
my_test_10_9_2 <- my_test_11_2 %>% select(-dti)

my_train_10_9_3 <- my_train_11_3 %>% select(-dti) 
my_test_10_9_3 <- my_test_11_2 %>% select(-dti)


#first set
s1_logit10_9 <- glm(default ~., family = binomial, data = my_train_10_9_1) 
s1_p_logit10_9 <- predict(s1_logit10_9, newdata = my_test_10_9_1, type = "response")
s1_probit10_9 <- glm(default ~., family = binomial(link = probit), data = my_train_10_9_1) 
s1_p_probit10_9 <- predict(s1_probit10_9, newdata = my_test_10_9_1, type = "response")
s1_clog10_9 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_9_1) 
s1_p_clog10_9 <- predict(s1_clog10_9, newdata = my_test_10_9_1, type = "response")


#second set
s2_logit10_9 <- glm(default ~., family = binomial, data = my_train_10_9_2) 
s2_p_logit10_9 <- predict(s2_logit10_9, newdata = my_test_10_9_2, type = "response")
s2_probit10_9 <- glm(default ~., family = binomial(link = probit), data = my_train_10_9_2) 
s2_p_probit10_9 <- predict(s2_probit10_9, newdata = my_test_10_9_2, type = "response")
s2_clog10_9 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_9_2) 
s2_p_clog10_9 <- predict(s2_clog10_9, newdata = my_test_10_9_2, type = "response")


#third set
s3_logit10_9 <- glm(default ~., family = binomial, data = my_train_10_9_3) 
s3_p_logit10_9 <- predict(s3_logit10_9, newdata = my_test_10_9_3, type = "response")
s3_probit10_9 <- glm(default ~., family = binomial(link = probit), data = my_train_10_9_3) 
s3_p_probit10_9 <- predict(s3_probit10_9, newdata = my_test_10_9_3, type = "response")
s3_clog10_9 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_9_3) 
s3_p_clog10_9 <- predict(s3_clog10_9, newdata = my_test_10_9_3, type = "response")


#evaluating the results
s1_e_logit10_9 <- round(eval(my_test_10_9_1$default, s1_p_logit10_9), digits = 6)
s1_e_probit10_9 <- round(eval(my_test_10_9_1$default, s1_p_probit10_9), digits = 6)
s1_e_clog10_9 <- round(eval(my_test_10_9_1$default, s1_p_clog10_9), digits = 6)
s2_e_logit10_9 <-round(eval(my_test_10_9_2$default, s2_p_logit10_9), digits = 6)
s2_e_probit10_9 <- round(eval(my_test_10_9_2$default, s2_p_probit10_9), digits = 6)
s2_e_clog10_9 <- round(eval(my_test_10_9_2$default, s2_p_clog10_9), digits = 6)
s3_e_logit10_9 <- round(eval(my_test_10_9_3$default, s3_p_logit10_9), digits = 6)
s3_e_probit10_9 <- round(eval(my_test_10_9_3$default, s3_p_probit10_9), digits = 6)
s3_e_clog10_9 <- round(eval(my_test_10_9_3$default, s3_p_clog10_9), digits = 6)

mean_clog10_9 <- round(mean(c(s1_e_clog10_9, s2_e_clog10_9, s3_e_clog10_9)), digits = 6)

max_s1_10_9 <- c('clog', 'logit', 'probit')[which.max(c(s1_e_clog10_9, s1_e_logit10_9, s1_e_probit10_9))] 
max_s2_10_9 <- c('clog', 'logit', 'probit')[which.max(c(s2_e_clog10_9, s2_e_logit10_9, s2_e_probit10_9))]
max_s3_10_9 <- c('clog', 'logit', 'probit')[which.max(c(s3_e_clog10_9, s3_e_logit10_9, s3_e_probit10_9))]

#confusion matrix 
pred_clog10_9 <- ifelse(s1_p_clog10_9 > 0.5, TRUE, FALSE) 
cm_clog10_9 <- table(my_test_10_9_1$default, pred_clog10_9)
mat_clog10_9 <- confusionMatrix(cm_clog10_9)

#ROC-curve
ROC_clog10_9 <- roc(my_test_10_9_1$default, s1_p_clog10_9) 


#appending results to Dataframe
all_10_9 <- c('w/o dti', s1_e_logit10_9, s1_e_probit10_9, s1_e_clog10_9, s2_e_logit10_9, s2_e_probit10_9, s2_e_clog10_9, s3_e_logit10_9, s3_e_probit10_9,
              s3_e_clog10_9, max_s1_10_9, max_s2_10_9, max_s3_10_9, mean_clog10_9, round(mat_clog10_9$overall['Accuracy'], digits = 6), round(auc(ROC_clog10_9), digits = 6))

find_model <- cbind(find_model, all_10_9)




########################
#leaving out 'open_acc'#
########################
my_train_10_10_1 <- my_train_11_1 %>% select(-open_acc) 
my_test_10_10_1 <- my_test_11_1 %>% select(-open_acc) 

my_train_10_10_2 <- my_train_11_2 %>% select(-open_acc) 
my_test_10_10_2 <- my_test_11_2 %>% select(-open_acc)

my_train_10_10_3 <- my_train_11_3 %>% select(-open_acc) 
my_test_10_10_3 <- my_test_11_2 %>% select(-open_acc)

#calculation and evaluation is the same as for the 11 variables. Therefore I have left out the comments
#first set
s1_logit10_10 <- glm(default ~., family = binomial, data = my_train_10_10_1) 
s1_p_logit10_10 <- predict(s1_logit10_10, newdata = my_test_10_10_1, type = "response")
s1_probit10_10 <- glm(default ~., family = binomial(link = probit), data = my_train_10_10_1) 
s1_p_probit10_10 <- predict(s1_probit10_10, newdata = my_test_10_10_1, type = "response")
s1_clog10_10 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_10_1) 
s1_p_clog10_10 <- predict(s1_clog10_10, newdata = my_test_10_10_1, type = "response")


#second set
s2_logit10_10 <- glm(default ~., family = binomial, data = my_train_10_10_2) 
s2_p_logit10_10 <- predict(s2_logit10_10, newdata = my_test_10_10_2, type = "response")
s2_probit10_10 <- glm(default ~., family = binomial(link = probit), data = my_train_10_10_2) 
s2_p_probit10_10 <- predict(s2_probit10_10, newdata = my_test_10_10_2, type = "response")
s2_clog10_10 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_10_2) 
s2_p_clog10_10 <- predict(s2_clog10_10, newdata = my_test_10_10_2, type = "response")


#third set
s3_logit10_10 <- glm(default ~., family = binomial, data = my_train_10_10_3) 
s3_p_logit10_10 <- predict(s3_logit10_10, newdata = my_test_10_10_3, type = "response")
s3_probit10_10 <- glm(default ~., family = binomial(link = probit), data = my_train_10_10_3) 
s3_p_probit10_10 <- predict(s3_probit10_10, newdata = my_test_10_10_3, type = "response")
s3_clog10_10 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_10_3) 
s3_p_clog10_10 <- predict(s3_clog10_10, newdata = my_test_10_10_3, type = "response")


#evaluating the results
s1_e_logit10_10 <- round(eval(my_test_10_10_1$default, s1_p_logit10_10), digits = 6)
s1_e_probit10_10 <- round(eval(my_test_10_10_1$default, s1_p_probit10_10), digits = 6)
s1_e_clog10_10 <- round(eval(my_test_10_10_1$default, s1_p_clog10_10), digits = 6)
s2_e_logit10_10 <-round(eval(my_test_10_10_2$default, s2_p_logit10_10), digits = 6)
s2_e_probit10_10 <- round(eval(my_test_10_10_2$default, s2_p_probit10_10), digits = 6)
s2_e_clog10_10 <- round(eval(my_test_10_10_2$default, s2_p_clog10_10), digits = 6)
s3_e_logit10_10 <- round(eval(my_test_10_10_3$default, s3_p_logit10_10), digits = 6)
s3_e_probit10_10 <- round(eval(my_test_10_10_3$default, s3_p_probit10_10), digits = 6)
s3_e_clog10_10 <- round(eval(my_test_10_10_3$default, s3_p_clog10_10), digits = 6)

mean_clog10_10 <- round(mean(c(s1_e_clog10_10, s2_e_clog10_10, s3_e_clog10_10)), digits = 6)

max_s1_10_10 <- c('clog', 'logit', 'probit')[which.max(c(s1_e_clog10_10, s1_e_logit10_10, s1_e_probit10_10))] 
max_s2_10_10 <- c('clog', 'logit', 'probit')[which.max(c(s2_e_clog10_10, s2_e_logit10_10, s2_e_probit10_10))]
max_s3_10_10 <- c('clog', 'logit', 'probit')[which.max(c(s3_e_clog10_10, s3_e_logit10_10, s3_e_probit10_10))]

#confusion matrix 
pred_clog10_10 <- ifelse(s1_p_clog10_10 > 0.5, TRUE, FALSE) 
cm_clog10_10 <- table(my_test_10_10_1$default, pred_clog10_10)
mat_clog10_10 <- confusionMatrix(cm_clog10_10)

#ROC-curve
ROC_clog10_10 <- roc(my_test_10_10_1$default, s1_p_clog10_10) 


#appending results to Dataframe
all_10_10 <- c('w/o open_acc', s1_e_logit10_10, s1_e_probit10_10, s1_e_clog10_10, s2_e_logit10_10, s2_e_probit10_10, s2_e_clog10_10, s3_e_logit10_10, s3_e_probit10_10,
               s3_e_clog10_10, max_s1_10_10, max_s2_10_10, max_s3_10_10, mean_clog10_10, round(mat_clog10_10$overall['Accuracy'], digits = 6), round(auc(ROC_clog10_10), digits = 6))

find_model <- cbind(find_model, all_10_10)





#########################
#leaving out 'total_acc'#
#########################
my_train_10_11_1 <- my_train_11_1 %>% select(-total_acc) 
my_test_10_11_1 <- my_test_11_1 %>% select(-total_acc) 

my_train_10_11_2 <- my_train_11_2 %>% select(-total_acc) 
my_test_10_11_2 <- my_test_11_2 %>% select(-total_acc)

my_train_10_11_3 <- my_train_11_3 %>% select(-total_acc) 
my_test_10_11_3 <- my_test_11_2 %>% select(-total_acc)

#calculation and evaluation is the same as for the 11 variables. Therefore I have left out the comments
#first set
s1_logit10_11 <- glm(default ~., family = binomial, data = my_train_10_11_1) 
s1_p_logit10_11 <- predict(s1_logit10_11, newdata = my_test_10_11_1, type = "response")
s1_probit10_11 <- glm(default ~., family = binomial(link = probit), data = my_train_10_11_1) 
s1_p_probit10_11 <- predict(s1_probit10_11, newdata = my_test_10_11_1, type = "response")
s1_clog10_11 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_11_1) 
s1_p_clog10_11 <- predict(s1_clog10_11, newdata = my_test_10_11_1, type = "response")


#second set
s2_logit10_11 <- glm(default ~., family = binomial, data = my_train_10_11_2) 
s2_p_logit10_11 <- predict(s2_logit10_11, newdata = my_test_10_11_2, type = "response")
s2_probit10_11 <- glm(default ~., family = binomial(link = probit), data = my_train_10_11_2) 
s2_p_probit10_11 <- predict(s2_probit10_11, newdata = my_test_10_11_2, type = "response")
s2_clog10_11 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_11_2) 
s2_p_clog10_11 <- predict(s2_clog10_11, newdata = my_test_10_11_2, type = "response")


#third set
s3_logit10_11 <- glm(default ~., family = binomial, data = my_train_10_11_3) 
s3_p_logit10_11 <- predict(s3_logit10_11, newdata = my_test_10_11_3, type = "response")
s3_probit10_11 <- glm(default ~., family = binomial(link = probit), data = my_train_10_11_3) 
s3_p_probit10_11 <- predict(s3_probit10_11, newdata = my_test_10_11_3, type = "response")
s3_clog10_11 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_11_3) 
s3_p_clog10_11 <- predict(s3_clog10_11, newdata = my_test_10_11_3, type = "response")


#evaluating the results
s1_e_logit10_11 <- round(eval(my_test_10_11_1$default, s1_p_logit10_11), digits = 6)
s1_e_probit10_11 <- round(eval(my_test_10_11_1$default, s1_p_probit10_11), digits = 6)
s1_e_clog10_11 <- round(eval(my_test_10_11_1$default, s1_p_clog10_11), digits = 6)
s2_e_logit10_11 <-round(eval(my_test_10_11_2$default, s2_p_logit10_11), digits = 6)
s2_e_probit10_11 <- round(eval(my_test_10_11_2$default, s2_p_probit10_11), digits = 6)
s2_e_clog10_11 <- round(eval(my_test_10_11_2$default, s2_p_clog10_11), digits = 6)
s3_e_logit10_11 <- round(eval(my_test_10_11_3$default, s3_p_logit10_11), digits = 6)
s3_e_probit10_11 <- round(eval(my_test_10_11_3$default, s3_p_probit10_11), digits = 6)
s3_e_clog10_11 <- round(eval(my_test_10_11_3$default, s3_p_clog10_11), digits = 6)

mean_clog10_11 <- round(mean(c(s1_e_clog10_11, s2_e_clog10_11, s3_e_clog10_11)), digits = 6)

max_s1_10_11 <- c('clog', 'logit', 'probit')[which.max(c(s1_e_clog10_11, s1_e_logit10_11, s1_e_probit10_11))] 
max_s2_10_11 <- c('clog', 'logit', 'probit')[which.max(c(s2_e_clog10_11, s2_e_logit10_11, s2_e_probit10_11))]
max_s3_10_11 <- c('clog', 'logit', 'probit')[which.max(c(s3_e_clog10_11, s3_e_logit10_11, s3_e_probit10_11))]

#confusion matrix 
pred_clog10_11 <- ifelse(s1_p_clog10_11 > 0.5, TRUE, FALSE) 
cm_clog10_11 <- table(my_test_10_11_1$default, pred_clog10_11)
mat_clog10_11 <- confusionMatrix(cm_clog10_11)

#ROC-curve
ROC_clog10_11 <- roc(my_test_10_11_1$default, s1_p_clog10_11) 


#appending results to Dataframe
all_10_11 <- c('w/o total_acc', s1_e_logit10_11, s1_e_probit10_11, s1_e_clog10_11, s2_e_logit10_11, s2_e_probit10_11, s2_e_clog10_11, s3_e_logit10_11, s3_e_probit10_11,
               s3_e_clog10_11, max_s1_10_11, max_s2_10_11, max_s3_10_11, mean_clog10_11, round(mat_clog10_11$overall['Accuracy'], digits = 6), round(auc(ROC_clog10_11), digits = 6))

find_model <- cbind(find_model, all_10_11)

write.xlsx(find_model, file = "find_model.xlsx", row.names = F)


###############################################################
#             calculating test data                           #
###############################################################

#preparing test data. home_ownership has new level 'ANY' -> assigning mode
dat_test <- dat_test %>% mutate(home_ownership = ifelse(home_ownership == 'ANY', Mode(home_ownership), home_ownership))

final <- glm(default ~., family = binomial(link = cloglog), data = dat_train)
summary(final)
pred_prob$pred_prob <- predict(final, newdata = dat_test, type = 'response')

write.csv(pred_prob, file = "predictions_3308460.csv", row.names = F)

