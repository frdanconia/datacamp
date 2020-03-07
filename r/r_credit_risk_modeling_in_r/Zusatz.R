# Create training set: training_set
my_train_10_1 <- my_train_10_11_1 
# Create test set: test_set
my_test_10_1 <- my_test_10_11_1

#Own Train and Test Set 2 
# Create training set: training_set
my_train_10_2 <- my_train_10_11_2 
# Create test set: test_set
my_test_10_2 <- my_test_10_11_2

#Own Train and Test Set 3
# Create training set: training_set
my_train_10_3 <- my_train_10_11_3
# Create test set: test_set
my_test_10_3 <- my_test_10_11_3



#All 10 Variables: 

#first set
s1_logit10 <- glm(default ~., family = binomial, data = my_train_10_1) 
s1_p_logit10 <- predict(s1_logit10, newdata = my_test_10_1, type = "response")
s1_probit10 <- glm(default ~., family = binomial(link = probit), data = my_train_10_1) 
s1_p_probit10 <- predict(s1_probit10, newdata = my_test_10_1, type = "response")
s1_clog10 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_1) 
s1_p_clog10 <- predict(s1_clog10, newdata = my_test_10_1, type = "response")


#second set
s2_logit10 <- glm(default ~., family = binomial, data = my_train_10_2) 
s2_p_logit10 <- predict(s2_logit10, newdata = my_test_10_2, type = "response")
s2_probit10 <- glm(default ~., family = binomial(link = probit), data = my_train_10_2) 
s2_p_probit10 <- predict(s2_probit10, newdata = my_test_10_2, type = "response")
s2_clog10 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_2) 
s2_p_clog10 <- predict(s2_clog10, newdata = my_test_10_2, type = "response")


#third set
s3_logit10 <- glm(default ~., family = binomial, data = my_train_10_3) 
s3_p_logit10 <- predict(s3_logit10, newdata = my_test_10_3, type = "response")
s3_probit10 <- glm(default ~., family = binomial(link = probit), data = my_train_10_3) 
s3_p_probit10 <- predict(s3_probit10, newdata = my_test_10_3, type = "response")
s3_clog10 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_3) 
s3_p_clog10 <- predict(s3_clog10, newdata = my_test_10_3, type = "response")


#evaluating the results
s1_e_logit10 <- eval(my_test_10_1$default, s1_p_logit10)
s1_e_probit10 <- eval(my_test_10_1$default, s1_p_probit10)
s1_e_clog10 <- eval(my_test_10_1$default, s1_p_clog10)
s2_e_logit10 <-eval(my_test_10_2$default, s2_p_logit10)
s2_e_probit10 <- eval(my_test_10_2$default, s2_p_probit10)
s2_e_clog10 <- eval(my_test_10_2$default, s2_p_clog10)
s3_e_logit10 <- eval(my_test_10_3$default, s3_p_logit10)
s3_e_probit10 <- eval(my_test_10_3$default, s3_p_probit10)
s3_e_clog10 <- eval(my_test_10_3$default, s3_p_clog10)

mean_clog10 <- mean(c(s1_e_clog10, s2_e_clog10, s3_e_clog10))

min_s1_10 <- c('clog', 'logit', 'probit')[which.max(c(s1_e_clog10, s1_e_logit10, s1_e_probit10))] 
min_s2_10 <- c('clog', 'logit', 'probit')[which.max(c(s2_e_clog10, s2_e_logit10, s2_e_probit10))]
min_s3_10 <- c('clog', 'logit', 'probit')[which.max(c(s3_e_clog10, s3_e_logit10, s3_e_probit10))]

#confusion matrices 
pred_clog10 <- ifelse(s1_p_clog10 > 0.5, TRUE, FALSE) 
cm_clog10 <- table(my_test_10_1$default, pred_clog10)
mat_clog10 <- confusionMatrix(cm_clog10)

#ROC-curces
ROC_clog10 <- roc(my_test_10_1$default, s1_p_clog10) 


#creating dataframe with results 
model <- c('','Set 1: logit', 'Set 1: probit', 'Set 1: clog', 'Set 2: logit', 'Set 2: probit', 'Set 2: clog', 'Set 3: logit', 'Set3: probit',
           'Set3: clog','Maximum Set 1', 'Maximum Set 2', 'Maximum Set 3', 'mean: clog', 'Accuracy: clog', 'AUC: clog')
all_10 <- c('All 10 Var.', s1_e_logit10, s1_e_probit10, s1_e_clog10, s2_e_logit10, s2_e_probit10, s2_e_clog10, s3_e_logit10, s3_e_probit10,
            s3_e_clog10, min_s1_10, min_s2_10, min_s3_10, mean_clog10, mat_clog10$overall['Accuracy'], auc(ROC_clog10))

find_model1 <- data.frame(model, all_10)

#All 10 Variables: 

#first set
s1_logit10 <- glm(default ~., family = binomial, data = my_train_10_1) 
s1_p_logit10 <- predict(s1_logit10, newdata = my_test_10_1, type = "response")
s1_probit10 <- glm(default ~., family = binomial(link = probit), data = my_train_10_1) 
s1_p_probit10 <- predict(s1_probit10, newdata = my_test_10_1, type = "response")
s1_clog10 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_1) 
s1_p_clog10 <- predict(s1_clog10, newdata = my_test_10_1, type = "response")


#second set
s2_logit10 <- glm(default ~., family = binomial, data = my_train_10_2) 
s2_p_logit10 <- predict(s2_logit10, newdata = my_test_10_2, type = "response")
s2_probit10 <- glm(default ~., family = binomial(link = probit), data = my_train_10_2) 
s2_p_probit10 <- predict(s2_probit10, newdata = my_test_10_2, type = "response")
s2_clog10 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_2) 
s2_p_clog10 <- predict(s2_clog10, newdata = my_test_10_2, type = "response")


#third set
s3_logit10 <- glm(default ~., family = binomial, data = my_train_10_3) 
s3_p_logit10 <- predict(s3_logit10, newdata = my_test_10_3, type = "response")
s3_probit10 <- glm(default ~., family = binomial(link = probit), data = my_train_10_3) 
s3_p_probit10 <- predict(s3_probit10, newdata = my_test_10_3, type = "response")
s3_clog10 <- glm(default ~., family = binomial(link = cloglog), data = my_train_10_3) 
s3_p_clog10 <- predict(s3_clog10, newdata = my_test_10_3, type = "response")


#evaluating the results
s1_e_logit10 <- eval(my_test_10_1$default, s1_p_logit10)
s1_e_probit10 <- eval(my_test_10_1$default, s1_p_probit10)
s1_e_clog10 <- eval(my_test_10_1$default, s1_p_clog10)
s2_e_logit10 <-eval(my_test_10_2$default, s2_p_logit10)
s2_e_probit10 <- eval(my_test_10_2$default, s2_p_probit10)
s2_e_clog10 <- eval(my_test_10_2$default, s2_p_clog10)
s3_e_logit10 <- eval(my_test_10_3$default, s3_p_logit10)
s3_e_probit10 <- eval(my_test_10_3$default, s3_p_probit10)
s3_e_clog10 <- eval(my_test_10_3$default, s3_p_clog10)

mean_clog10 <- mean(c(s1_e_clog10, s2_e_clog10, s3_e_clog10))

min_s1_10 <- c('clog', 'logit', 'probit')[which.max(c(s1_e_clog10, s1_e_logit10, s1_e_probit10))] 
min_s2_10 <- c('clog', 'logit', 'probit')[which.max(c(s2_e_clog10, s2_e_logit10, s2_e_probit10))]
min_s3_10 <- c('clog', 'logit', 'probit')[which.max(c(s3_e_clog10, s3_e_logit10, s3_e_probit10))]

#confusion matrices 
pred_clog10 <- ifelse(s1_p_clog10 > 0.5, TRUE, FALSE) 
cm_clog10 <- table(my_test_10_1$default, pred_clog10)
mat_clog10 <- confusionMatrix(cm_clog10)

#ROC-curces
ROC_clog10 <- roc(my_test_10_1$default, s1_p_clog10) 


#creating dataframe with results 
model <- c('','Set 1: logit', 'Set 1: probit', 'Set 1: clog', 'Set 2: logit', 'Set 2: probit', 'Set 2: clog', 'Set 3: logit', 'Set3: probit',
           'Set3: clog','Maximum Set 1', 'Maximum Set 2', 'Maximum Set 3', 'mean: clog', 'Accuracy: clog', 'AUC: clog')
all_10 <- c('All 10 Var.', s1_e_logit10, s1_e_probit10, s1_e_clog10, s2_e_logit10, s2_e_probit10, s2_e_clog10, s3_e_logit10, s3_e_probit10,
            s3_e_clog10, min_s1_10, min_s2_10, min_s3_10, mean_clog10, mat_clog10$overall['Accuracy'], auc(ROC_clog10))

find_model1 <- data.frame(model, all_10)



###############################################
#                 9 Variablen                #
###############################################

#Leave each of the 10 Variables out of the model one after another and compare the LL-values. 
#Find the highest LL-value of the coming models and then stick with that model. 
#########################
#Leaving out 'loan_amnt'#
#########################
my_train_9_1_1 <- my_train_10_1 %>% select(-loan_amnt) 
my_test_9_1_1 <- my_test_10_1 %>% select(-loan_amnt) 

my_train_9_1_2 <- my_train_10_2 %>% select(-loan_amnt) 
my_test_9_1_2 <- my_test_10_2 %>% select(-loan_amnt)

my_train_9_1_3 <- my_train_10_3 %>% select(-loan_amnt) 
my_test_9_1_3 <- my_test_10_2 %>% select(-loan_amnt)


#first set
s1_logit9_1 <- glm(default ~., family = binomial, data = my_train_9_1_1) 
s1_p_logit9_1 <- predict(s1_logit9_1, newdata = my_test_9_1_1, type = "response")
s1_probit9_1 <- glm(default ~., family = binomial(link = probit), data = my_train_9_1_1) 
s1_p_probit9_1 <- predict(s1_probit9_1, newdata = my_test_9_1_1, type = "response")
s1_clog9_1 <- glm(default ~., family = binomial(link = cloglog), data = my_train_9_1_1) 
s1_p_clog9_1 <- predict(s1_clog9_1, newdata = my_test_9_1_1, type = "response")


#second set
s2_logit9_1 <- glm(default ~., family = binomial, data = my_train_9_1_2) 
s2_p_logit9_1 <- predict(s2_logit9_1, newdata = my_test_9_1_2, type = "response")
s2_probit9_1 <- glm(default ~., family = binomial(link = probit), data = my_train_9_1_2) 
s2_p_probit9_1 <- predict(s2_probit9_1, newdata = my_test_9_1_2, type = "response")
s2_clog9_1 <- glm(default ~., family = binomial(link = cloglog), data = my_train_9_1_2) 
s2_p_clog9_1 <- predict(s2_clog9_1, newdata = my_test_9_1_2, type = "response")


#third set
s3_logit9_1 <- glm(default ~., family = binomial, data = my_train_9_1_3) 
s3_p_logit9_1 <- predict(s3_logit9_1, newdata = my_test_9_1_3, type = "response")
s3_probit9_1 <- glm(default ~., family = binomial(link = probit), data = my_train_9_1_3) 
s3_p_probit9_1 <- predict(s3_probit9_1, newdata = my_test_9_1_3, type = "response")
s3_clog9_1 <- glm(default ~., family = binomial(link = cloglog), data = my_train_9_1_3) 
s3_p_clog9_1 <- predict(s3_clog9_1, newdata = my_test_9_1_3, type = "response")


#evaluating the results
s1_e_logit9_1 <- eval(my_test_9_1_1$default, s1_p_logit9_1)
s1_e_probit9_1 <- eval(my_test_9_1_1$default, s1_p_probit9_1)
s1_e_clog9_1 <- eval(my_test_9_1_1$default, s1_p_clog9_1)
s2_e_logit9_1 <-eval(my_test_9_1_2$default, s2_p_logit9_1)
s2_e_probit9_1 <- eval(my_test_9_1_2$default, s2_p_probit9_1)
s2_e_clog9_1 <- eval(my_test_9_1_2$default, s2_p_clog9_1)
s3_e_logit9_1 <- eval(my_test_9_1_3$default, s3_p_logit9_1)
s3_e_probit9_1 <- eval(my_test_9_1_3$default, s3_p_probit9_1)
s3_e_clog9_1 <- eval(my_test_9_1_3$default, s3_p_clog9_1)

mean_clog9_1 <- mean(c(s1_e_clog9_1, s2_e_clog9_1, s3_e_clog9_1))

min_s1_9_1 <- c('clog', 'logit', 'probit')[which.max(c(s1_e_clog9_1, s1_e_logit9_1, s1_e_probit9_1))] 
min_s2_9_1 <- c('clog', 'logit', 'probit')[which.max(c(s2_e_clog9_1, s2_e_logit9_1, s2_e_probit9_1))]
min_s3_9_1 <- c('clog', 'logit', 'probit')[which.max(c(s3_e_clog9_1, s3_e_logit9_1, s3_e_probit9_1))]

#confusion matrices 
pred_clog9_1 <- ifelse(s1_p_clog9_1 > 0.5, TRUE, FALSE) 
cm_clog9_1 <- table(my_test_9_1_1$default, pred_clog9_1)
mat_clog9_1 <- confusionMatrix(cm_clog9_1)

#ROC-curces
ROC_clog9_1 <- roc(my_test_9_1_1$default, s1_p_clog9_1) 


#appending results to Dataframe
all_9_1 <- c('All 9_1 Var.', s1_e_logit9_1, s1_e_probit9_1, s1_e_clog9_1, s2_e_logit9_1, s2_e_probit9_1, s2_e_clog9_1, s3_e_logit9_1, s3_e_probit9_1,
              s3_e_clog9_1, min_s1_9_1, min_s2_9_1, min_s3_9_1, mean_clog9_1, mat_clog9_1$overall['Accuracy'], auc(ROC_clog9_1))

find_model1 <- cbind(find_model1, all_9_1) 

####################
#leaving out 'term'#
####################
my_train_9_2_1 <- my_train_10_1 %>% select(-term) 
my_test_9_2_1 <- my_test_10_1 %>% select(-term) 

my_train_9_2_2 <- my_train_10_2 %>% select(-term) 
my_test_9_2_2 <- my_test_10_2 %>% select(-term)

my_train_9_2_3 <- my_train_10_3 %>% select(-term) 
my_test_9_2_3 <- my_test_10_2 %>% select(-term)


#first set
s1_logit9_2 <- glm(default ~., family = binomial, data = my_train_9_2_1) 
s1_p_logit9_2 <- predict(s1_logit9_2, newdata = my_test_9_2_1, type = "response")
s1_probit9_2 <- glm(default ~., family = binomial(link = probit), data = my_train_9_2_1) 
s1_p_probit9_2 <- predict(s1_probit9_2, newdata = my_test_9_2_1, type = "response")
s1_clog9_2 <- glm(default ~., family = binomial(link = cloglog), data = my_train_9_2_1) 
s1_p_clog9_2 <- predict(s1_clog9_2, newdata = my_test_9_2_1, type = "response")


#second set
s2_logit9_2 <- glm(default ~., family = binomial, data = my_train_9_2_2) 
s2_p_logit9_2 <- predict(s2_logit9_2, newdata = my_test_9_2_2, type = "response")
s2_probit9_2 <- glm(default ~., family = binomial(link = probit), data = my_train_9_2_2) 
s2_p_probit9_2 <- predict(s2_probit9_2, newdata = my_test_9_2_2, type = "response")
s2_clog9_2 <- glm(default ~., family = binomial(link = cloglog), data = my_train_9_2_2) 
s2_p_clog9_2 <- predict(s2_clog9_2, newdata = my_test_9_2_2, type = "response")


#third set
s3_logit9_2 <- glm(default ~., family = binomial, data = my_train_9_2_3) 
s3_p_logit9_2 <- predict(s3_logit9_2, newdata = my_test_9_2_3, type = "response")
s3_probit9_2 <- glm(default ~., family = binomial(link = probit), data = my_train_9_2_3) 
s3_p_probit9_2 <- predict(s3_probit9_2, newdata = my_test_9_2_3, type = "response")
s3_clog9_2 <- glm(default ~., family = binomial(link = cloglog), data = my_train_9_2_3) 
s3_p_clog9_2 <- predict(s3_clog9_2, newdata = my_test_9_2_3, type = "response")


#evaluating the results
s1_e_logit9_2 <- eval(my_test_9_2_1$default, s1_p_logit9_2)
s1_e_probit9_2 <- eval(my_test_9_2_1$default, s1_p_probit9_2)
s1_e_clog9_2 <- eval(my_test_9_2_1$default, s1_p_clog9_2)
s2_e_logit9_2 <-eval(my_test_9_2_2$default, s2_p_logit9_2)
s2_e_probit9_2 <- eval(my_test_9_2_2$default, s2_p_probit9_2)
s2_e_clog9_2 <- eval(my_test_9_2_2$default, s2_p_clog9_2)
s3_e_logit9_2 <- eval(my_test_9_2_3$default, s3_p_logit9_2)
s3_e_probit9_2 <- eval(my_test_9_2_3$default, s3_p_probit9_2)
s3_e_clog9_2 <- eval(my_test_9_2_3$default, s3_p_clog9_2)

mean_clog9_2 <- mean(c(s1_e_clog9_2, s2_e_clog9_2, s3_e_clog9_2))

min_s1_9_2 <- c('clog', 'logit', 'probit')[which.max(c(s1_e_clog9_2, s1_e_logit9_2, s1_e_probit9_2))] 
min_s2_9_2 <- c('clog', 'logit', 'probit')[which.max(c(s2_e_clog9_2, s2_e_logit9_2, s2_e_probit9_2))]
min_s3_9_2 <- c('clog', 'logit', 'probit')[which.max(c(s3_e_clog9_2, s3_e_logit9_2, s3_e_probit9_2))]

#confusion matrices 
pred_clog9_2 <- ifelse(s1_p_clog9_2 > 0.5, TRUE, FALSE) 
cm_clog9_2 <- table(my_test_9_2_1$default, pred_clog9_2)
mat_clog9_2 <- confusionMatrix(cm_clog9_2)

#ROC-curces
ROC_clog9_2 <- roc(my_test_9_2_1$default, s1_p_clog9_2) 


#appending results to Dataframe
all_9_2 <- c('All 9_2 Var.', s1_e_logit9_2, s1_e_probit9_2, s1_e_clog9_2, s2_e_logit9_2, s2_e_probit9_2, s2_e_clog9_2, s3_e_logit9_2, s3_e_probit9_2,
              s3_e_clog9_2, min_s1_9_2, min_s2_9_2, min_s3_9_2, mean_clog9_2, mat_clog9_2$overall['Accuracy'], auc(ROC_clog9_2))

find_model1 <- cbind(find_model1, all_9_2) 


########################
#leaving out 'int_rate'#
########################
my_train_9_3_1 <- my_train_10_1 %>% select(-int_rate) 
my_test_9_3_1 <- my_test_10_1 %>% select(-int_rate) 

my_train_9_3_2 <- my_train_10_2 %>% select(-int_rate) 
my_test_9_3_2 <- my_test_10_2 %>% select(-int_rate)

my_train_9_3_3 <- my_train_10_3 %>% select(-int_rate) 
my_test_9_3_3 <- my_test_10_2 %>% select(-int_rate)


#first set
s1_logit9_3 <- glm(default ~., family = binomial, data = my_train_9_3_1) 
s1_p_logit9_3 <- predict(s1_logit9_3, newdata = my_test_9_3_1, type = "response")
s1_probit9_3 <- glm(default ~., family = binomial(link = probit), data = my_train_9_3_1) 
s1_p_probit9_3 <- predict(s1_probit9_3, newdata = my_test_9_3_1, type = "response")
s1_clog9_3 <- glm(default ~., family = binomial(link = cloglog), data = my_train_9_3_1) 
s1_p_clog9_3 <- predict(s1_clog9_3, newdata = my_test_9_3_1, type = "response")


#second set
s2_logit9_3 <- glm(default ~., family = binomial, data = my_train_9_3_2) 
s2_p_logit9_3 <- predict(s2_logit9_3, newdata = my_test_9_3_2, type = "response")
s2_probit9_3 <- glm(default ~., family = binomial(link = probit), data = my_train_9_3_2) 
s2_p_probit9_3 <- predict(s2_probit9_3, newdata = my_test_9_3_2, type = "response")
s2_clog9_3 <- glm(default ~., family = binomial(link = cloglog), data = my_train_9_3_2) 
s2_p_clog9_3 <- predict(s2_clog9_3, newdata = my_test_9_3_2, type = "response")


#third set
s3_logit9_3 <- glm(default ~., family = binomial, data = my_train_9_3_3) 
s3_p_logit9_3 <- predict(s3_logit9_3, newdata = my_test_9_3_3, type = "response")
s3_probit9_3 <- glm(default ~., family = binomial(link = probit), data = my_train_9_3_3) 
s3_p_probit9_3 <- predict(s3_probit9_3, newdata = my_test_9_3_3, type = "response")
s3_clog9_3 <- glm(default ~., family = binomial(link = cloglog), data = my_train_9_3_3) 
s3_p_clog9_3 <- predict(s3_clog9_3, newdata = my_test_9_3_3, type = "response")


#evaluating the results
s1_e_logit9_3 <- eval(my_test_9_3_1$default, s1_p_logit9_3)
s1_e_probit9_3 <- eval(my_test_9_3_1$default, s1_p_probit9_3)
s1_e_clog9_3 <- eval(my_test_9_3_1$default, s1_p_clog9_3)
s2_e_logit9_3 <-eval(my_test_9_3_2$default, s2_p_logit9_3)
s2_e_probit9_3 <- eval(my_test_9_3_2$default, s2_p_probit9_3)
s2_e_clog9_3 <- eval(my_test_9_3_2$default, s2_p_clog9_3)
s3_e_logit9_3 <- eval(my_test_9_3_3$default, s3_p_logit9_3)
s3_e_probit9_3 <- eval(my_test_9_3_3$default, s3_p_probit9_3)
s3_e_clog9_3 <- eval(my_test_9_3_3$default, s3_p_clog9_3)

mean_clog9_3 <- mean(c(s1_e_clog9_3, s2_e_clog9_3, s3_e_clog9_3))

min_s1_9_3 <- c('clog', 'logit', 'probit')[which.max(c(s1_e_clog9_3, s1_e_logit9_3, s1_e_probit9_3))] 
min_s2_9_3 <- c('clog', 'logit', 'probit')[which.max(c(s2_e_clog9_3, s2_e_logit9_3, s2_e_probit9_3))]
min_s3_9_3 <- c('clog', 'logit', 'probit')[which.max(c(s3_e_clog9_3, s3_e_logit9_3, s3_e_probit9_3))]

#confusion matrices 
pred_clog9_3 <- ifelse(s1_p_clog9_3 > 0.5, TRUE, FALSE) 
cm_clog9_3 <- table(my_test_9_3_1$default, pred_clog9_3)
mat_clog9_3 <- confusionMatrix(cm_clog9_3)

#ROC-curces
ROC_clog9_3 <- roc(my_test_9_3_1$default, s1_p_clog9_3) 


#appending results to Dataframe
all_9_3 <- c('All 9_3 Var.', s1_e_logit9_3, s1_e_probit9_3, s1_e_clog9_3, s2_e_logit9_3, s2_e_probit9_3, s2_e_clog9_3, s3_e_logit9_3, s3_e_probit9_3,
              s3_e_clog9_3, min_s1_9_3, min_s2_9_3, min_s3_9_3, mean_clog9_3, mat_clog9_3$overall['Accuracy'], auc(ROC_clog9_3))

find_model1 <- cbind(find_model1, all_9_3)


####################
#leaving out 'installment'#
####################
my_train_9_4_1 <- my_train_10_1 %>% select(-installment) 
my_test_9_4_1 <- my_test_10_1 %>% select(-installment) 

my_train_9_4_2 <- my_train_10_2 %>% select(-installment) 
my_test_9_4_2 <- my_test_10_2 %>% select(-installment)

my_train_9_4_3 <- my_train_10_3 %>% select(-installment) 
my_test_9_4_3 <- my_test_10_2 %>% select(-installment)


#first set
s1_logit9_4 <- glm(default ~., family = binomial, data = my_train_9_4_1) 
s1_p_logit9_4 <- predict(s1_logit9_4, newdata = my_test_9_4_1, type = "response")
s1_probit9_4 <- glm(default ~., family = binomial(link = probit), data = my_train_9_4_1) 
s1_p_probit9_4 <- predict(s1_probit9_4, newdata = my_test_9_4_1, type = "response")
s1_clog9_4 <- glm(default ~., family = binomial(link = cloglog), data = my_train_9_4_1) 
s1_p_clog9_4 <- predict(s1_clog9_4, newdata = my_test_9_4_1, type = "response")


#second set
s2_logit9_4 <- glm(default ~., family = binomial, data = my_train_9_4_2) 
s2_p_logit9_4 <- predict(s2_logit9_4, newdata = my_test_9_4_2, type = "response")
s2_probit9_4 <- glm(default ~., family = binomial(link = probit), data = my_train_9_4_2) 
s2_p_probit9_4 <- predict(s2_probit9_4, newdata = my_test_9_4_2, type = "response")
s2_clog9_4 <- glm(default ~., family = binomial(link = cloglog), data = my_train_9_4_2) 
s2_p_clog9_4 <- predict(s2_clog9_4, newdata = my_test_9_4_2, type = "response")


#third set
s3_logit9_4 <- glm(default ~., family = binomial, data = my_train_9_4_3) 
s3_p_logit9_4 <- predict(s3_logit9_4, newdata = my_test_9_4_3, type = "response")
s3_probit9_4 <- glm(default ~., family = binomial(link = probit), data = my_train_9_4_3) 
s3_p_probit9_4 <- predict(s3_probit9_4, newdata = my_test_9_4_3, type = "response")
s3_clog9_4 <- glm(default ~., family = binomial(link = cloglog), data = my_train_9_4_3) 
s3_p_clog9_4 <- predict(s3_clog9_4, newdata = my_test_9_4_3, type = "response")


#evaluating the results
s1_e_logit9_4 <- eval(my_test_9_4_1$default, s1_p_logit9_4)
s1_e_probit9_4 <- eval(my_test_9_4_1$default, s1_p_probit9_4)
s1_e_clog9_4 <- eval(my_test_9_4_1$default, s1_p_clog9_4)
s2_e_logit9_4 <-eval(my_test_9_4_2$default, s2_p_logit9_4)
s2_e_probit9_4 <- eval(my_test_9_4_2$default, s2_p_probit9_4)
s2_e_clog9_4 <- eval(my_test_9_4_2$default, s2_p_clog9_4)
s3_e_logit9_4 <- eval(my_test_9_4_3$default, s3_p_logit9_4)
s3_e_probit9_4 <- eval(my_test_9_4_3$default, s3_p_probit9_4)
s3_e_clog9_4 <- eval(my_test_9_4_3$default, s3_p_clog9_4)

mean_clog9_4 <- mean(c(s1_e_clog9_4, s2_e_clog9_4, s3_e_clog9_4))

min_s1_9_4 <- c('clog', 'logit', 'probit')[which.max(c(s1_e_clog9_4, s1_e_logit9_4, s1_e_probit9_4))] 
min_s2_9_4 <- c('clog', 'logit', 'probit')[which.max(c(s2_e_clog9_4, s2_e_logit9_4, s2_e_probit9_4))]
min_s3_9_4 <- c('clog', 'logit', 'probit')[which.max(c(s3_e_clog9_4, s3_e_logit9_4, s3_e_probit9_4))]

#confusion matrices 
pred_clog9_4 <- ifelse(s1_p_clog9_4 > 0.5, TRUE, FALSE) 
cm_clog9_4 <- table(my_test_9_4_1$default, pred_clog9_4)
mat_clog9_4 <- confusionMatrix(cm_clog9_4)

#ROC-curces
ROC_clog9_4 <- roc(my_test_9_4_1$default, s1_p_clog9_4) 


#appending results to Dataframe
all_9_4 <- c('All 9_4 Var.', s1_e_logit9_4, s1_e_probit9_4, s1_e_clog9_4, s2_e_logit9_4, s2_e_probit9_4, s2_e_clog9_4, s3_e_logit9_4, s3_e_probit9_4,
              s3_e_clog9_4, min_s1_9_4, min_s2_9_4, min_s3_9_4, mean_clog9_4, mat_clog9_4$overall['Accuracy'], auc(ROC_clog9_4))

find_model1 <- cbind(find_model1, all_9_4)




####################
#leaving out 'grade'#
####################
my_train_9_5_1 <- my_train_10_1 %>% select(-grade) 
my_test_9_5_1 <- my_test_10_1 %>% select(-grade) 

my_train_9_5_2 <- my_train_10_2 %>% select(-grade) 
my_test_9_5_2 <- my_test_10_2 %>% select(-grade)

my_train_9_5_3 <- my_train_10_3 %>% select(-grade) 
my_test_9_5_3 <- my_test_10_2 %>% select(-grade)


#first set
s1_logit9_5 <- glm(default ~., family = binomial, data = my_train_9_5_1) 
s1_p_logit9_5 <- predict(s1_logit9_5, newdata = my_test_9_5_1, type = "response")
s1_probit9_5 <- glm(default ~., family = binomial(link = probit), data = my_train_9_5_1) 
s1_p_probit9_5 <- predict(s1_probit9_5, newdata = my_test_9_5_1, type = "response")
s1_clog9_5 <- glm(default ~., family = binomial(link = cloglog), data = my_train_9_5_1) 
s1_p_clog9_5 <- predict(s1_clog9_5, newdata = my_test_9_5_1, type = "response")


#second set
s2_logit9_5 <- glm(default ~., family = binomial, data = my_train_9_5_2) 
s2_p_logit9_5 <- predict(s2_logit9_5, newdata = my_test_9_5_2, type = "response")
s2_probit9_5 <- glm(default ~., family = binomial(link = probit), data = my_train_9_5_2) 
s2_p_probit9_5 <- predict(s2_probit9_5, newdata = my_test_9_5_2, type = "response")
s2_clog9_5 <- glm(default ~., family = binomial(link = cloglog), data = my_train_9_5_2) 
s2_p_clog9_5 <- predict(s2_clog9_5, newdata = my_test_9_5_2, type = "response")


#third set
s3_logit9_5 <- glm(default ~., family = binomial, data = my_train_9_5_3) 
s3_p_logit9_5 <- predict(s3_logit9_5, newdata = my_test_9_5_3, type = "response")
s3_probit9_5 <- glm(default ~., family = binomial(link = probit), data = my_train_9_5_3) 
s3_p_probit9_5 <- predict(s3_probit9_5, newdata = my_test_9_5_3, type = "response")
s3_clog9_5 <- glm(default ~., family = binomial(link = cloglog), data = my_train_9_5_3) 
s3_p_clog9_5 <- predict(s3_clog9_5, newdata = my_test_9_5_3, type = "response")


#evaluating the results
s1_e_logit9_5 <- eval(my_test_9_5_1$default, s1_p_logit9_5)
s1_e_probit9_5 <- eval(my_test_9_5_1$default, s1_p_probit9_5)
s1_e_clog9_5 <- eval(my_test_9_5_1$default, s1_p_clog9_5)
s2_e_logit9_5 <-eval(my_test_9_5_2$default, s2_p_logit9_5)
s2_e_probit9_5 <- eval(my_test_9_5_2$default, s2_p_probit9_5)
s2_e_clog9_5 <- eval(my_test_9_5_2$default, s2_p_clog9_5)
s3_e_logit9_5 <- eval(my_test_9_5_3$default, s3_p_logit9_5)
s3_e_probit9_5 <- eval(my_test_9_5_3$default, s3_p_probit9_5)
s3_e_clog9_5 <- eval(my_test_9_5_3$default, s3_p_clog9_5)

mean_clog9_5 <- mean(c(s1_e_clog9_5, s2_e_clog9_5, s3_e_clog9_5))

min_s1_9_5 <- c('clog', 'logit', 'probit')[which.max(c(s1_e_clog9_5, s1_e_logit9_5, s1_e_probit9_5))] 
min_s2_9_5 <- c('clog', 'logit', 'probit')[which.max(c(s2_e_clog9_5, s2_e_logit9_5, s2_e_probit9_5))]
min_s3_9_5 <- c('clog', 'logit', 'probit')[which.max(c(s3_e_clog9_5, s3_e_logit9_5, s3_e_probit9_5))]

#confusion matrices 
pred_clog9_5 <- ifelse(s1_p_clog9_5 > 0.5, TRUE, FALSE) 
cm_clog9_5 <- table(my_test_9_5_1$default, pred_clog9_5)
mat_clog9_5 <- confusionMatrix(cm_clog9_5)

#ROC-curces
ROC_clog9_5 <- roc(my_test_9_5_1$default, s1_p_clog9_5) 


#appending results to Dataframe
all_9_5 <- c('All 9_5 Var.', s1_e_logit9_5, s1_e_probit9_5, s1_e_clog9_5, s2_e_logit9_5, s2_e_probit9_5, s2_e_clog9_5, s3_e_logit9_5, s3_e_probit9_5,
              s3_e_clog9_5, min_s1_9_5, min_s2_9_5, min_s3_9_5, mean_clog9_5, mat_clog9_5$overall['Accuracy'], auc(ROC_clog9_5))

find_model1 <- cbind(find_model1, all_9_5)




####################
#leaving out 'home_ownership'#
####################
my_train_9_6_1 <- my_train_10_1 %>% select(-home_ownership) 
my_test_9_6_1 <- my_test_10_1 %>% select(-home_ownership) 

my_train_9_6_2 <- my_train_10_2 %>% select(-home_ownership) 
my_test_9_6_2 <- my_test_10_2 %>% select(-home_ownership)

my_train_9_6_3 <- my_train_10_3 %>% select(-home_ownership) 
my_test_9_6_3 <- my_test_10_2 %>% select(-home_ownership)


#first set
s1_logit9_6 <- glm(default ~., family = binomial, data = my_train_9_6_1) 
s1_p_logit9_6 <- predict(s1_logit9_6, newdata = my_test_9_6_1, type = "response")
s1_probit9_6 <- glm(default ~., family = binomial(link = probit), data = my_train_9_6_1) 
s1_p_probit9_6 <- predict(s1_probit9_6, newdata = my_test_9_6_1, type = "response")
s1_clog9_6 <- glm(default ~., family = binomial(link = cloglog), data = my_train_9_6_1) 
s1_p_clog9_6 <- predict(s1_clog9_6, newdata = my_test_9_6_1, type = "response")


#second set
s2_logit9_6 <- glm(default ~., family = binomial, data = my_train_9_6_2) 
s2_p_logit9_6 <- predict(s2_logit9_6, newdata = my_test_9_6_2, type = "response")
s2_probit9_6 <- glm(default ~., family = binomial(link = probit), data = my_train_9_6_2) 
s2_p_probit9_6 <- predict(s2_probit9_6, newdata = my_test_9_6_2, type = "response")
s2_clog9_6 <- glm(default ~., family = binomial(link = cloglog), data = my_train_9_6_2) 
s2_p_clog9_6 <- predict(s2_clog9_6, newdata = my_test_9_6_2, type = "response")


#third set
s3_logit9_6 <- glm(default ~., family = binomial, data = my_train_9_6_3) 
s3_p_logit9_6 <- predict(s3_logit9_6, newdata = my_test_9_6_3, type = "response")
s3_probit9_6 <- glm(default ~., family = binomial(link = probit), data = my_train_9_6_3) 
s3_p_probit9_6 <- predict(s3_probit9_6, newdata = my_test_9_6_3, type = "response")
s3_clog9_6 <- glm(default ~., family = binomial(link = cloglog), data = my_train_9_6_3) 
s3_p_clog9_6 <- predict(s3_clog9_6, newdata = my_test_9_6_3, type = "response")


#evaluating the results
s1_e_logit9_6 <- eval(my_test_9_6_1$default, s1_p_logit9_6)
s1_e_probit9_6 <- eval(my_test_9_6_1$default, s1_p_probit9_6)
s1_e_clog9_6 <- eval(my_test_9_6_1$default, s1_p_clog9_6)
s2_e_logit9_6 <-eval(my_test_9_6_2$default, s2_p_logit9_6)
s2_e_probit9_6 <- eval(my_test_9_6_2$default, s2_p_probit9_6)
s2_e_clog9_6 <- eval(my_test_9_6_2$default, s2_p_clog9_6)
s3_e_logit9_6 <- eval(my_test_9_6_3$default, s3_p_logit9_6)
s3_e_probit9_6 <- eval(my_test_9_6_3$default, s3_p_probit9_6)
s3_e_clog9_6 <- eval(my_test_9_6_3$default, s3_p_clog9_6)

mean_clog9_6 <- mean(c(s1_e_clog9_6, s2_e_clog9_6, s3_e_clog9_6))

min_s1_9_6 <- c('clog', 'logit', 'probit')[which.max(c(s1_e_clog9_6, s1_e_logit9_6, s1_e_probit9_6))] 
min_s2_9_6 <- c('clog', 'logit', 'probit')[which.max(c(s2_e_clog9_6, s2_e_logit9_6, s2_e_probit9_6))]
min_s3_9_6 <- c('clog', 'logit', 'probit')[which.max(c(s3_e_clog9_6, s3_e_logit9_6, s3_e_probit9_6))]

#confusion matrices 
pred_clog9_6 <- ifelse(s1_p_clog9_6 > 0.5, TRUE, FALSE) 
cm_clog9_6 <- table(my_test_9_6_1$default, pred_clog9_6)
mat_clog9_6 <- confusionMatrix(cm_clog9_6)

#ROC-curces
ROC_clog9_6 <- roc(my_test_9_6_1$default, s1_p_clog9_6) 


#appending results to Dataframe
all_9_6 <- c('All 9_6 Var.', s1_e_logit9_6, s1_e_probit9_6, s1_e_clog9_6, s2_e_logit9_6, s2_e_probit9_6, s2_e_clog9_6, s3_e_logit9_6, s3_e_probit9_6,
              s3_e_clog9_6, min_s1_9_6, min_s2_9_6, min_s3_9_6, mean_clog9_6, mat_clog9_6$overall['Accuracy'], auc(ROC_clog9_6))

find_model1 <- cbind(find_model1, all_9_6)


####################
#leaving out 'annual_inc'#
####################
my_train_9_7_1 <- my_train_10_1 %>% select(-annual_inc) 
my_test_9_7_1 <- my_test_10_1 %>% select(-annual_inc) 

my_train_9_7_2 <- my_train_10_2 %>% select(-annual_inc) 
my_test_9_7_2 <- my_test_10_2 %>% select(-annual_inc)

my_train_9_7_3 <- my_train_10_3 %>% select(-annual_inc) 
my_test_9_7_3 <- my_test_10_2 %>% select(-annual_inc)


#first set
s1_logit9_7 <- glm(default ~., family = binomial, data = my_train_9_7_1) 
s1_p_logit9_7 <- predict(s1_logit9_7, newdata = my_test_9_7_1, type = "response")
s1_probit9_7 <- glm(default ~., family = binomial(link = probit), data = my_train_9_7_1) 
s1_p_probit9_7 <- predict(s1_probit9_7, newdata = my_test_9_7_1, type = "response")
s1_clog9_7 <- glm(default ~., family = binomial(link = cloglog), data = my_train_9_7_1) 
s1_p_clog9_7 <- predict(s1_clog9_7, newdata = my_test_9_7_1, type = "response")


#second set
s2_logit9_7 <- glm(default ~., family = binomial, data = my_train_9_7_2) 
s2_p_logit9_7 <- predict(s2_logit9_7, newdata = my_test_9_7_2, type = "response")
s2_probit9_7 <- glm(default ~., family = binomial(link = probit), data = my_train_9_7_2) 
s2_p_probit9_7 <- predict(s2_probit9_7, newdata = my_test_9_7_2, type = "response")
s2_clog9_7 <- glm(default ~., family = binomial(link = cloglog), data = my_train_9_7_2) 
s2_p_clog9_7 <- predict(s2_clog9_7, newdata = my_test_9_7_2, type = "response")


#third set
s3_logit9_7 <- glm(default ~., family = binomial, data = my_train_9_7_3) 
s3_p_logit9_7 <- predict(s3_logit9_7, newdata = my_test_9_7_3, type = "response")
s3_probit9_7 <- glm(default ~., family = binomial(link = probit), data = my_train_9_7_3) 
s3_p_probit9_7 <- predict(s3_probit9_7, newdata = my_test_9_7_3, type = "response")
s3_clog9_7 <- glm(default ~., family = binomial(link = cloglog), data = my_train_9_7_3) 
s3_p_clog9_7 <- predict(s3_clog9_7, newdata = my_test_9_7_3, type = "response")


#evaluating the results
s1_e_logit9_7 <- eval(my_test_9_7_1$default, s1_p_logit9_7)
s1_e_probit9_7 <- eval(my_test_9_7_1$default, s1_p_probit9_7)
s1_e_clog9_7 <- eval(my_test_9_7_1$default, s1_p_clog9_7)
s2_e_logit9_7 <-eval(my_test_9_7_2$default, s2_p_logit9_7)
s2_e_probit9_7 <- eval(my_test_9_7_2$default, s2_p_probit9_7)
s2_e_clog9_7 <- eval(my_test_9_7_2$default, s2_p_clog9_7)
s3_e_logit9_7 <- eval(my_test_9_7_3$default, s3_p_logit9_7)
s3_e_probit9_7 <- eval(my_test_9_7_3$default, s3_p_probit9_7)
s3_e_clog9_7 <- eval(my_test_9_7_3$default, s3_p_clog9_7)

mean_clog9_7 <- mean(c(s1_e_clog9_7, s2_e_clog9_7, s3_e_clog9_7))

min_s1_9_7 <- c('clog', 'logit', 'probit')[which.max(c(s1_e_clog9_7, s1_e_logit9_7, s1_e_probit9_7))] 
min_s2_9_7 <- c('clog', 'logit', 'probit')[which.max(c(s2_e_clog9_7, s2_e_logit9_7, s2_e_probit9_7))]
min_s3_9_7 <- c('clog', 'logit', 'probit')[which.max(c(s3_e_clog9_7, s3_e_logit9_7, s3_e_probit9_7))]

#confusion matrices 
pred_clog9_7 <- ifelse(s1_p_clog9_7 > 0.5, TRUE, FALSE) 
cm_clog9_7 <- table(my_test_9_7_1$default, pred_clog9_7)
mat_clog9_7 <- confusionMatrix(cm_clog9_7)

#ROC-curces
ROC_clog9_7 <- roc(my_test_9_7_1$default, s1_p_clog9_7) 


#appending results to Dataframe
all_9_7 <- c('All 9_7 Var.', s1_e_logit9_7, s1_e_probit9_7, s1_e_clog9_7, s2_e_logit9_7, s2_e_probit9_7, s2_e_clog9_7, s3_e_logit9_7, s3_e_probit9_7,
              s3_e_clog9_7, min_s1_9_7, min_s2_9_7, min_s3_9_7, mean_clog9_7, mat_clog9_7$overall['Accuracy'], auc(ROC_clog9_7))

find_model1 <- cbind(find_model1, all_9_7)



####################
#leaving out 'purpose'#
####################
my_train_9_8_1 <- my_train_10_1 %>% select(-purpose) 
my_test_9_8_1 <- my_test_10_1 %>% select(-purpose) 

my_train_9_8_2 <- my_train_10_2 %>% select(-purpose) 
my_test_9_8_2 <- my_test_10_2 %>% select(-purpose)

my_train_9_8_3 <- my_train_10_3 %>% select(-purpose) 
my_test_9_8_3 <- my_test_10_2 %>% select(-purpose)


#first set
s1_logit9_8 <- glm(default ~., family = binomial, data = my_train_9_8_1) 
s1_p_logit9_8 <- predict(s1_logit9_8, newdata = my_test_9_8_1, type = "response")
s1_probit9_8 <- glm(default ~., family = binomial(link = probit), data = my_train_9_8_1) 
s1_p_probit9_8 <- predict(s1_probit9_8, newdata = my_test_9_8_1, type = "response")
s1_clog9_8 <- glm(default ~., family = binomial(link = cloglog), data = my_train_9_8_1) 
s1_p_clog9_8 <- predict(s1_clog9_8, newdata = my_test_9_8_1, type = "response")


#second set
s2_logit9_8 <- glm(default ~., family = binomial, data = my_train_9_8_2) 
s2_p_logit9_8 <- predict(s2_logit9_8, newdata = my_test_9_8_2, type = "response")
s2_probit9_8 <- glm(default ~., family = binomial(link = probit), data = my_train_9_8_2) 
s2_p_probit9_8 <- predict(s2_probit9_8, newdata = my_test_9_8_2, type = "response")
s2_clog9_8 <- glm(default ~., family = binomial(link = cloglog), data = my_train_9_8_2) 
s2_p_clog9_8 <- predict(s2_clog9_8, newdata = my_test_9_8_2, type = "response")


#third set
s3_logit9_8 <- glm(default ~., family = binomial, data = my_train_9_8_3) 
s3_p_logit9_8 <- predict(s3_logit9_8, newdata = my_test_9_8_3, type = "response")
s3_probit9_8 <- glm(default ~., family = binomial(link = probit), data = my_train_9_8_3) 
s3_p_probit9_8 <- predict(s3_probit9_8, newdata = my_test_9_8_3, type = "response")
s3_clog9_8 <- glm(default ~., family = binomial(link = cloglog), data = my_train_9_8_3) 
s3_p_clog9_8 <- predict(s3_clog9_8, newdata = my_test_9_8_3, type = "response")


#evaluating the results
s1_e_logit9_8 <- eval(my_test_9_8_1$default, s1_p_logit9_8)
s1_e_probit9_8 <- eval(my_test_9_8_1$default, s1_p_probit9_8)
s1_e_clog9_8 <- eval(my_test_9_8_1$default, s1_p_clog9_8)
s2_e_logit9_8 <-eval(my_test_9_8_2$default, s2_p_logit9_8)
s2_e_probit9_8 <- eval(my_test_9_8_2$default, s2_p_probit9_8)
s2_e_clog9_8 <- eval(my_test_9_8_2$default, s2_p_clog9_8)
s3_e_logit9_8 <- eval(my_test_9_8_3$default, s3_p_logit9_8)
s3_e_probit9_8 <- eval(my_test_9_8_3$default, s3_p_probit9_8)
s3_e_clog9_8 <- eval(my_test_9_8_3$default, s3_p_clog9_8)

mean_clog9_8 <- mean(c(s1_e_clog9_8, s2_e_clog9_8, s3_e_clog9_8))

min_s1_9_8 <- c('clog', 'logit', 'probit')[which.max(c(s1_e_clog9_8, s1_e_logit9_8, s1_e_probit9_8))] 
min_s2_9_8 <- c('clog', 'logit', 'probit')[which.max(c(s2_e_clog9_8, s2_e_logit9_8, s2_e_probit9_8))]
min_s3_9_8 <- c('clog', 'logit', 'probit')[which.max(c(s3_e_clog9_8, s3_e_logit9_8, s3_e_probit9_8))]

#confusion matrices 
pred_clog9_8 <- ifelse(s1_p_clog9_8 > 0.5, TRUE, FALSE) 
cm_clog9_8 <- table(my_test_9_8_1$default, pred_clog9_8)
mat_clog9_8 <- confusionMatrix(cm_clog9_8)

#ROC-curces
ROC_clog9_8 <- roc(my_test_9_8_1$default, s1_p_clog9_8) 


#appending results to Dataframe
all_9_8 <- c('All 9_8 Var.', s1_e_logit9_8, s1_e_probit9_8, s1_e_clog9_8, s2_e_logit9_8, s2_e_probit9_8, s2_e_clog9_8, s3_e_logit9_8, s3_e_probit9_8,
              s3_e_clog9_8, min_s1_9_8, min_s2_9_8, min_s3_9_8, mean_clog9_8, mat_clog9_8$overall['Accuracy'], auc(ROC_clog9_8))

find_model1 <- cbind(find_model1, all_9_8)




####################
#leaving out 'dti'#
####################
my_train_9_9_1 <- my_train_10_1 %>% select(-dti) 
my_test_9_9_1 <- my_test_10_1 %>% select(-dti) 

my_train_9_9_2 <- my_train_10_2 %>% select(-dti) 
my_test_9_9_2 <- my_test_10_2 %>% select(-dti)

my_train_9_9_3 <- my_train_10_3 %>% select(-dti) 
my_test_9_9_3 <- my_test_10_2 %>% select(-dti)


#first set
s1_logit9_9 <- glm(default ~., family = binomial, data = my_train_9_9_1) 
s1_p_logit9_9 <- predict(s1_logit9_9, newdata = my_test_9_9_1, type = "response")
s1_probit9_9 <- glm(default ~., family = binomial(link = probit), data = my_train_9_9_1) 
s1_p_probit9_9 <- predict(s1_probit9_9, newdata = my_test_9_9_1, type = "response")
s1_clog9_9 <- glm(default ~., family = binomial(link = cloglog), data = my_train_9_9_1) 
s1_p_clog9_9 <- predict(s1_clog9_9, newdata = my_test_9_9_1, type = "response")


#second set
s2_logit9_9 <- glm(default ~., family = binomial, data = my_train_9_9_2) 
s2_p_logit9_9 <- predict(s2_logit9_9, newdata = my_test_9_9_2, type = "response")
s2_probit9_9 <- glm(default ~., family = binomial(link = probit), data = my_train_9_9_2) 
s2_p_probit9_9 <- predict(s2_probit9_9, newdata = my_test_9_9_2, type = "response")
s2_clog9_9 <- glm(default ~., family = binomial(link = cloglog), data = my_train_9_9_2) 
s2_p_clog9_9 <- predict(s2_clog9_9, newdata = my_test_9_9_2, type = "response")


#third set
s3_logit9_9 <- glm(default ~., family = binomial, data = my_train_9_9_3) 
s3_p_logit9_9 <- predict(s3_logit9_9, newdata = my_test_9_9_3, type = "response")
s3_probit9_9 <- glm(default ~., family = binomial(link = probit), data = my_train_9_9_3) 
s3_p_probit9_9 <- predict(s3_probit9_9, newdata = my_test_9_9_3, type = "response")
s3_clog9_9 <- glm(default ~., family = binomial(link = cloglog), data = my_train_9_9_3) 
s3_p_clog9_9 <- predict(s3_clog9_9, newdata = my_test_9_9_3, type = "response")


#evaluating the results
s1_e_logit9_9 <- eval(my_test_9_9_1$default, s1_p_logit9_9)
s1_e_probit9_9 <- eval(my_test_9_9_1$default, s1_p_probit9_9)
s1_e_clog9_9 <- eval(my_test_9_9_1$default, s1_p_clog9_9)
s2_e_logit9_9 <-eval(my_test_9_9_2$default, s2_p_logit9_9)
s2_e_probit9_9 <- eval(my_test_9_9_2$default, s2_p_probit9_9)
s2_e_clog9_9 <- eval(my_test_9_9_2$default, s2_p_clog9_9)
s3_e_logit9_9 <- eval(my_test_9_9_3$default, s3_p_logit9_9)
s3_e_probit9_9 <- eval(my_test_9_9_3$default, s3_p_probit9_9)
s3_e_clog9_9 <- eval(my_test_9_9_3$default, s3_p_clog9_9)

mean_clog9_9 <- mean(c(s1_e_clog9_9, s2_e_clog9_9, s3_e_clog9_9))

min_s1_9_9 <- c('clog', 'logit', 'probit')[which.max(c(s1_e_clog9_9, s1_e_logit9_9, s1_e_probit9_9))] 
min_s2_9_9 <- c('clog', 'logit', 'probit')[which.max(c(s2_e_clog9_9, s2_e_logit9_9, s2_e_probit9_9))]
min_s3_9_9 <- c('clog', 'logit', 'probit')[which.max(c(s3_e_clog9_9, s3_e_logit9_9, s3_e_probit9_9))]

#confusion matrices 
pred_clog9_9 <- ifelse(s1_p_clog9_9 > 0.5, TRUE, FALSE) 
cm_clog9_9 <- table(my_test_9_9_1$default, pred_clog9_9)
mat_clog9_9 <- confusionMatrix(cm_clog9_9)

#ROC-curces
ROC_clog9_9 <- roc(my_test_9_9_1$default, s1_p_clog9_9) 


#appending results to Dataframe
all_9_9 <- c('All 9_9 Var.', s1_e_logit9_9, s1_e_probit9_9, s1_e_clog9_9, s2_e_logit9_9, s2_e_probit9_9, s2_e_clog9_9, s3_e_logit9_9, s3_e_probit9_9,
              s3_e_clog9_9, min_s1_9_9, min_s2_9_9, min_s3_9_9, mean_clog9_9, mat_clog9_9$overall['Accuracy'], auc(ROC_clog9_9))

find_model1 <- cbind(find_model1, all_9_9)




####################
#leaving out 'open_acc'#
####################
my_train_9_10_1 <- my_train_10_1 %>% select(-open_acc) 
my_test_9_10_1 <- my_test_10_1 %>% select(-open_acc) 

my_train_9_10_2 <- my_train_10_2 %>% select(-open_acc) 
my_test_9_10_2 <- my_test_10_2 %>% select(-open_acc)

my_train_9_10_3 <- my_train_10_3 %>% select(-open_acc) 
my_test_9_10_3 <- my_test_10_2 %>% select(-open_acc)


#first set
s1_logit9_10 <- glm(default ~., family = binomial, data = my_train_9_10_1) 
s1_p_logit9_10 <- predict(s1_logit9_10, newdata = my_test_9_10_1, type = "response")
s1_probit9_10 <- glm(default ~., family = binomial(link = probit), data = my_train_9_10_1) 
s1_p_probit9_10 <- predict(s1_probit9_10, newdata = my_test_9_10_1, type = "response")
s1_clog9_10 <- glm(default ~., family = binomial(link = cloglog), data = my_train_9_10_1) 
s1_p_clog9_10 <- predict(s1_clog9_10, newdata = my_test_9_10_1, type = "response")


#second set
s2_logit9_10 <- glm(default ~., family = binomial, data = my_train_9_10_2) 
s2_p_logit9_10 <- predict(s2_logit9_10, newdata = my_test_9_10_2, type = "response")
s2_probit9_10 <- glm(default ~., family = binomial(link = probit), data = my_train_9_10_2) 
s2_p_probit9_10 <- predict(s2_probit9_10, newdata = my_test_9_10_2, type = "response")
s2_clog9_10 <- glm(default ~., family = binomial(link = cloglog), data = my_train_9_10_2) 
s2_p_clog9_10 <- predict(s2_clog9_10, newdata = my_test_9_10_2, type = "response")


#third set
s3_logit9_10 <- glm(default ~., family = binomial, data = my_train_9_10_3) 
s3_p_logit9_10 <- predict(s3_logit9_10, newdata = my_test_9_10_3, type = "response")
s3_probit9_10 <- glm(default ~., family = binomial(link = probit), data = my_train_9_10_3) 
s3_p_probit9_10 <- predict(s3_probit9_10, newdata = my_test_9_10_3, type = "response")
s3_clog9_10 <- glm(default ~., family = binomial(link = cloglog), data = my_train_9_10_3) 
s3_p_clog9_10 <- predict(s3_clog9_10, newdata = my_test_9_10_3, type = "response")


#evaluating the results
s1_e_logit9_10 <- eval(my_test_9_10_1$default, s1_p_logit9_10)
s1_e_probit9_10 <- eval(my_test_9_10_1$default, s1_p_probit9_10)
s1_e_clog9_10 <- eval(my_test_9_10_1$default, s1_p_clog9_10)
s2_e_logit9_10 <-eval(my_test_9_10_2$default, s2_p_logit9_10)
s2_e_probit9_10 <- eval(my_test_9_10_2$default, s2_p_probit9_10)
s2_e_clog9_10 <- eval(my_test_9_10_2$default, s2_p_clog9_10)
s3_e_logit9_10 <- eval(my_test_9_10_3$default, s3_p_logit9_10)
s3_e_probit9_10 <- eval(my_test_9_10_3$default, s3_p_probit9_10)
s3_e_clog9_10 <- eval(my_test_9_10_3$default, s3_p_clog9_10)

mean_clog9_10 <- mean(c(s1_e_clog9_10, s2_e_clog9_10, s3_e_clog9_10))

min_s1_9_10 <- c('clog', 'logit', 'probit')[which.max(c(s1_e_clog9_10, s1_e_logit9_10, s1_e_probit9_10))] 
min_s2_9_10 <- c('clog', 'logit', 'probit')[which.max(c(s2_e_clog9_10, s2_e_logit9_10, s2_e_probit9_10))]
min_s3_9_10 <- c('clog', 'logit', 'probit')[which.max(c(s3_e_clog9_10, s3_e_logit9_10, s3_e_probit9_10))]

#confusion matrices 
pred_clog9_10 <- ifelse(s1_p_clog9_10 > 0.5, TRUE, FALSE) 
cm_clog9_10 <- table(my_test_9_10_1$default, pred_clog9_10)
mat_clog9_10 <- confusionMatrix(cm_clog9_10)

#ROC-curces
ROC_clog9_10 <- roc(my_test_9_10_1$default, s1_p_clog9_10) 


#appending results to Dataframe
all_9_10 <- c('All 9_10 Var.', s1_e_logit9_10, s1_e_probit9_10, s1_e_clog9_10, s2_e_logit9_10, s2_e_probit9_10, s2_e_clog9_10, s3_e_logit9_10, s3_e_probit9_10,
              s3_e_clog9_10, min_s1_9_10, min_s2_9_10, min_s3_9_10, mean_clog9_10, mat_clog9_10$overall['Accuracy'], auc(ROC_clog9_10))

find_model1 <- cbind(find_model1, all_9_10)





