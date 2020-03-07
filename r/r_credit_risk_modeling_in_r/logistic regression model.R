#Logistic regression model including the variable age as a predictor

log_model_age<-glm(formula=loan_status~age,family="binomial",data=training_set)

# Build a glm model with variable ir_cat as a predictor
log_model_cat=glm(formula=loan_status~ir_cat,family="binomial",data=training_set)


# Print the parameter estimates 
#category for which no parameter estimate is given is called the reference category.

log_model_cat

# Look at the different categories in ir_cat using table()
table(loan_data_expert$ir_cat)

log_model_cat$coefficients

# Build the logistic regression model
log_model_multi=glm(formula=loan_status~age+ir_cat+grade+loan_amnt+annual_inc,family="binomial",data=training_set)



# Obtain significance levels using summary()
summary(log_model_multi)
