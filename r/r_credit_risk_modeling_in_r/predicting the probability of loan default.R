# Build the logistic regression model

log_model_small<-glm(formula=loan_status~age+home_ownership,family="binomial",data=training_set)

test_case<-as.data.frame(test_set[1,])

predict(log_model_small,newdata=test_case,type="response")


# Build the logistic regression model on

log_model_age_ir<-glm(formula=loan_status~age+ir_cat,family="binomial",data=training_set)

test_case<-as.data.frame(test_set[,])

predictions_all_age_ir<-predict(log_model_age_ir,newdata=test_case,type="response")

# Look at the range of the object 
range(predictions_all_age_ir)

# Build the logistic regression model
log_model_full <- glm(loan_status ~ ., family = "binomial", data = training_set)

# Make PD-predictions for all test set elements using the the full logistic regression model
predictions_all_full <- predict(log_model_full, newdata = test_set, type = "response")

# Look at the predictions range
range(predictions_all_full)
