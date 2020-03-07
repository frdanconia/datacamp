# The code for the logistic regression model and the predictions is given below
log_model_full <- glm(loan_status ~ ., family = "binomial", data = training_set)
predictions_all_full <- predict(log_model_full, newdata = test_set, type = "response")

# Make a binary predictions-vector using a cut-off of 15%
pred_cutoff_15 <- ifelse(predictions_all_full > 0.15, 1, 0)

# Construct a confusion matrix

df<-data.frame(test_set$loan_status,pred_cutoff_15)

confusion_matrix<-table(test_set$loan_status, pred_cutoff_15)

accuracy <- sum(diag(confusion_matrix)) / nrow(test_set)





