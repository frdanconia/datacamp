pred_undersample <- predict(ptree_undersample, newdata = test_set)[,2]
pred_prior <- predict(ptree_prior, newdata = test_set)[,2]
pred_loss_matrix <- predict(ptree_loss_matrix, newdata = test_set)[,2]
pred_weights <- predict(ptree_weights, newdata = test_set)[,2]

# Construct the objects containing ROC-information
ROC_undersample <- roc(test_set$loan_status, pred_undersample)
ROC_prior <- roc(test_set$loan_status, pred_prior)
ROC_loss_matrix <- roc(test_set$loan_status, pred_loss_matrix)
ROC_weights <- roc(test_set$loan_status, pred_weights)

# Draw the ROC-curves in one plot
plot(ROC_undersample)
lines(ROC_prior, col="blue")
lines(ROC_loss_matrix, col="red")
lines(ROC_weights, col="green")

# Compute the AUCs
auc(ROC_undersample)
auc(ROC_prior)
auc(ROC_loss_matrix)
auc(ROC_weights)