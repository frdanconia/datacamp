# Set seed of 567
set.seed(567)

# Store row numbers for training set: index_train
index_train=sample(1:nrow(loan_data_expert),2/3*nrow(loan_data_expert))

# Create training set: training_set
training_set <- loan_data_expert[index_train, ]

training_set$emp_length<-NULL
training_set$int_rate<-NULL



# Create test set: test_set

test_set<-loan_data_expert[-index_train, ]

test_set$emp_length<-NULL
test_set$int_rate<-NULL

# Create confusion matrix
model_pred <- rbinom(9697, 1,0.5)

conf_matrix <- table(test_set$loan_status, model_pred)

# Compute classification accuracy
acc_classification <- sum(diag(conf_matrix)) / nrow(test_set)





