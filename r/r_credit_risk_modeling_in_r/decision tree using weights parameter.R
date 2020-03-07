library(rpart)
library(rpart.plot)
library(caTools)
library(rio)



case_weights <- rep(NA, length(training_set$loan_status))

case_weights[which(training_set$loan_status == 0)] <- "1"
case_weights[which(training_set$loan_status == 1)] <- "3"

#case_weights <- ifelse(z$loan_status==0,1,3)

case_weights<-as.numeric(case_weights)
#tree_weights<-rpart(loan_status~.,method="class",data=z)

tree_weights <- rpart(loan_status ~ ., method = "class",
                      data = training_set, weights = case_weights,
                      control = rpart.control(minsplit = 5, minbucket = 2, cp = 0.001))

# Plot the cross-validated error rate for a changing cp
plotcp(tree_weights)

# Create an index for of the row with the minimum xerror
index <- which.min(tree_weights$cp[ , "xerror"])



# Create tree_min
tree_min <- tree_weights$cp[index, "CP"]

#  Prune the tree using tree_min
ptree_weights <- prune(tree_weights, tree_min)

# Plot the pruned tree using the rpart.plot()-package
prp(ptree_weights, extra = 1)




pred_weights <- predict(ptree_weights, newdata = test_set,type="class")


