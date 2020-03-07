library(rpart)
library(rpart.plot)
library(caTools)
library(rio)

fit_default<-rpart(loan_status~.,method="class",data=training_set)

ab <- subset(training_set, training_set$loan_status==1)

ed <- subset(training_set, training_set$loan_status==0)                

cd<- ed[sample(nrow(ed),4380), ]


undersampled_training_set <- rbind(ab,cd)

#A decision tree is constructed using the undersampled training set. 
#Include rpart.control to relax the complexity parameter to 0.001.

tree_undersample <- rpart(loan_status ~ ., method = "class",
                          data =  undersampled_training_set,
                          control = rpart.control(cp = 0.001))

# Plot the decision tree
plot(tree_undersample, uniform = TRUE)

# Add labels to the decision tree
text(tree_undersample)


# Plot the cross-validated error rate as a function of the complexity parameter
plotcp(tree_undersample)

# Use printcp() to identify for which complexity parameter the cross-validated error rate is minimized
printcp(tree_undersample)

# Create an index for of the row with the minimum xerror
index <- which.min(tree_undersample$cptable[, "xerror"])

# Create tree_min
tree_min <- tree_undersample$cptable[index, "CP"]

#  Prune the tree using tree_min
ptree_undersample <- prune(tree_undersample, cp = tree_min)

# Use prp() to plot the pruned tree
prp(ptree_undersample,extra=1)
