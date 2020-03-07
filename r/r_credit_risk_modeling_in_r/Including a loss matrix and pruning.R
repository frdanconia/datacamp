library(rpart)
library(rpart.plot)
library(caTools)
library(rio)
# Change the code provided in the video such that a decision tree is constructed using a loss matrix penalizing 10 times more heavily for misclassified defaults.
tree_loss_matrix  <- rpart(loan_status ~ ., method = "class", data = training_set,
                           parms = list(loss = matrix(c(0, 10, 1, 0), ncol = 2)),
                           control = rpart.control(cp = 0.001))

# Plot the decision tree
plot(tree_loss_matrix, uniform = TRUE)

# Add labels to the decision tree
text(tree_loss_matrix)

# tree_prior is loaded in your workspace

# Plot the cross-validated error rate as a function of the complexity parameter
plotcp(tree_loss_matrix)

# Use printcp() to identify for which complexity parameter the cross-validated error rate is minimized
printcp(tree_loss_matrix)

# Create an index for of the row with the minimum xerror
index <- which.min(tree_loss_matrix$cptable[, "xerror"])

# Create tree_min
tree_min <- tree_loss_matrix$cptable[index, "CP"]

#  Prune the tree using tree_min
ptree_loss_matrix <- prune(tree_loss_matrix, cp = tree_min)

# Use prp() to plot the pruned tree
prp(ptree_loss_matrix,extra=1)
