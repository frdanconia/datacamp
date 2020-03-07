pred_loss_matrix <- predict(tree_loss_matrix, newdata = test_set)[,2]
strategy_bank<-
  function(prob_of_def){
    cutoff=rep(NA, 21)
    bad_rate=rep(NA, 21)
    accept_rate=seq(1,0,by=-0.05)
    for (i in 1:21){
      cutoff[i]=quantile(prob_of_def,accept_rate[i])
      pred_i=ifelse(prob_of_def> cutoff[i], 1, 0)
      pred_as_good=test_set$loan_status[pred_i==0]
      bad_rate[i]=as.numeric(table(pred_as_good))[2]/length(pred_as_good)}
    table=cbind(accept_rate,cutoff=round(cutoff,4),bad_rate=round(bad_rate,4))
    return(list(table=table,bad_rate=bad_rate, accept_rate=accept_rate, cutoff=cutoff))
  }


# Have a look at the function strategy_bank
strategy_bank

# Apply the function strategy_bank to both predictions_cloglog and predictions_loss_matrix
strategy_cloglog <- strategy_bank(predictions_cloglog)
strategy_loss_matrix <- strategy_bank(pred_loss_matrix)

# Obtain the strategy tables for both prediction-vectors
strategy_cloglog$table
strategy_loss_matrix$table

# Draw the strategy functions
par(mfrow = c(1,2))
plot(strategy_cloglog$accept_rate, strategy_cloglog$bad_rate, 
     type = "l", xlab = "Acceptance rate", ylab = "Bad rate", 
     lwd = 2, main = "logistic regression")

plot(strategy_loss_matrix$accept_rate, strategy_loss_matrix$bad_rate, 
     type = "l", xlab = "Acceptance rate", 
     ylab = "Bad rate", lwd = 2, main = "tree")