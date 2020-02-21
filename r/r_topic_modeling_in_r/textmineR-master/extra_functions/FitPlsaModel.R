#### Just chicken scratch for now. This is my workspace to create a Plsa model
# fitted by the EM algorithm. Likely implementation will actually be in C++.
# ideally, I'd like to approximate it by chunking the data, calculating parameters
# in parallel, then reconciling by averaging. Wash, rinse, repeat.

### As of 10/26/2017, this is garbage that doesn't converge. More work to follow!

### Maybe I should try logs?

rm(list = ls())

library(textmineR)

### initialize some stuff with toy data ----------------------------------------
dtm <- as.matrix(nih_sample_dtm)

# these will (later) be initialized at random
# phi <- nih_sample_topic_model$phi
# theta <- nih_sample_topic_model$theta

k <- 10

phi <- gtools::rdirichlet(n = k, alpha = colSums(dtm) / sum(dtm) * 100) # do.call(rbind, lapply(1:k, function(x) colSums(dtm) / sum(dtm)))

theta <- gtools::rdirichlet(n = nrow(dtm), alpha = rep(0.1, k)) # do.call(rbind, lapply(seq_len(nrow(dtm)), function(x) rep(1 / k, k))) # 

p_w <- colSums(dtm) / sum(dtm)

p_d <- rowSums(dtm) / sum(dtm)

max_iters <- 1000 # something small for now

cost_history <- numeric(max_iters)

### declare the main calculating functions -------------------------------------
GetPhi <- function(gamma, theta, p_d, p_w){ # 
  
  p_t <- p_d %*% theta
  
  # p_w <- p_t %*% phi
  
  result <- log(t(gamma)) + log(p_w)
  
  result <- t(result) - as.numeric(log(p_t))
  
  result <- exp(result)
  
  result <- result / rowSums(result)
  
}

GetGamma <- function(phi, theta, p_d, p_w){ # 
  
  p_t <- p_d %*% theta
  
  p_w <- p_t %*% phi
  
  result <- log(phi) + log(as.numeric(p_t))
  
  result <- t(result) - log(p_w)

  result <- exp(result)
  
  result <- result / rowSums(result)
  
  result <- t(result)
}

GetTheta <- function(gamma, dtm){
  result <- dtm %*% t(gamma)
  # result <- result / rowSums(result)
  result <- exp(result) / rowSums(exp(result))
  result
}

GetSS <- function(phi, theta, dtm){
  
  yhat <- (theta %*% phi) * rowSums(dtm)
  
  ss <- (yhat - dtm) ^ 2
  
  ss <- sum(rowSums(ss))
  
  ss
}


### do some iterations ---------------------------------------------------------

iter <- 1

while (iter <= max_iters) {
  
  # get gamma
  gamma <- CalcPhiPrime(phi = phi, theta = theta, p_docs = p_d) # GetGamma(phi = phi, theta = theta, p_d = p_d, p_w = p_w) #
  
  # get theta
  theta <- GetTheta(gamma = gamma, dtm = dtm)
  
  # get phi
  phi <- GetPhi(gamma = gamma, theta = theta, p_d = p_d, p_w = p_w) # 
  
  # get names lined up
  rownames(theta) <- rownames(dtm)
  colnames(phi) <- colnames(dtm)
  rownames(phi) <- paste0("t_", seq_len(nrow(phi)))
  colnames(theta) <- rownames(phi)
  
  # get cost
  cost_history[ iter ] <- mean(CalcProbCoherence(phi, dtm, 5)) # GetSS(phi = phi, theta = theta, dtm = dtm) # CalcTopicModelR2(dtm = Matrix(dtm, sparse = T), phi, theta) #
  
  iter <- iter + 1
  
}


plot(cost_history, type = "l")
