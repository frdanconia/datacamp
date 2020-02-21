library(ggplot2)

sample.space <- c(0,1)

theta <- 0.5 # this is a fair coin
N <- 10000 # we want to flip a coin 20 times

flips1 <- data.frame(sample(sample.space, 
                            size = N, 
                            replace = TRUE, 
                            prob = c(theta, 1 - theta)))
colnames(flips1) <- "coinFlip"

flips2 <- data.frame(sample(sample.space, 
                            size = N, 
                            replace = TRUE, 
                            prob = c(theta, 1 - theta)))
colnames(flips2) <- "coinFlip"