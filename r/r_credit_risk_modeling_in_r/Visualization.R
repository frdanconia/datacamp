# load libraries
library(lattice)

# create histograms for each attribute
par(mfrow=c(1,5))
for(i in 1:5) {
  hist(numeric_loan_data[,i], main=names(numeric_loan_data)[i])
}


# create a panel of simpler density plots by attribute
par(mfrow=c(1,5))
for(i in 1:5) {
  plot(density(numeric_loan_data[,i]), main=names(numeric_loan_data)[i])
}


# Create separate boxplots for each attribute
par(mfrow=c(1,5))
for(i in 1:5) {
  boxplot(numeric_loan_data[,i], main=names(numeric_loan_data)[i])
}

# create a bar plot of each categorical attribute
par(mfrow=c(2,4))
for(i in 1:3) {
  counts <- table(category_loan_data[,i])
  name <- names(category_loan_data)[i]
  barplot(counts, main=name)
}

# load libraries
library(Amelia)
library(mlbench)

# create a missing map
missmap(loan_data, col=c("black", "grey"), legend=FALSE)

