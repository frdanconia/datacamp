                    #Exploring the Credit Data


library(openxlsx)
library(rio)
library(gmodels)

loan_data<-import("loan_data.csv")

loan_data$home_ownership=as.factor(loan_data$home_ownership)
loan_data$grade=as.factor(loan_data$grade)
loan_data$loan_status=as.factor(loan_data$loan_status)

# View the structure of loan_data

str(loan_data)

head(loan_data,10)
