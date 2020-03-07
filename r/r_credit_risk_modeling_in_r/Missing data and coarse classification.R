                         #Missing Inputs:Strategies


#Missing Inputs Dataset

loan_data_missing_inputs <- loan_data_expert[rowSums(is.na(loan_data_expert)) > 0,]

#Missing Inputs of Employement Length Variable

summary(loan_data_expert$emp_length)

#Missing Inputs of Interest Rate Variable

summary(loan_data_expert$int_rate)

                            #Deleting the missing values

#Delete Rows with missing inputs in Employement Length variable

index_NA<-which(is.na(loan_data_expert$emp_length))
loan_data_NA<-loan_data_expert[-c(index_NA),]

#Delete Column Employement Length
loan_data_delete_employ<-loan_data_expert
loan_data_delete_employ$emp_length<-NULL

#Delete Rows with missing inputs in Interest Rate variable

# Get indices of missing interest rates: na_index
na_index <- which(is.na(loan_data_expert$int_rate))

# Remove observations with missing interest rates: loan_data_delrow_na
loan_data_delrow_na <- loan_data_expert[-na_index, ]

# Make copy of loan_data
loan_data_delcol_na <- loan_data_expert

# Delete interest rate column from loan_data_delcol_na
loan_data_delcol_na$int_rate <- NULL

                             
                             #Replacing the missing values

#Median Imputation
index_NA_M<-which(is.na(loan_data_expert$emp_length))
loan_data_replace<-loan_data_expert
loan_data_replace$emp_length[index_NA_M]<-median(loan_data_expert$emp_length,na.rm=TRUE)

# Compute the median of int_rate
median_ir <- median(loan_data_expert$int_rate, na.rm = TRUE)

# Make copy of loan_data
loan_data_replace_ir <- loan_data_expert

# Replace missing interest rates with median
loan_data_replace_ir$int_rate[na_index] <- median_ir




                            #Keeping the missing values

#Problem: will cause row deletions for many models
#Solution: coarse classification,put variables in "bins"

                            #Coarse Classification

#Using this method we put a continuous variable into so called "bins"

# Make the necessary replacements in the coarse classification example below 
loan_data_expert$ir_cat <- rep(NA, length(loan_data_expert$int_rate))

loan_data_expert$ir_cat[which(loan_data_expert$int_rate <= 8)] <- "0-8"
loan_data_expert$ir_cat[which(loan_data_expert$int_rate > 8 & loan_data_expert$int_rate <= 11)] <- "8-11"
loan_data_expert$ir_cat[which(loan_data_expert$int_rate > 11 & loan_data_expert$int_rate <= 13.5)] <- "11-13.5"
loan_data_expert$ir_cat[which(loan_data_expert$int_rate > 13.5)] <- "13.5+"
loan_data_expert$ir_cat[which(is.na(loan_data_expert$int_rate))] <- "Missing"

loan_data_expert$ir_cat <- as.factor(loan_data_expert$ir_cat)

# Look at your new variable using plot()
plot(loan_data_expert$ir_cat)

# Make the necessary replacements in the coarse classification example below 
loan_data_expert$emp_cat <- rep(NA, length(loan_data_expert$emp_length))

loan_data_expert$emp_cat[which(loan_data_expert$emp_length <= 15)] <- "0-15"
loan_data_expert$emp_cat[which(loan_data_expert$emp_length > 15 & loan_data_expert$emp_length <= 30)] <- "15-30"
loan_data_expert$emp_cat[which(loan_data_expert$emp_length > 30 & loan_data_expert$emp_length <= 45)] <- "30-45"
loan_data_expert$emp_cat[which(loan_data_expert$emp_length > 45)] <- "45+"
loan_data_expert$emp_cat[which(is.na(loan_data_expert$emp_length))] <- "Missing"

loan_data_expert$emp_cat <- as.factor(loan_data_expert$emp_cat)

# Look at your new variable using plot()
plot(loan_data_expert$emp_cat)


