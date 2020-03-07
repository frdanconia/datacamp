                     #Explore Categorical Variables

library(gmodels)
library(rio)


# Call CrossTable() on loan_status
CrossTable(loan_data$loan_status)

# Call CrossTable() on home_ownership
CrossTable(loan_data$home_ownership)

# Call CrossTable() on grade
CrossTable(loan_data$grade)

# Call CrossTable() on grade and loan_status
CrossTable(loan_data$grade,loan_data$loan_status,prop.r=TRUE,prop.c=FALSE , prop.t=FALSE,prop.chisq=FALSE)

# Call CrossTable() on home_ownership and loan_status
CrossTable(loan_data$home_ownership,loan_data$loan_status,prop.r=TRUE,prop.c=FALSE , prop.t=FALSE,prop.chisq=FALSE)

