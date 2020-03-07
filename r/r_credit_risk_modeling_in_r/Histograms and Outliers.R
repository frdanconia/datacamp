                       #Explore Continuous Variables

#Histograms
    
hist(loan_data$int_rate,main="Histogram of Interest Rate",xlab="Interest Rate")
  
hist(loan_data$age,main="Histogram of Age",xlab="Age")

hist_income<-hist(loan_data$annual_inc,main="Histogram of Annual Income",xlab="Annual Income")


#locations of the breaks    

hist_income$breaks
  
n_breaks=sqrt(nrow(loan_data))
      
hist_income_n<-hist(loan_data$annual_inc,breaks=n_breaks,main="Histogram of Annual Income",xlab="Annual Income")
        
                        #Outlier Detection
          
# Plot the annual income variable

plot(loan_data$annual_inc,ylab="Annual Income")
        
# Plot the age variable
plot(loan_data$age,ylab="Age")
        
#Bivariate Plot of age and annual income variable

plot(loan_data$age,loan_data$annual_inc,xlab="Age",ylab="Annual Income")
        
                                    
                        #Outliers Deletion
          
#Expert Judgement

index_outlier_expert<-which(loan_data$annual_inc>3000000)
          
loan_data_expert<-loan_data[-index_outlier_expert,]
            
hist(loan_data_expert$annual_inc,breaks=sqrt(nrow(loan_data_expert)),main="Expert Judgement",xlab="Annual Income")
            
#Rule of Thumb
              
outlier_cutoff<-quantile(loan_data$annual_inc,0.75)+1.5*IQR(loan_data$annual_inc)
              
index_outlier_ROT<-which(loan_data$annual_inc>outlier_cutoff)
                
loan_data_ROT<-loan_data[-index_outlier_ROT,]
                  
hist(loan_data_ROT$annual_inc,breaks=sqrt(nrow(loan_data_ROT)),main="Rule Of Thumb",xlab="Annual Income")

#View Outlier from Expert Judgement

loan_data_OUTLIER<-loan_data[index_outlier_expert,]
