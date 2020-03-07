library(ggplot2)          
ggplot(loan_data, aes(age, annual_inc)) + geom_point(aes(color = grade)) + 
    scale_x_continuous("age", breaks = seq(0,150,20))+
       scale_y_continuous("annual_inc", breaks = seq(0,6000000,by = 1000000))+
       theme_bw() + labs(title="Scatterplot")

ggplot(loan_data, aes(age, annual_inc)) + geom_point(aes(color = grade)) + 
  scale_x_continuous("age", breaks = seq(0,150,20))+
  scale_y_continuous("annual_inc", breaks = seq(0,6000000,by = 1000000))+ 
  theme_bw() + labs(title="Scatterplot") + facet_wrap( ~ grade)
install.packages("corrgram")
library(corrgram)

corrgram(loan_data, order=NULL, panel=panel.shade, text.panel=panel.txt,
         main="Correlogram") 
