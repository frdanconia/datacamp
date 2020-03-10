# linear regression on skin non skin

#library
setwd("~/Desktop/DataSets/SkinNonSkin")
library(janitor)
library(caTools)
library(caret)
#import CSV
data1<-read.csv("Skin_Nonskin.csv",sep="\t")
data1<-clean_names(data1)
set.seed(101)
#Split data 70/30
split1<-sample.split(data1$x1,SplitRatio = 0.7)
train_set1<-subset(data,split1==TRUE)
test_set1<-subset(data,split1==FALSE)
#Model for 70/30
model1<-lm(formula=x1~.,data1=train_set)
y_pred1<-predict(model,test_set1)

result1<-cbind(y_pred1,test_set1$x1)
colnames(result1)<-c('predicted','actual')
result1<-as.data.frame(result1)

#root mean square error and mse for 70/30
mae1<-MAE(result1$predicted,result1$actual)
mse1<-mean((result1$predicted-result1$actual)^2)
rmse1<-sqrt(mse1)
print("mae of 70/30 on skin non skin")
print(mae)
print("rmse of 70/30 on skin non skin")
print(rmse)

#10 fold cross validation
model1.1<-train(x1~.,data1,method="lm",trControl=trainControl(method="CV",number = 10,verboseIter = TRUE))

# metric
print(" rmse and mae of skin non skin with 10 fold")
print(model1.1)

#logical regression on skin non skin

setwd("~/Desktop/DataSets/SkinNonSkin")
library(janitor)
library(caTools)
library(caret)

#import CSV
data2<-read.csv("Skin_Nonskin.csv",sep="\t")
data2<-clean_names(data2)
set.seed(101)

#Split data 70/30
split2<-sample.split(data2$x1,SplitRatio = 0.7)
train_set2<-subset(data2,split2==TRUE)
test_set2<-subset(data2,split2==FALSE)

#Model for 70/30
model2 = glm(formula = x1 ~., data2 = train_set)
summary(model2)
y_pred2<-predict(model2,test_set2)
result2<-cbind(y_pred2,test_set2$x1)
colnames(result2)<-c('predicted','actual')
result2<-as.data.frame(result2)
#summary(result)

#root mean square error and mse for 70/30
mae2<-MAE(result2$predicted,result2$actual)
mse2<-mean((result2$predicted-result2$actual)^2)
rmse2<-sqrt(mse2)
print("MAE of log reg with 70/30 split on skin non skin")
print(mae2)
print("RMSE of log reg with 70/30 split on skin non skin")
print(rmse2)

#10 fold cross validation
model2.1<-train(x1~.,data2,method="lm",trControl=trainControl(method="CV",number = 10,verboseIter = TRUE))

# metric
print("rmse and mae on skin non skin with 10 fold")
print(model2.1)

#random forest on skin non skin

setwd("~/Desktop/DataSets/SkinNonSkin")
library(janitor)
library(caTools)
library(caret)
library(randomForest)
library(lattice)
library(ggplot2)
library(caret)

#import CSV
data3<-read.csv("Skin_Nonskin.csv",sep="\t")
data3<-clean_names(data3)
set.seed(101)

#Split data 70/30
split3<-sample.split(data3$x1,SplitRatio = 0.7)
train_set3<-subset(data,split3==TRUE)
test_set3<-subset(data,split3==FALSE)

#Model for 70/30
model3 = randomForest(x = train_set3[0:2],
                     y = train_set3$x1,
                     ntree = 10) 

#summary(model)
y_pred3<-predict(model,test_set3)
result3<-cbind(y_pred3,test_set3$x1)
colnames(result3)<-c('predicted','actual')
result3<-as.data.frame(result3)
#summary(result)

#root mean square error and mse for 70/30
mae3<-MAE(result3$predicted,result3$actual)
mse3<-mean((result3$predicted-result3$actual)^2)
rmse3<-sqrt(mse3)
print(mae3)
print(rmse3)


#10 fold cross validation
model3.1 <- train(
  x1 ~ ., data3,
  method = "rf",ntree= 10,
  trControl = trainControl(
    method = "cv", number = 2,
    verboseIter = TRUE
  )
)

print(model3.1)

