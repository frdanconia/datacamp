library(Boruta)
train <- loan_data_expert


boruta.train <- Boruta(loan_status~., data = train, doTrace = 2)


plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)
final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

getSelectedAttributes(final.boruta, withTentative = F)
boruta.df <- attStats(final.boruta)
View(print(boruta.df))
data_f <- data.frame(boruta.df)
data_f= data_f[-c(5,6,7,8,9),]
write.csv(data_f,file="D:/All_POCs/All_POCs/Demand_forecasting/data/imp_table.csv")


train$ORDER_QUANTITY <- as.factor(train$ORDER_QUANTITY)
# install.packages("tree")
library(ctree)
tree <- lm(INVOICE_AMOUNT~., type = gaussian, data = train)
train_tree <- predict(tree, newdata = train)
tree3 <- cbind(train, train_tree)

View(as.data.frame(str(train))
)