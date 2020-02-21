#Script por Alex Escudero M para ACT1 BI UAI Septiembre 2017


url="http://dyzz9obi78pm5.cloudfront.net/app/image/id/56
0d29b532131ca16af2b9e4/n/WA_Fn-UseC_-HR-EmployeeAttrition.csv"
employee=read.csv(url, header = TRUE, sep = ",")

head(employee)
employee<-employee[-c(2,3,5,8,12,16,18,22,23,9,27)]
head(employee)
scaled.employee<-scale(employee)
cor<-cor(scaled.employee)
round(cor,2)
findCorrelation(cor,cutoff=.60,verbose=TRUE)
emp.pca<-prcomp(scaled.employee,scale=FALSE)
names(emp.pca)

#Plot de las 2 primeras PC del prcomp hecho
comp<-data.frame(emp.pca$x[,1:2])
plot(comp,pch=16,col=rgb(0,0,0,0.5))

#Eigenvalues
eig<-(emp.pca$sdev)^2
#Varianzas
variance<-eig*100/sum(eig)
#Varianzas Acumulativas
cumvar<-cumsum(variance)
emp.pca.active<-data.frame(eig=eig,variance=variance,cumvariance=cumvar)
head(emp.pca.active)
#Podemos observar los loadings o variabilidades por PC usando factoextra
library(factoextra)
fviz_pca_var(emp.pca)

#4)
# Kmeans se puede usar con datos estandarizados o no, depende de las medidas de la data misma.
# Aqui usaremos kmeans con la data estandarizada scaled().
library(factoextra)
fviz_nbclust(scaled.employee,kmeans,method="gap_stat")

emp.cluster<-kmeans(scaled.employee, centers=2, iter.max=100, nstart=20)
emp.cluster
plot(scaled.employee, col =(emp.cluster$cluster +1) , main="K-Means result with 2 clusters", pch=20, cex=2)

# Elbow Method para encontrar k optimo en kmeans
mydata<-scaled.employee
wss<-(nrow(mydata)-1)*sum(apply(mydata,2,var))
for(i in 2:15) wss[i]<- sum(kmeans(mydata,centers=i)$withinss)
plot(1:15,wss,type="b",xlab="Number of Clusters",ylab="Within groups sum of squares",main="Assessing the Optimal Number of Clusters with the Elbow Method",pch=20,cex=2)

#5) Graficando Clusters vs Resultados PCA
#Comp es el resultado de PCA aplicado en forma de Dataframe de los dos primeros PC
comp<-data.frame(emp.pca$x[,1:2])
plot(comp, col =(emp.cluster$cluster +1) , main="K-Means clusters vs PCA", pch=20, cex=2)


#6) DBSCAN
library(fpc)
library(dbscan)
#DBSCAN Test 1 eps?0.15, minpts=5
emp.dbscan<-fpc::dbscan(scaled.employee,eps=0.15,MinPts=5)
plot(emp.dbscan,scaled.employee,main="DBSCAN",frame=FALSE)
fviz_cluster(emp.dbscan, scaled.employee, stand = FALSE, ellipse = TRUE, geom = "point")
print(emp.dbscan)
#Optimal eps method
#k-nearest neighbor distances
#Calcular las distancias promedio de todo punto a sus k vecinos mas cercanos
dbscan::kNNdistplot(scaled.employee,k=5)
abline(h=4.9,lty=2)
#Se puede ver que le eps (radio) optimo es en una distancia alrededor de 4.9
emp.dbscan<-fpc::dbscan(scaled.employee,eps=4,MinPts = 4 )
fviz_cluster(emp.dbscan, scaled.employee, stand = FALSE, ellipse = TRUE, geom = "point")

#Parece ser la mejor solucion es tener un eps=4 y un MinPts=4 o menor, el radio coincide con 
#el optimo calculado anteriormente pero la diferencia esta en elegir un minpts de 4 o menor.

#7 Graficando resultados clusters dbscan vs datos obtenidos por PCA
plot(emp.pca.active, col =(emp.dbscan +1) , main="DBSCAN result with 4 clusters", pch=20, cex=2)

#9 $cluster de la variable emp.cluster son las etiquetas de pertenecia de clusters de kmeans aplicado
cluster.belong <- emp.cluster$cluster
belonging<-cluster.belong
employee2<-cbind(employee, belonging)
employee2.kmeans<-kmeans(employee2, centers=2, iter.max=100, nstart=20)
plot(employee2$belonging, employee2$MonthlyIncome)

#10
plot(employee2$JobRole, employee2$MonthlyIncome)
plot(employee2$JobRole, employee2$belonging, main="JobRole Cluster Belonging")
library(scatterplot3d)
attach(mtcars)
scatterplot3d(employee2$MonthlyIncome, employee2$belonging, employee2$JobRole)

#11
hist(employee2$TotalWorkingYears)
