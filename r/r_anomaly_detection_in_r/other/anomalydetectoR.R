set.seed(9810232)

d <- as.dataset.frame(rbind(cbind(rnorm(50,2,1),rnorm(50,2,1),rep(1,50)),
                         cbind(rnorm(38,2,1),rnorm(38,6,0.5),rep(2,38)),
                         cbind(rnorm(76,5,1),rnorm(76,3,1),rep(3,76))))

colnames(d) <- c("x","y","class")
d$class <- as.factor(d$class)
ggplot(d, aes(x=x,y=y,color=as.factor(class))) + geom_point()

initAssignment <- function(x, k){
  sample(1:x, k)
}

dist.euc <- function(x, pt=0){
  x <- sweep(x[,unlist(lapply(x, is.numeric))],2,pt) * sweep(x[,unlist(lapply(x, is.numeric))],2,pt)
  sqrt(apply(x,1,sum))
}

dist.gow <- function(x,pt=0){
  diff_ <- abs(sweep(x[,unlist(lapply(x, is.numeric))],2,pt[unlist(lapply(x, is.numeric))]))
  range_ <- apply(sapply(x[,unlist(lapply(x, is.numeric))],range),2,diff)
  x$numeric_distance <- sweep(diff_,2,range_,'/')
  x$factor_distance <- 1-as.integer(x[unlist(lapply(x, is.factor))]==h[unlist(lapply(x, is.factor))])
  apply(cbind(x$numeric_distance,x$factor_distance), 2, sum)
  
}


km <- function(dataset, k, max.iter = 1000, metric=dist.gow){
  dataset <- as.data.frame(dataset)
  inGr <- initAssignment(dim(dataset)[1], k)
  dataset$class__ <- 0
  dataset[initAssignment(dim(dataset)[1], k),]$class__ <- 1:k
  
  metric(dataset, pt=subset(dataset,class__==1))
  
}

km(d,10)


