
clickpath = read.csv(file.choose(),sep=',')
head (clickpath)
colnames(clickpath) <- c("seq","gender","last","age","city","zip","digit","date","pick")
str(clickpath)
summary(clickpath)
clickpath[!complete.cases(clickpath),]
clickpath[,!complete.cases(clickpath)]
clickpath <- na.omit(clickpath,na.action=TRUE)
mydata <- clickpath[,c(4,6:7)]
head(mydata)
boxplot(mydata)
boxplot(mydata[,c(1)])
plot(mydata[,c(1)])
install.packages("data.table")
library(data.table)
outlierReplace = function(dataframe, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dataframe, rows, cols, newValue)
  }
}
outlierReplace(clickpath, "unemp_rate", which(mydata$unemp_rate > 1.5), 1.5)
fivenum(mydata$unemp_rate)
boxplot(mydata[,c(3)])
mydata <- scale(mydata[,1:3])
head(mydata)
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for(i in 2:15)wss[i]<- sum(fit=kmeans(mydata,centers=i,15)$withinss)
plot(1:15,wss,type="b",main="15 clusters",xlab="no. of cluster",ylab="with clsuter sum of squares")
fit <- kmeans(mydata,3)
fit$withinss
fit$betweenss
fit$size 
plot(mydata,col=fit$cluster,pch=15)
points(fit$centers,col=1:8,pch=3)
install.packages('cluster')
install.packages('fpc')
library(cluster)
library(fpc)
plotcluster(mydata,fit$cluster)
points(fit$centers,col=1:8,pch=16)
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
mydata <- clickpath[,c(4,6:7)]
mydata <- data.frame(mydata,fit$cluster)
cluster_mean <- aggregate(mydata[,1:4],by = list(fit$cluster),FUN = mean)
cluster_mean