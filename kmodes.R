data = read.csv(file.choose(),sep=',')

head(data)
library(klaR)

ds1=data.frame(data$gender, data$digit)

install.packages("factoextra")
library(factoextra)

cluster.results <- kmodes(ds1, 3, iter.max = 10, weighted = FALSE)

plot(ds1,col= cluster.results$cluster)



df <- scale(data) # Scaling the data
ds1=data.frame(data$city,data$age, data$zip)

# View the firt 3 rows of the data
head(ds1, n = 3)
set.seed(123)
distance <- get_dist(ds1)

km.res <- kmeans(distance, 4, nstart = 25)
print(km.res)
aggregate(data, by=list(cluster=km.res$cluster), mean)
dd <- cbind(data, cluster = km.res$cluster)
head(dd)
km.res$cluster
head(km.res$cluster, 4)
km.res$size
km.res$centers
plot(km.res, km.res$centers,)
fviz_cluster(cluster.results, data=ds1)


