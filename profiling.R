user = read.csv(file.choose(), sep = ",")
head(user)
ds1=data.frame(user$age, user$digit)
head(ds1)
scale(ds1)
library(tidyverse)  
library(cluster)    
library(factoextra)
set.seed(123)
fviz_nbclust(don.acm$li, kmeans, method = "wss")
fviz_nbclust(don.acm$co, kmeans, method = "silhouette")
gap_stat <- clusGap(ds1, FUN = kmeans, nstart = 25,K.max = 10, B = 50)
fviz_gap_stat(gap_stat)
final <- kmeans(ds1, 2, nstart = 25)
print(final)
ACM #
install.packages('ade4')
library(ade4)
don.acm <- dudi.coa(df = ds1, scannf = FALSE, nf = 4)

names(don.acm) # les attributs
don.acm$tab # les variavles

don.acm$li # les individus
plot(don.acm)
fit <- kmeans(don.acm$tab,2)
plot(don.acm$tab,col=fit$cluster,pch=15)

par(mfrow=c(2,2))
hist(ds1)
