 

library(datasets)
library(cluster)
library(fpc)
library(clue)
library(pvclust)

data("iris")
attach(iris)
View(iris)
 

#Randomising the dataset
indices <- sample(1:nrow(iris),nrow(iris), replace=FALSE)

#Removing the classification variable
iris.data <- as.data.frame(iris[indices,-5])


#############################################################################################################################
#K - means Clustering method
#############################################################################################################################


#Exploring on the Possible number of clusters: Note the Unsupervised Learning!
clusplot(pam(iris.data, 3, metric = "euclidean",stand = TRUE), main = "Cluster Plot For the Iris Data") #3- Clusters
#Note the point Variability Explained by the components.
#You may try increasing the clusters




# Clustering
cluster.model.K.Means <- kmeans(iris.data, 3) # 3 cluster solution
# get cluster means
aggregate(iris.data,by=list(cluster.model.K.Means$cluster),FUN=mean)

model.clusters <- as.data.frame(cluster.model.K.Means$cluster)
View(model.clusters)

#Merging the two datsets
merged.data <- merge(iris.data , model.clusters , by="row.names")
attach(merged.data)
View(merged.data)

#Partioning the Ploting Window
par(mfrow=c(2,2))
boxplot(Petal.Length~cluster.model.K.Means$cluster )
boxplot(Petal.Width~cluster.model.K.Means$cluster )
boxplot(Sepal.Length~cluster.model.K.Means$cluster )
boxplot(Sepal.Width~cluster.model.K.Means$cluster )





