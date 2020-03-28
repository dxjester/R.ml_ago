
# Author: dxjester
# Comments:  Use the R function kmeans to cluster the points as well as possible

rm(list = ls()) # new environment

library(ggplot2) # for graphing needs

set.seed(0213)

k.max <- 25
irisData <- read.table("iris.txt", header = TRUE, row.names = 1) # data import from working directory
rowNumbers= nrow(irisData) # retrieve number of rows

iris.predictors<- iris[,c(1,2,3,4)] # create new predictor vector set which leaves out the response variable
iris.response<- iris[,"Species"] # create new vector with only response variables

head(iris.predictors)
head(iris.response)

# -------------------------------------------------------------- #
# ----------------  BEGIN 2 x CLUSTER ANALYSIS ----------------- # 
# -------------------------------------------------------------- #
twoClusters<- kmeans(iris.predictors,2) #aplly k-means algorithm with no. of centroids(k)=3
twoClusters$size # gives no. of records in each cluster

par(mfrow=c(2,2), mar=c(5,4,2,2))
plot(iris.predictors[c(1,2)], col=twoClusters$cluster)# Plot to see how Sepal.Length and Sepal.Width data points have been distributed in clusters
plot(iris.predictors[c(1,2)], col=iris.response)# Plot to see how Sepal.Length and Sepal.Width data points have been distributed originally as per "class" attribute in dataset
plot(iris.predictors[c(3,4)], col=twoClusters$cluster)# Plot to see how Petal.Length and Petal.Width data points have been distributed in clusters
plot(iris.predictors[c(3,4)], col=iris.response)

table(twoClusters$cluster,iris.response)


# -------------------------------------------------------------- #
# ----------------  BEGIN 4 x CLUSTER ANALYSIS ----------------- # 
# -------------------------------------------------------------- #
fourClusters<- kmeans(iris.predictors,4) #aplly k-means algorithm with no. of centroids(k)=3
fourClusters$size # gives no. of records in each cluster

par(mfrow=c(2,2), mar=c(5,4,2,2))
plot(iris.predictors[c(1,2)], col=fourClusters$cluster)# Plot to see how Sepal.Length and Sepal.Width data points have been distributed in clusters
plot(iris.predictors[c(1,2)], col=iris.response)# Plot to see how Sepal.Length and Sepal.Width data points have been distributed originally as per "class" attribute in dataset
plot(iris.predictors[c(3,4)], col=fourClusters$cluster)# Plot to see how Petal.Length and Petal.Width data points have been distributed in clusters
plot(iris.predictors[c(3,4)], col=iris.response)

table(fourClusters$cluster,iris.response)

# -------------------------------------------------------------- #
# ----------------  BEGIN 3 x CLUSTER ANALYSIS ----------------- # 
# -------------------------------------------------------------- #
threeClusters<- kmeans(iris.predictors,3, nstart = 5) #aplly k-means algorithm with no. of centroids(k)=3
threeClusters$size # gives no. of records in each cluster

par(mfrow=c(2,2), mar=c(5,4,2,2))
plot(iris.predictors[c(1,2)], col=threeClusters$cluster)# Plot to see how Sepal.Length and Sepal.Width data points have been distributed in clusters
plot(iris.predictors[c(1,2)], col=iris.response)# Plot to see how Sepal.Length and Sepal.Width data points have been distributed originally as per "class" attribute in dataset
plot(iris.predictors[c(3,4)], col=threeClusters$cluster)# Plot to see how Petal.Length and Petal.Width data points have been distributed in clusters
plot(iris.predictors[c(3,4)], col=iris.response)

threeClusters$tot.withinss
table(threeClusters$cluster,iris.response)

# -------------------------------------------------------------- #
# ----------------  BEGIN 3 x CLUSTER ANALYSIS ----------------- # 
# -------------------------------------------------------------- #
threeClusters<- kmeans(iris.predictors,3, nstart = 50) #aplly k-means algorithm with no. of centroids(k)=3
threeClusters$size # gives no. of records in each cluster

par(mfrow=c(2,2), mar=c(5,4,2,2))
plot(iris.predictors[c(1,2)], col=threeClusters$cluster)# Plot to see how Sepal.Length and Sepal.Width data points have been distributed in clusters
plot(iris.predictors[c(1,2)], col=iris.response)# Plot to see how Sepal.Length and Sepal.Width data points have been distributed originally as per "class" attribute in dataset
plot(iris.predictors[c(3,4)], col=threeClusters$cluster)# Plot to see how Petal.Length and Petal.Width data points have been distributed in clusters
plot(iris.predictors[c(3,4)], col=iris.response)

table(threeClusters$cluster,iris.response)

library(readr)
irisModel <- sapply(1:k.max, 
              function(k){
                kmeans(iris.predictors, k, nstart=50,iter.max = 20 )$tot.withinss
              }
)
irisModel
plot(1:k.max, irisModel,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
