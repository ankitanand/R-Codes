################################################################################
# Clustering
# Data Mining
# Matthew A. Lanham
################################################################################
# Load data
getwd()
setwd("C:\\Users\\malan\\Dropbox\\_Purdue\\_Teaching\\DM571\\8_Clustering\\2_KohonenNetworks")
#setwd("C:\\Users\\Matthew A. Lanham\\Dropbox\\_Purdue\\_Teaching\\DM\\7_Clustering")
library(kohonen)
# reading in the churn txt file from the web
myUrl <- "http://dataminingconsultant.com/data/churn.txt"
churn <- read.table(file=myUrl, header=T, sep=",", quote="")
names(churn)

# dummy/flag variables
IntPlan <- ifelse(churn$Int.l.Plan == "yes",1,0)
VMPlan <- ifelse(churn$VMail.Plan == "yes",1,0)
Churn <- ifelse(churn$Churn. == "True.",1,0)

# standardize numeric features
names(churn)
churn_z <- scale(churn[,7:20])
churn_z <- data.frame(scale(churn_z))

# The som() function that creates the Kohonen network requires that the
# input data is a matrix, so he we create a our input matrix
df <- cbind(IntPlan, VMPlan, churn_z)
mynames <- names(df)
df <- as.matrix(df); colnames(df) <- mynames
str(df)

# fit the kohonen network/SOM cluster algorithm
library(kohonen)
som.6 <- som(df 
             , grid = somgrid(3,2)    # a grid for the representatives
             , rlen = 170             # number of times the complete data set will be presented to the network
             , alpha = c(0.3, 0.00)   # learning rate, a vector of two numbers indicating the amount of change
             , radius = 2             # the radius of the neighbourhood
             )

# plot the make-up of each cluster
#plot(som.6
#     , type=c("codes")
#     , palette.name = rainbow
#     , main = "Cluster content"
#     )

# plot the counts in each cluster
plot(som.6
     , type=c("counts")
     , palette.name = rainbow
     , main = "Cluster counts"
     )
# count of observations in each cluster
table(som.6$unit.classif)

# plot make-up of clusters against voice main plan to see if there is a story by 
# each group
churn$cluster <- som.6$unit.classif  # winning clusters

som.6$grid$pts      # plot locations
coords <- matrix(0, ncol=2, nrow=nrow(df))
for (i in 1:nrow(df)){
    coords[i,] <- som.6$grid$pts[som.6$unit.classif[i],]
}
pchVMPlan <- ifelse(df[,2]==0, 1, 16)
colVMPlan <- ifelse(df[,2]==0, 1, 2)
plot(jitter(coords), main="Kohonen Network colored by VM Plan"
     , xlab="", ylab=""
     , col=colVMPlan
     , pch=pchVMPlan)

# table of percent churn by cluster
c.table <- table(Churn, som.6$unit.classif)
round(prop.table(c.table,2)*100, 2)


################################################################################
# another self organizing maps package
# https://cran.r-project.org/web/packages/som/som.pdf
################################################################################
## remove (almost) everything in the working environment.
## You will get no warning, so don't do this unless you are really sure.
rm(list = ls())



################################################################################
# Chapter 22
################################################################################
data(iris)
head(iris)

# use clusterSim to easily do variable transformations.
# Min-Max Normalization (aka unitization with zero minimum)
library(clusterSim)
iris_z <- data.Normalization(iris[,1:4], type="n4")
# change variable names so they have a "_z" after them
for (i in 1:ncol(iris_z)) {
    names(iris_z)[i] <- paste0(names(iris_z)[i],"_z")
}
names(iris_z)

################################################################################
# Silhouette values
library(cluster)
# kmeans (k=3)
km3 <- kmeans(iris_z, 3)
dist3 <- dist(iris_z, method="euclidean")
sil3 <- silhouette(km3$cluster, dist3)
plot(sil3, col=c("black","red","green"), main="Silhouette plot (k=3) K-means")

# kmeans (k=2)
km2 <- kmeans(iris_z, 2)
dist2 <- dist(iris_z, method="euclidean")
sil2 <- silhouette(km2$cluster, dist2)
plot(sil2, col=c("black","red"), main="Silhouette plot (k=2) K-means")

################################################################################
# visualizing clusters using scatterplot3d package
library(scatterplot3d)
par(mfrow=c(2,2))
scatterplot3d(x = iris_z[,1], y = iris_z[,2], z = iris_z[,3]
              , color = "black", angle=40, cex.symbols=1.3, pch=21
              , main="k-Means (k=2)"
              , xlab=names(iris_z)[1], ylab=names(iris_z)[2]
              , zlab=names(iris_z)[3], bg=km2$cluster)
scatterplot3d(x = iris_z[,3], y = iris_z[,4], z = iris_z[,1]
              , color = "black", angle=40, cex.symbols=1.3, pch=21
              , main="k-Means (k=2)"
              , xlab=names(iris_z)[3], ylab=names(iris_z)[4]
              , zlab=names(iris_z)[1], bg=km2$cluster)
scatterplot3d(x = iris_z[,1], y = iris_z[,2], z = iris_z[,3]
              , color = "black", angle=40, cex.symbols=1.3, pch=21
              , main="k-Means (k=3)"
              , xlab=names(iris_z)[1], ylab=names(iris_z)[2]
              , zlab=names(iris_z)[3], bg=km3$cluster)
scatterplot3d(x = iris_z[,3], y = iris_z[,4], z = iris_z[,1]
              , color = "black", angle=40, cex.symbols=1.3, pch=21
              , main="k-Means (k=3)"
              , xlab=names(iris_z)[3], ylab=names(iris_z)[4]
              , zlab=names(iris_z)[1], bg=km3$cluster)
################################################################################
# k-means clustering
set.seed(1234) # use this to replicate results
#run kmeans for diff values of k so we can identify performance by # of clusters
cost_df <- data.frame() #accumulator for cost results
for(k in 1:15){
    #allow up to 50 iterations for convergence, and do 5 random starts
    kmeans <- kmeans(x=iris_z, centers=k, nstart=5, iter.max=50)
    
    #Combine cluster number and cost together, write to df
    cost_df <- rbind(cost_df, cbind(k, kmeans$tot.withinss))
}
names(cost_df) <- c("cluster", "cost")

# create an elbow plot
par(mfrow=c(1,1))
cost_df$cost <- cost_df$cost/1000
plot(cost_df, main="k-Means Elbow Plot", col="blue", pch=19, type="b"
     , xlab="Number of Clusters", ylab="MSE (in 1000s)", cex.lab=1.2)
################################################################################
# plot silhouette values for k=3 clusters
names(iris_z)
sil3 <- sil3[order(sil3[,"cluster"], -sil3[,"sil_width"]),] 

par(mfrow=c(1,1))
plot(sil3[which(sil3[,"cluster"]=="1"),"sil_width"], col="blue", pch=19, type="b"
     , ylim=c(0,max(sil3[,"sil_width"])), ylab="Silhouette values", cex.lab=1
     , xlab="Points in each cluster", xlim=c(0,max(table(sil3[,"cluster"])))
     , main="Silhouette values by cluster")
points(sil3[which(sil3[,"cluster"]=="2"),"sil_width"], col="red", pch=19, type="b")
points(sil3[which(sil3[,"cluster"]=="3"),"sil_width"], col="green", pch=19, type="b")
legend("bottomleft", col=c("blue","red","green"), pch=19
       , legend=c("cluster1","cluster2","cluster3"))

# average silhouette values by cluster
avgSilValues <- aggregate(sil3[,"sil_width"], by=list(sil3[,"cluster"]), FUN=mean)
names(avgSilValues) <- c("Cluster","Avg Silhouette")
avgSilValues

################################################################################
# plot silhouette values
names(iris_z)
par(mfrow=c(1,2))
silva3 <- ifelse(sil3[,3] <= 0.33, 0, 1)
plot(iris_z$Petal.Length_z, iris_z$Petal.Width_z
     , col=silva3 + 1, pch=16, main="Silhouette Values (k=3)"
     , xlab="Petal length (min-max)", ylab="Petal width (min-max)")
legend("topleft", col=c(1,2), pch=16, legend=c("<=33",">33"))

silva2 <- ifelse(sil2[,3] <= 0.33, 0, 1)
plot(iris_z$Petal.Length_z, iris_z$Petal.Width_z
     , col=silva2 + 1, pch=16, main="Silhouette Values (k=2)"
     , xlab="Petal length (min-max)", ylab="Petal width (min-max)")
legend("topleft", col=c(1,2), pch=16, legend=c("<=33",">33"))

################################################################################
# plot silhouette values for k=2 clusters
names(iris_z)
sil2 <- sil2[order(sil2[,"cluster"], -sil2[,"sil_width"]),] 

par(mfrow=c(1,1))
plot(sil2[which(sil2[,"cluster"]=="1"),"sil_width"], col="blue", pch=19, type="b"
     , ylim=c(0,max(sil2[,"sil_width"])), ylab="Silhouette values", cex.lab=1
     , xlab="Points in each cluster", xlim=c(0,max(table(sil2[,"cluster"])))
     , main="Silhouette values by cluster")
points(sil2[which(sil2[,"cluster"]=="2"),"sil_width"], col="red", pch=19, type="b")
legend("bottomleft", col=c("blue","red"), pch=19
       , legend=c("cluster1","cluster2"))

# average silhouette values by cluster
avgSilValues <- aggregate(sil2[,"sil_width"], by=list(sil2[,"cluster"]), FUN=mean)
names(avgSilValues) <- c("Cluster","Avg Silhouette")
avgSilValues

################################################################################
# Generating silhouette plots another way for k-means (k=2 and k=3)
library(cluster)
# kmeans (k=3)
km3 <- kmeans(iris_z, 3)
dist3 <- dist(iris_z, method="euclidean")
sil3 <- silhouette(km3$cluster, dist3)
plot(sil3, col=c("black","red","green"), main="Silhouette plot (k=3) K-means")

# kmeans (k=2)
km2 <- kmeans(iris_z, 2)
dist2 <- dist(iris_z, method="euclidean")
sil2 <- silhouette(km2$cluster, dist2)
plot(sil2, col=c("black","red"), main="Silhouette plot (k=2) K-means")

################################################################################
#Using the pseudo-F statistic to determine best clustering solution
library(clusterSim)
#pseudo F-statistics
psF3 <- index.G1(x=iris_z, cl=km3$cluster) #k=3
psF2 <- index.G1(x=iris_z, cl=km2$cluster) #k=2
#calculate p-values
pvalue_k3 <- pf(psF3, df1=3-1, df2=nrow(iris_z)-2, lower.tail=F) #k=3
pvalue_k2 <- pf(psF2, df1=2-1, df2=nrow(iris_z)-1, lower.tail=F) #k=2
#whichever p-value is smallest is the best clustering solution
pvalues <- data.frame(cbind(c("pvalue_k3","pvalue_k2"),c(pvalue_k3,pvalue_k2)))
names(pvalues) <- c("Solution","p-value")
pvalues 

# here k=3 is preferred because the p-value is smallest
#   Solution              p-value
#1 pvalue_k3  1.44671042796269e-57
#2 pvalue_k2  3.1746054567459e-41
################################################################################
# Hartigan's rule
library(useful)
hart <- FitKMeans(iris_z, max.clusters=15, nstart=20, seed=1234)
hart
PlotHartigan(hart)

################################################################################
#Cluster validation
# create a training and testing set
set.seed(1234)
rows = sample(1:nrow(iris_z), round(nrow(iris_z)*.7,0))
train = iris_z[rows, ]
test = iris_z[-rows, ]

# perform k-means on training set
library(flexclust)
km2 <- kcca(x=train, k=2, family=kccaFamily("kmeans"))
km3 <- kcca(x=train, k=3, family=kccaFamily("kmeans"))

#### OPTION 1 ####
# predict cluster assignment using our build models (km2 and km3) using data
# from the test set
test$clust2 <- predict(km2, test[,1:4])
test$clust3 <- predict(km3, test[,1:4])

# average silhouette values by cluster on test data set
# k=2
km2_train <- data.frame(rep("train k=2",nrow(km2@"centers"))
                        ,cbind(c(1:nrow(km2@"centers")), km2@"centers"))
km2_test <- data.frame(rep("test k=2",nrow(km2@"centers")),
                       aggregate(test[,1:4], by=list(test[,"clust2"]), FUN=mean))
names(km2_train)[1:2] <- c("Dataset","Cluster")
names(km2_test)[1:2] <- c("Dataset","Cluster")
# k=3
km3_train <- data.frame(rep("train k=3",nrow(km3@"centers"))
                        ,cbind(c(1:nrow(km3@"centers")), km3@"centers"))
km3_test <- data.frame(rep("test k=3",nrow(km3@"centers")),
                       aggregate(test[,1:4], by=list(test[,"clust3"]), FUN=mean))
names(km3_train)[1:2] <- c("Dataset","Cluster")
names(km3_test)[1:2] <- c("Dataset","Cluster")
# all results merged together - want this table to compare train and test stats
# for each model and cluster
results <- rbind(km2_train, km2_test, km3_train, km3_test)
results

# visualize cluster centers - should make it easier to compare
source("multiplot.R")
library(ggplot2)
p1=ggplot(results, aes(x=Dataset, y=Sepal.Length_z, fill=Cluster)) + geom_bar(stat="identity")
p2=ggplot(results, aes(x=Dataset, y=Sepal.Width_z, fill=Cluster)) + geom_bar(stat="identity")
p3=ggplot(results, aes(x=Dataset, y=Petal.Length_z, fill=Cluster)) + geom_bar(stat="identity")
p4=ggplot(results, aes(x=Dataset, y=Petal.Width_z, fill=Cluster)) + geom_bar(stat="identity")
multiplot(p1,p2,p3,p4)

# perform k-means on test set
library(flexclust)
te_km2 <- kcca(x=test[,1:4], k=2, family=kccaFamily("kmeans"))
te_km3 <- kcca(x=test[,1:4], k=3, family=kccaFamily("kmeans"))

#### OPTION 2 ####
# k=2
km2_train <- data.frame(rep("train k=2",nrow(km2@"centers"))
                        ,cbind(c(1:nrow(km2@"centers")), km2@"centers"))
km2_test2 <- data.frame(rep("test k=2",nrow(te_km2@"centers"))
                       ,cbind(c(1:nrow(te_km2@"centers")), te_km2@"centers"))
names(km2_train)[1:2] <- c("Dataset","Cluster")
names(km2_test2)[1:2] <- c("Dataset","Cluster")
# k=3
km3_train <- data.frame(rep("train k=3",nrow(km3@"centers"))
                        ,cbind(c(1:nrow(km3@"centers")), km3@"centers"))
km3_test2 <- data.frame(rep("test k=3",nrow(te_km3@"centers"))
                       ,cbind(c(1:nrow(te_km3@"centers")), te_km3@"centers"))
names(km3_train)[1:2] <- c("Dataset","Cluster")
names(km3_test2)[1:2] <- c("Dataset","Cluster")
# all results merged together - want this table to compare train and test stats
# for each model and cluster
results2 <- rbind(km2_train, km2_test2, km3_train, km3_test2)
results2 

# visualize cluster centers - should make it easier to compare
source("multiplot.R")
library(ggplot2)
p1=ggplot(results2, aes(x=Dataset, y=Sepal.Length_z, fill=Cluster)) + geom_bar(stat="identity")
p2=ggplot(results2, aes(x=Dataset, y=Sepal.Width_z, fill=Cluster)) + geom_bar(stat="identity")
p3=ggplot(results2, aes(x=Dataset, y=Petal.Length_z, fill=Cluster)) + geom_bar(stat="identity")
p4=ggplot(results2, aes(x=Dataset, y=Petal.Width_z, fill=Cluster)) + geom_bar(stat="identity")
multiplot(p1,p2,p3,p4)
