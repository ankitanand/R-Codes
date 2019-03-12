setwd("D:\\Data Mining\\HWs\\HW3")
h <- read.table(file = "household_data.csv", header=T, fill=TRUE, sep=",")
i <- read.table(file = "individual_data.csv", header=T, fill=TRUE, sep=",")
#r <- read.table(file = "registration_status.csv", header=T, fill=TRUE, sep=",")
r <- read.table(file = "registration_status.csv", header=T, fill=TRUE, sep="|")

# examine structure of data
str(h)
str(i)
str(r)

# coerce variables to different data types
h$is_urban <- as.factor(h$is_urban)
h$is_owner_of_home <- as.factor(h$is_owner_of_home)
h$tercile_of_census_tract_income <- as.factor(h$tercile_of_census_tract_income)
i$is_head_of_household <- as.factor(i$is_head_of_household)
i$married <- as.factor(i$married)
i$gender <- as.factor(i$gender)
i$race <- as.factor(i$race)
i$voted_in_2012 <- as.factor(i$voted_in_2012)
i$is_college_graduate <- as.factor(i$is_college_graduate)
r$is_registered_democrat <- as.factor(r$is_registered_democrat)

# distribution of response variable
table(r$is_registered_democrat)

# join the datasets into one (could also use SQL if you want)
library(plyr)
d <- join(x=r, y=i, by="person_id", type="inner")
d <- join(x=d, y=h, by="hh_id", type="inner")

rm(h,i,r) # clean up environment

# re-arrange columns. Want target as first column
head(d)
d <- d[,c(2,1,3:14)]

counter <- 0.1

# change column names
names(d)
names(d)[1] = "y"
names(d)[4] = "hoh"
names(d)[10] = "college_grad"
names(d)[13] = "home_owner"
names(d)[14] = "income_tercile"
names(d)

# change 'NA' to actually missing value NA; and make 1/2s as 0/1s
d$y <- ifelse(d$y == "NA", NA, d$y)
d$y <- ifelse(d$y == 1, 0, d$y)
d$y <- ifelse(d$y == 2, 1, d$y)
table(d$y)
d$y <- as.factor(d$y)


d1 <- data.frame(d[,6])

#DATA Cleaning
#Converting all values to UPPER CASE
library(dplyr)
d1 <- mutate_all(d1, funs(toupper))



#Changing values of Gender to Have only Male & Female
names(d1)[1] = "gender"
d1$gender <- ifelse(d1$gender == "M", "MALE", d1$gender)
d1$gender <- ifelse(d1$gender == "F", "FEMALE", d1$gender)


d$gender <- d1$gender
rm(d1)

# for ppl whose age is <7, they cannot be college grad
d$college_grad[d$age<7] <- 0

#Q6 Make names and relevel your response variable using R
d$y <- ifelse(d$y == 1, "YES", "NO")
d$y <- as.factor(d$y)
d$y<-relevel(d$y,"YES")


# 'd' dataset and 'score_set'
score_set <- d[is.na(d$y),]
d <- d[!is.na(d$y),]



write.csv(d, file = "MyData.csv")


#Q1 K-means
#chosing only numeric variables for k-means clustering
str(d)
dnum<-d[,c(8,11)] 

# get correlations matrix
M <- cor(dnum) 
library('corrplot') #package corrplot
corrplot(M, method = "circle") #plot matrix

#scaling variables
dfz <- scale(dnum) 
dfz <- data.frame(scale(dfz))

names(dfz)
#Q1 K-means clustering
#make train and test sets for k-means
set.seed(1234)  

rows = sample(1:nrow(dfz), round(nrow(dfz)*.5,0)) #identifying rows

traink <- dfz[rows,] #train set created
testk<- dfz[-rows,]  #test set created

cost_df <- data.frame() #accumulator for cost results
cost_df
for(k in 1:25){
  # train set
  kmeans_tr <- kmeans(x=traink, centers=k, nstart=20, iter.max=100)
  # test set
  kmeans_te <- kmeans(x=testk, centers=k, nstart=20, iter.max=100)
  
  #Combine cluster number and cost together, write to df
  cost_df <- rbind(cost_df, cbind(k, kmeans_tr$tot.withinss
                                  , kmeans_te$tot.withinss))
}


# the cost_df data.frame contains the # of clusters k and the MSE for each cluster
names(cost_df) <- c("cluster", "tr_cost", "te_cost")
cost_df

par(mfrow = c(1,1))
cost_df[,2:3]<-cost_df[,2:3]/1000
plot(x=cost_df$cluster, y=cost_df$tr_cost, main="k-Means Elbow Plot"
     , col="blue", pch=19, type="b", cex.lab=1.2
     , xlab="Number of Clusters", ylab="MSE (in 1000s)")
points(x=cost_df$cluster, y=cost_df$te_cost, col="green")

#Q2 Generate Silhouette
# Silhouette values
  library(cluster)
# kmeans (k=3)
km3 <- kmeans(traink, 3)
dist3 <- dist(traink, method="euclidean")
sil3 <- silhouette(km3$cluster, dist3)
plot(sil3, col=c("black","red","green"), main="Silhouette plot (k=3) K-means",border = NA)
# we get Avg Silhouette as 0.7

# kmeans (k=4)
km4 <- kmeans(traink, 4)
dist4 <- dist(traink, method="euclidean")
sil4 <- silhouette(km4$cluster, dist4)
plot(sil4, col=c("black","red","green"), main="Silhouette plot (k=4) K-means",border = NA)
# we get Avg Silhouette as 0.51

# kmeans (k=5)
km5 <- kmeans(traink, 5)
dist5 <- dist(traink, method="euclidean")
sil5 <- silhouette(km5$cluster, dist5)
plot(sil5, col=c("black","red","green"), main="Silhouette plot (k=5) K-means",border = NA)
# we get Avg Silhouette as 0.46


#Q-3 cluster validation chosen k=3 from silhouette width
dNums <- dnum
rows = sample(1:nrow(dNums), round(nrow(dNums)*.5,0))

set.seed(1234)

train_kmeans <- dNums[rows,]  # training data set
test_kmeans <- dNums[-rows,]  # testing data set

library(flexclust)
km3 <- kcca(x=train_kmeans, k=3, family=kccaFamily("kmeans"))

test_kmeans$clust3 <- predict(km3, test_kmeans[,1:2])

km3_train <- data.frame(rep("train k=3",nrow(km3@"centers"))
                        ,cbind(c(1:nrow(km3@"centers")), km3@"centers"))
km3_test <- data.frame(rep("test k=3",nrow(km3@"centers")),
                       aggregate(test_kmeans[,1:2], by=list(test_kmeans[,"clust3"]), FUN=mean))

names(km3_train)[1:2] <- c("Dataset","Cluster")
names(km3_test)[1:2] <- c("Dataset","Cluster")

results <- rbind(km3_train, km3_test)
results

kmeans_tr <- kmeans(x=train_kmeans, centers=3, nstart=20, iter.max=100)
kmeans_te <- kmeans(x=test_kmeans, centers=3, nstart=20, iter.max=100)

train_kmeans$cluster <- kmeans_tr$cluster
test_kmeans$cluster <- kmeans_te$cluster

# Cluster-wise plot of age v/s hh_income
library(ggplot2)
ggplot(data=train_kmeans, aes(x=age, y=hh_income, color=cluster)) + geom_point(stat = "identity")


#Q-5 EDA
#Q5 A TABLEAU Dashboard has also been submitted for the same purpose
plot(d$y, d$age, main="y vs age")
plot(d$y, d$hh_income, main= "y vs income")
plot(d$y, d$hoh, main="y vs hoh")
plot(d$y, d$college_grad, main="y vs College_grad")
plot(d$y, d$married, main="y vs Married/Single")
plot(d$y, d$voted_in_2012, main="Voted in 2012")
plot(d$y, d$is_urban, main="Urban or not")
plot(d$y, d$income_tercile, main="Income Tercile")


#Q4
#Data Preprocessing
################################################################################
## Handling missing values
################################################################################
# % of rowing having missing values
dim(d[!complete.cases(d),])[[1]]/nrow(d)*100

# missing value report
source("DataQualityReport.R")
DataQualityReport(d)

################################################################################
## Creating Dummy Variables
################################################################################
# Here we want to take a 'factor variable' or 'categorical variable' and create
# columns for each factor level.
library(caret)
dummies <- dummyVars(y ~ ., data = d)            # create dummyes for Xs
ex <- data.frame(predict(dummies, newdata = d))  # actually creates the dummies
names(ex) <- gsub("\\.", "", names(ex))          # removes dots from col names
d <- cbind(d$y, ex)                              # combine your target variable with Xs
names(d)[1] <- "y"                               # make target variable called 'y'
rm(dummies, ex)                                  # delete temporary things we no longer need


################################################################################
# Remove Zero- and Near Zero-Variance Predictors
################################################################################
# Can you imagine having an X (independent) variable that contained exactly the
# same value for all its records? Do you think it would help you predict y?
# Obviously it would not help. Thus, we would want to delete features like this.
# We would also want to delete features that almost always have the same value.
# This is the idea behind zero-variance (i.e. same exact value) and near-zero-variance
# (i.e. almost the same value). We easily identify those features in this code
# chunk and remove them
library(caret)
dim(d) # dimension of dataset
nzv <- nearZeroVar(d[,2:ncol(d)], uniqueCut=10) # identify columns that are "near zero"
d_filtered <- d[,2:ncol(d)][, -nzv]            # remove those columns from your dataset
dim(d_filtered)                                # dimension of your filtered dataset

rm(d_filtered)


################################################################################
# Identify Correlated Predictors and remove them
################################################################################
# If you build a model that has highly correlated independent variables it can
# lead to unstable models because it will tend to weight those more even though
# they might not be that important

# calculate correlation matrix using Pearson's correlation formula
descrCor <-  cor(d[,2:ncol(d)])                           # correlation matrix
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .90) # number of Xs having a corr > some value
summary(descrCor[upper.tri(descrCor)])                    # summarize the correlations

# which columns in your correlation matrix have a correlation greater than some
# specified absolute cutoff?
highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.90)
filteredDescr <- d[,2:ncol(d)][,-highlyCorDescr] # remove those specific columns from your dataset
descrCor2 <- cor(filteredDescr)                  # calculate a new correlation matrix

# summarize those correlations to see if all features are now within our range
summary(descrCor2[upper.tri(descrCor2)])

# update our d dataset by removing those filtered variables that were highly correlated
d <- cbind(d$y, filteredDescr)
names(d)[1] <- "y"

rm(filteredDescr, descrCor, descrCor2, highCorr, highlyCorDescr)  # clean up

################################################################################
# Identifying linear dependencies and remove them
################################################################################
# Find if any linear combinations exist and which column combos they are.
# Below I add a vector of 1s at the beginning of the dataset. This helps ensure
# the same features are identified and removed.

library(caret)
# first save response
y <- d$y

# create a column of 1s. This will help identify all the right linear combos
d <- cbind(rep(1, nrow(d)), d[2:ncol(d)])
names(d)[1] <- "ones"

# identify the columns that are linear combos
comboInfo <- findLinearCombos(d)
comboInfo

# remove columns identified that led to linear combos
d <- d[, -comboInfo$remove]

# remove the "ones" column in the first column
d <- d[, c(2:ncol(d))]

# Add the target variable back to our data.frame
d <- cbind(y, d)

rm(y, comboInfo)  # clean up


################################################################################
# Standardize (and/ normalize) your input features.
################################################################################
# Here we standardize the input features (Xs) using the preProcess() function 
# by performing a typical Z-score standardization (method = c("center","scale")))
# This will make all the numeric features centered at 0 and have a standard
# deviation of 1. 
#
# As shown in previous class examples, we could have tried a min-max normalization
# ("range") and even tried to make the features more bell-shaped by doing a
# "YeoJohnson" transformation. If you wanted to do those things, you would just
# modify the method like so: method = c("range","YeoJohnson")

# To make sure I do not standardize the dummy variables I'll create a set that 
# contains the 0/1 variables (dCats) and the numeric features (dNums) 
numcols <- apply(X=d, MARGIN=2, function(c) sum(c==0 | c==1)) != nrow(d)
catcols <- apply(X=d, MARGIN=2, function(c) sum(c==0 | c==1)) == nrow(d)
dNums <- d[,numcols]
dCats <- d[,catcols]

# Step 1) figures out the means, standard deviations, other parameters, etc. to 
# transform each variable
preProcValues <- preProcess(dNums[,2:ncol(dNums)], method = c("center","scale"))

# Step 2) the predict() function actually does the transformation using the 
# parameters identified in the previous step. Weird that it uses predict() to do 
# this, but it does!
dNums <- predict(preProcValues, dNums)

# combine the standardized numeric features with the dummy vars
d <- cbind(dNums, dCats)
d$person_id <- NULL

rm(preProcValues, numcols, catcols, dNums, dCats)  # clean up


#Q7
#perform 5-fold cross-validation and doing a classification problem
library(caret)
ctrl <- trainControl(method="cv",     # cross-validation set approach to use
                     number=5,        # k number of times to do k-fold
                     classProbs = T,  
                     summaryFunction = defaultSummary,
                     allowParallel=T)


#set.seed(1234) # set a seed so you can replicate your results

# identify records that will be used in the training set. Here we are doing a
# 70% train/ 30% test split. You might modify this.
inTrain <- createDataPartition(y = d$y,   # outcome variable
                               p = .70,   # % of training data you want
                               list = F)
# create your partitions
d_train <- d[inTrain,]  # training data set
d_test <- d[-inTrain,]  # test data set

#Q8 build a logistic regression model
logisticmodel <- train(y ~ .,
                  data = d_train,
                  method = "glm",
                  trControl = ctrl,
                  #preProcess=c("center","scale"), # not needed; already transformed
                  tuneLength = 15,                # this specifies various s values
                  metric = "Accuracy")
summary(logisticmodel)

#Q11 Linear Model
#predicted probabilities and predicted classes 
# Capture the train and test estimated probabilities and predicted classes
# model 1 
lm_trp <- predict(logisticmodel, newdata=d_train, type='prob')[,1]
lm_trc <- predict(logisticmodel, newdata=d_train)

#Q9 Features to select 
# Employing RECURSIVE FEATURE SELECTION
subsets <- c(1:5, 10, 15, 20, 25)
y=ifelse(d_train$y == "YES",1,0)
ctrl <- rfeControl(functions = lmFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

lmProfile <- rfe(d_train, y,
                 sizes = subsets,
                 rfeControl = ctrl)

lmProfile



#Q12 Logistic Model
# logistic model confusionMatrix
(cm_lm <- confusionMatrix(data=lm_trc, d_train$y))

#Q12 Logistic Model
#AUC
library(pROC)
b=ifelse(d_train$y == "YES",1,0)
auc(b,lm_trp)

#Q13
#Matthews correlation coefficient for Logistic Model
#getting the confusion matrix values 
r<-as.data.frame(b)
p<-as.data.frame(lm_trc)
p$lm_trc=ifelse(p$lm_trc == "YES",1,0)

w <- cbind(r,p)

w["newclass"] <- ifelse(r$b==0 & p$lm_trc==0, "TN", 
                        ifelse(r$b==0 & p$lm_trc==1, "FP",
                               ifelse(r$b==1 & p$lm_trc==0, "FN", "TP")))

#Create raw confusion matrix using "table" function
(conf.val <- table(w["newclass"]))
FN <- conf.val[1] 
FP <- conf.val[2] 
TN <- conf.val[3] 
TP <- conf.val[4] 


t1<- (TP*TN)-(FP*FN)
x1<- sqrt(TP+FP)
x2 <- sqrt(FN+TN)
x3<- sqrt(FP+TN)
x4 <- sqrt(TP+FN)
t2<- x1*x2*x3*x4

mcc_lm <- (t1/t2)

rm(FN,FP,TN,TP,x1,x2,x3,x4,t1,t2)


#Q10 
# train a classification tree model
myModel1 <- train(y ~ .,               # model specification
                  data = d_train,        # train set used to build model
                  method = "C5.0Tree",    # type of model you want to build
                  trControl = ctrl,    # how you want to learn
                  tuneLength = 1:15,   # vary tree size
                  metric = "ROC"       # performance measure
)
myModel1

#Q11 Classification Model
#predicted probabilities and predicted classes 
# Capture the train and test estimated probabilities and predicted classes
# model 1 
tree1_trp <- predict(myModel1, newdata=d_train, type='prob')[,1]
tree1_trc <- predict(myModel1, newdata=d_train)

#Q12 classification Model
# classification model confusionMatrix
# model 1 - classification tree
(cm_t <- confusionMatrix(data=tree1_trc, d_train$y))


#Q12 classification Model
#AUC
library(pROC)
a=ifelse(d_train$y == "YES",1,0)
auc(a,tree1_trp)

#Q13 Classification Model
#Matthews correlation coefficient 
#getting the confusion matrix values 
r<-as.data.frame(a)
p<-as.data.frame(tree1_trc)
p$tree1_trc=ifelse(p$tree1_trc == "YES",1,0)

w <- cbind(r,p)

w["newclass"] <- ifelse(r$a==0 & p$tree1_trc==0, "TN", 
                        ifelse(r$a==0 & p$tree1_trc==1, "FP",
                               ifelse(r$a==1 & p$tree1_trc==0, "FN", "TP")))

#Create raw confusion matrix using "table" function
(conf.val <- table(w["newclass"]))
FN <- conf.val[1] 
FP <- conf.val[2] 
TN <- conf.val[3] 
TP <- conf.val[4] 


t1<- (TP*TN)-(FP*FN)
x1<- sqrt(TP+FP)
x2 <- sqrt(FN+TN)
x3<- sqrt(FP+TN)
x4 <- sqrt(TP+FN)
t2<- x1*x2*x3*x4

mcc_tree <- 0
mcc_tree <- t1/t2

rm(FN,FP,TN,TP,x1,x2,x3,x4,t1,t2)

#Q12 Plotting the ROC Curve for Linear Model & Classification Model
#Plotting of variable ROC Curve
plot.roc(b,lm_trp, col='green')
par(new=TRUE)
plot.roc(a, tree1_trp, col='blue')
legend("topleft", legend = c("GLM","Tree") , pch = 15, bty = 'n', col = c("green","blue"))



#Q14
# Trying out Neural Net Model to check if we get better Balanced Accuracy'
myModel3 <- train(y ~ .,               # model specification
                  data = d_train,        # train set used to build model
                  method = "nnet",     # type of model you want to build
                  trControl = ctrl,    # how you want to learn
                  tuneLength = 1:5,   # how many tuning parameter combos to try
                  maxit = 100,         # max # of iterations
                  metric = "ROC"       # performance measure
)
myModel3

# Capture the train and test estimated probabilities and predicted classes
# model 3 Neural Net
nn1_trp <- predict(myModel3, newdata=d_train, type='prob')[,1]
nn1_trc <- predict(myModel3, newdata=d_train)

# model 3 - Neural Net
(cm_nn <- confusionMatrix(data=nn1_trc, d_train$y))

#getting the confusion matrix values 
r<-as.data.frame(a)
p<-as.data.frame(nn1_trc)
p$nn1_trc=ifelse(p$nn1_trc == "YES",1,0)

w <- cbind(r,p)

w["newclass"] <- ifelse(r$a==0 & p$nn1_trc==0, "TN", 
                        ifelse(r$a==0 & p$nn1_trc==1, "FP",
                               ifelse(r$a==1 & p$nn1_trc==0, "FN", "TP")))

#Create raw confusion matrix using "table" function
(conf.val <- table(w["newclass"]))
FN <- conf.val[1] 
FP <- conf.val[2] 
TN <- conf.val[3] 
TP <- conf.val[4] 


t1<- (TP*TN)-(FP*FN)
x1<- sqrt(TP+FP)
x2 <- sqrt(FN+TN)
x3<- sqrt(FP+TN)
x4 <- sqrt(TP+FN)
t2<- x1*x2*x3*x4

mcc_nn1 <- 0
mcc_nn1 <- t1/t2

rm(FN,FP,TN,TP,x1,x2,x3,x4,t1,t2)


#AUC
library(pROC)
c=ifelse(d_train$y == "YES",1,0)
auc(c,nn1_trp)

#feed-forward neural net on the down-sampled train
# down-sampled training set
dnTrain <- downSample(x=d_train[,2:ncol(d)], y=d_train$y)
names(dnTrain)[15] <- "y"

myGrid <-  expand.grid(size = c(10,15,20)     # number of units in the hidden layer.
                       , decay = c(.09,0.12))  #parameter for weight decay. Default 0.
myModel4 <- train(y ~ .,              # model specification
                  data = dnTrain,       # train set used to build model
                  method = "nnet",    # type of model you want to build
                  trControl = ctrl,   # how you want to learn
                  tuneGrid = myGrid,  # tuning parameter combos to try
                  maxit = 100,        # max # of iterations
                  metric = "ROC"      # performance measure
)
myModel4

#feed-forward neural net on the down-sampled train
# model 4 feed-forward neural net on the down-sampled train Predictions
nn2_trp <- predict(myModel4, newdata=dnTrain, type='prob')[,1]
nn2_trc <- predict(myModel4, newdata=dnTrain)

#feed-forward neural net on the down-sampled train
#confusion Matrix
(cm4 <- confusionMatrix(data=nn2_trc, dnTrain$y))

#feed-forward neural net on the down-sampled train
#getting the confusion matrix values 
r<-as.data.frame(a)
p<-as.data.frame(nn2_trc)
p$nn2_trc=ifelse(p$nn2_trc == "YES",1,0)

w <- cbind(r,p)

w["newclass"] <- ifelse(r$a==0 & p$nn2_trc==0, "TN", 
                        ifelse(r$a==0 & p$nn2_trc==1, "FP",
                               ifelse(r$a==1 & p$nn2_trc==0, "FN", "TP")))

#Create raw confusion matrix using "table" function
(conf.val <- table(w["newclass"]))
FN <- conf.val[1] 
FP <- conf.val[2] 
TN <- conf.val[3] 
TP <- conf.val[4] 


t1<- (TP*TN)-(FP*FN)
x1<- sqrt(TP+FP)
x2 <- sqrt(FN+TN)
x3<- sqrt(FP+TN)
x4 <- sqrt(TP+FN)
t2<- x1*x2*x3*x4

mcc_nn2 <- 0
mcc_nn2 <- t1/t2

rm(FN,FP,TN,TP,x1,x2,x3,x4,t1,t2,r,p,w)


#Q15
# Observing the values above we found that out of 
#Linear Model
# Classification Model
# Neural Net
#feed-forward neural net on the down-sampled train

# Classification Model had the best/highest Matthews correlation coefficient
#Hence using this model to get prediction values 

write.csv(score_set, file = "score_set.csv")



################################################################################
## Handling missing values
################################################################################
# % of rowing having missing values
dim(score_set[!complete.cases(score_set),])[[1]]/nrow(score_set)*100

# missing value report
source("DataQualityReport.R")
DataQualityReport(score_set)

################################################################################
## Creating Dummy Variables
################################################################################
# Here we want to take a 'factor variable' or 'categorical variable' and create
# columns for each factor level.
library(caret)
dummies <- dummyVars(y ~ ., data = score_set)            # create dummyes for Xs
ex <- data.frame(predict(dummies, newdata = score_set))  # actually creates the dummies
names(ex) <- gsub("\\.", "", names(ex))          # removes dots from col names
score_set <- cbind(d=score_set$y, ex)                              # combine your target variable with Xs
names(score_set)[1] <- "y"                               # make target variable called 'y'
rm(dummies, ex)                                  # delete temporary things we no longer need


################################################################################
# Remove Zero- and Near Zero-Variance Predictors
################################################################################
# Can you imagine having an X (independent) variable that contained exactly the
# same value for all its records? Do you think it would help you predict y?
# Obviously it would not help. Thus, we would want to delete features like this.
# We would also want to delete features that almost always have the same value.
# This is the idea behind zero-variance (i.e. same exact value) and near-zero-variance
# (i.e. almost the same value). We easily identify those features in this code
# chunk and remove them
library(caret)
dim(score_set) # dimension of dataset
nzv <- nearZeroVar(score_set[,2:ncol(score_set)], uniqueCut=10) # identify columns that are "near zero"
score_set_filtered <- score_set[,2:ncol(score_set)][, -nzv]            # remove those columns from your dataset
dim(score_set_filtered)                                # dimension of your filtered dataset

rm(score_set_filtered)


################################################################################
# Identify Correlated Predictors and remove them
################################################################################
# If you build a model that has highly correlated independent variables it can
# lead to unstable models because it will tend to weight those more even though
# they might not be that important

# calculate correlation matrix using Pearson's correlation formula
descrCor <-  cor(score_set[,2:ncol(score_set)])                           # correlation matrix
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .90) # number of Xs having a corr > some value
summary(descrCor[upper.tri(descrCor)])                    # summarize the correlations

# which columns in your correlation matrix have a correlation greater than some
# specified absolute cutoff?
highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.90)
filteredDescr <- score_set[,2:ncol(score_set)][,-highlyCorDescr] # remove those specific columns from your dataset
descrCor2 <- cor(filteredDescr)                  # calculate a new correlation matrix

# summarize those correlations to see if all features are now within our range
summary(descrCor2[upper.tri(descrCor2)])

# update our d dataset by removing those filtered variables that were highly correlated
score_set <- cbind(score_set$y, filteredDescr)
names(score_set)[1] <- "y"

rm(filteredDescr, descrCor, descrCor2, highCorr, highlyCorDescr)  # clean up

################################################################################
# Identifying linear dependencies and remove them
################################################################################
# Find if any linear combinations exist and which column combos they are.
# Below I add a vector of 1s at the beginning of the dataset. This helps ensure
# the same features are identified and removed.

library(caret)
# first save response
y <- score_set$y

# create a column of 1s. This will help identify all the right linear combos
score_set <- cbind(rep(1, nrow(score_set)), score_set[2:ncol(score_set)])
names(score_set)[1] <- "ones"

# identify the columns that are linear combos
comboInfo <- findLinearCombos(score_set)
comboInfo

# remove columns identified that led to linear combos
score_set <- score_set[, -comboInfo$remove]

# remove the "ones" column in the first column
score_set <- score_set[, c(2:ncol(d))]

# Add the target variable back to our data.frame
score_set <- cbind(y, score_set)

rm(y, comboInfo)  # clean up


################################################################################
# Standardize (and/ normalize) your input features.
################################################################################
# Here we standardize the input features (Xs) using the preProcess() function 
# by performing a typical Z-score standardization (method = c("center","scale")))
# This will make all the numeric features centered at 0 and have a standard
# deviation of 1. 
#
# As shown in previous class examples, we could have tried a min-max normalization
# ("range") and even tried to make the features more bell-shaped by doing a
# "YeoJohnson" transformation. If you wanted to do those things, you would just
# modify the method like so: method = c("range","YeoJohnson")

# To make sure I do not standardize the dummy variables I'll create a set that 
# contains the 0/1 variables (dCats) and the numeric features (dNums) 
numcols <- apply(X=score_set, MARGIN=2, function(c) sum(c==0 | c==1)) != nrow(score_set)
catcols <- apply(X=score_set, MARGIN=2, function(c) sum(c==0 | c==1)) == nrow(score_set)
dNums <- score_set[,numcols]
dCats <- score_set[,catcols]

# Step 1) figures out the means, standard deviations, other parameters, etc. to 
# transform each variable
preProcValues <- preProcess(dNums[,2:ncol(dNums)], method = c("center","scale"))

# Step 2) the predict() function actually does the transformation using the 
# parameters identified in the previous step. Weird that it uses predict() to do 
# this, but it does!
dNums <- predict(preProcValues, dNums)

# combine the standardized numeric features with the dummy vars
score_set <- cbind(dNums, dCats)
score_set$person_id <- NULL

rm(preProcValues, numcols, catcols, dNums, dCats)  # clean up

# Capture the train and test estimated probabilities and predicted classes
# model Classification
tree2_trp <- predict(myModel1, newdata=score_set, type='prob')[,1]
tree2_trc <- predict(myModel1, newdata=score_set)

tree2_trp <- data.frame(tree2_trp)

names(tree2_trp)[1] = "prediction"
final_prediction <- as.data.frame(cbind(score_set$person_id,tree2_trp$prediction))
names(final_prediction)[1] = "person_id"
names(final_prediction)[2] = "prediction"

#Final Prediction Values
write.csv(final_prediction, file = "anand57_scores.csv")
