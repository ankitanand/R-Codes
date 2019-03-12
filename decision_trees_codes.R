###############################################################################
# Decision tree modeling examples
# Data Mining
# Matthew A. Lanham
###############################################################################
# Load in dataset
library(ISLR)
data(Hitters)
head(Hitters, n=2)
names(Hitters)

# build a regression tree with one split on Years
library(tree)
?tree
treefit <- tree(log(Salary) ~ Years, data=Hitters, split="deviance")
treefit <- prune.tree(treefit, best=2)
summary(treefit)
plot(treefit); text(treefit, pretty=0)

################################################################################
# Example of finding the first split in the tree from scratch
################################################################################
# make response variable first column, numeric colums next, then factors last
#d <- Hitters[,c(19,1:13,16:18,14,15,20)]
#d <- d[,c(1:17)] # only going to consider numeric features for example
d <- Hitters[,c(19,7,2)] # just has Salary, Years, and Hits
names(d)[1] <- "y"
str(d)

# create new dataset without missing data 
d<- na.omit(d)

# obtain min and max values for each numeric feature
library(dplyr)
library(psych)
(range <- describe(d)[,c("min","max")])

# specify how many cutoff thresholds you'd like to try per feature
nCuts = 20

# construct a data.frame to store error results for each cutoff by feature
firstCutE <- data.frame(matrix(NA, nrow=nCuts, ncol=(ncol(d)-1)))
names(firstCutE) <- names(d)[2:ncol(d)]
# construct a data.frame to store the possible cuts we tried by feature
cutsUsed <- data.frame(matrix(NA, nrow=nCuts, ncol=(ncol(d)-1)))
names(cutsUsed) <- names(d)[2:ncol(d)]

# loop through each feature for each cutoff, generate predictions,and save errors
for (i in 2:ncol(d)){
    for (j in 1:nCuts){
        # create possible splits for this feature
        (cuts <- seq(from=range[i,1], to=range[i,2], length.out = nCuts))
        cutsUsed[,i-1] <- cuts
        
        # create data set partitions
        tmpL <- d %>% filter(d[,i] <= cuts[j])
        tmpR <- d %>% filter(d[,i] > cuts[j])
        
        # compute terminal node averages
        predL <- mean(tmpL$y, na.rm=T)
        predR <- mean(tmpR$y, na.rm=T)
        
        # compute prediction
        yhat <- ifelse(d[,i] <= cuts[j], predL, predR)
        
        # compute error and save it (here used log(y) and log(yhat))
        firstCutE[j,i-1] <- round(sum((log(d$y) - log(yhat))^2),1)
    }
}

# feature split on first
which(firstCutE == min(firstCutE), arr.ind = TRUE)
names(firstCutE)[which(firstCutE == min(firstCutE), arr.ind = TRUE)[2]] 

# plot splits by error rate by feature
par(mfrow=c(1,2))
plot(x=cutsUsed[,1], y=firstCutE[,1], pch=19, main="Years", xlab="split", ylab="Error")
lines(x=cutsUsed[,1], y=firstCutE[,1], pch=19)
plot(x=cutsUsed[,2], y=firstCutE[,2], pch=19, main="Hits", xlab="split", ylab="Error")
lines(x=cutsUsed[,2], y=firstCutE[,2], pch=19)


################################################################################
# Build a large tree using all features, T0
################################################################################
# Load in dataset
library(ISLR)
data(Hitters)
head(Hitters)

# create a training and testing set
set.seed(2)
train = sample(1:nrow(Hitters), 132)
Hitters.train = Hitters[train, ]
Hitters.test = Hitters[-train, ]

# a utility function for use with the control argument in how the tree builds
tree.control(mincut = 0     # min # of obs to include in either child node. 
             , minsize = 1  # smallest allowed node size: a weighted quantity.
             , mindev = 0.01
             , nobs=dim(Hitters.train)[1]
             )
# fit the tree
treefit = tree(log(Salary) ~ AtBat + Hits + HmRun + Runs + RBI + Walks + Years 
               + PutOuts 
              #, control = tree.control
              , data = Hitters.train)
summary(treefit)
par(mfrow=c(1,1))
plot(treefit); text(treefit, pretty=0) # plot the tree

# perform cross-validation to find optimal number of terminal nodes
cv.hitters = cv.tree(treefit)
?cv.tree
plot(cv.hitters$size
     , cv.hitters$dev
     , type = 'b')

# prune tree where the number of terminal nodes is 3
prunedfit = prune.tree(treefit, best=3)
?prune.tree
summary(prunedfit)
plot(prunedfit); text(prunedfit, pretty=0)

###############################################################################
# Example: Predict Boston Housing prices using a regression tree 
###############################################################################
# load the dataset
library(MASS)
data(Boston)
head(Boston)

# set a seed and identify training records
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)

# fit the tree
tree.boston = tree(medv ~.
                   , data = Boston
                   , subset = train)
summary(tree.boston)
# plot the tree
plot(tree.boston); text(tree.boston,pretty=0)

# perform cross-validation
cv.boston = cv.tree(tree.boston)
plot(cv.boston$size
     , cv.boston$dev
     , type = 'b')

# prune the tree
prune.boston = prune.tree(tree.boston, best=7)
plot(prune.boston); text(prune.boston,pretty=0)

# use the prune tree to predict future values on test set
yhat = predict(prune.boston, newdata=Boston[-train,])

# evaluate performance of test set
boston.test = Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)

################################################################################
# Classification tree using caret
################################################################################
# set memory limits
options(java.parameters = "-Xmx64048m") # 64048 is 64 GB
set.seed(1234)

# Connecting to a MySQL on the server
# Need to change 'lanhamm' to your user name. Make sure path classPath= is correct
library(RJDBC)
drv <- JDBC(driverClass="com.mysql.jdbc.Driver"
            ,classPath="/home/lanhamm/mysql-connector-java-5.1.47.jar")
conn <- dbConnect(drv, url="jdbc:mysql://datamine.rcac.purdue.edu:3306/politics"
                  , user="gen_user", password="gen_user")

# run a SQL query and pull in data from the database
h <- dbGetQuery(conn, "select * from household_data")
i <- dbGetQuery(conn, "select * from individual_data")
r <- dbGetQuery(conn, "select * from registration_status")

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

# 'd' dataset and 'score_set'
score_set <- d[is.na(d$y),]
d <- d[!is.na(d$y),]

# Get the target variable how we want it for modeling with caret
# make names for target if not already made
levels(d$y) <- make.names(levels(factor(d$y)))
levels(d$y)

# levels of a factor are re-ordered so that the level specified is first and 
# "X1" is what we are predicting. The X before the 1 has nothing to do with the
# X variables. It's just something weird with R. 'X1' is the same as 1 for the Y 
# variable and 'X0' is the same as 0 for the Y variable.
d$y <- relevel(d$y,"X1")

# remove irrelevant features
d$person_id <- NULL
d$hh_id <- NULL

################################################################################
## Creating Dummy Variables
################################################################################
# Here we want to create a dummy 0/1 variable for every level of a categorical 
# variable
library(caret)
dummies <- dummyVars(y ~ ., data = d)            # create dummies for Xs
ex <- data.frame(predict(dummies, newdata = d))  # actually creates the dummies
names(ex) <- gsub("\\.", "", names(ex))          # removes dots from col names
d <- cbind(d$y, ex)                              # combine target var with Xs
names(d)[1] <- "y"                               # name target var 'y'
rm(dummies, ex)                                  # clean environment
################################################################################
# Identify Correlated Predictors and remove them
################################################################################
# If you build a model that has highly correlated independent variables it can
# lead to unstable models because it will tend to weight those more even though
# they might not be that important

# calculate correlation matrix using Pearson's correlation formula
descrCor <-  cor(d[,2:ncol(d)])                           # correlation matrix
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .85) # num Xs with cor > t
summary(descrCor[upper.tri(descrCor)])                    # summarize the cors

# which columns in your correlation matrix have a correlation greater than some
# specified absolute cutoff. Find them and remove them
highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.85)
filteredDescr <- d[,2:ncol(d)][,-highlyCorDescr] # remove those specific columns
descrCor2 <- cor(filteredDescr)                  # calculate a new cor matrix
# summarize those correlations to see if all features are now within our range
summary(descrCor2[upper.tri(descrCor2)])

# update dataset by removing those filtered vars that were highly correlated
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
# by performing a min-max normalization (aka "range" in caret).

# Step 1) figures out the means, standard deviations, other parameters, etc. to 
# transform each variable
preProcValues <- preProcess(d[,2:ncol(d)], method = c("range"))
# Step 2) the predict() function actually does the transformation using the 
# parameters identified in the previous step. Weird that it uses predict() to do 
# this, but it does!
d <- predict(preProcValues, d)
################################################################################
# Data partitioning
################################################################################
set.seed(1234) # set a seed so you can replicate your results
library(caret)

# identify records that will be used in the training set. Here we are doing a
# 70/30 train-test split. You might modify this.
inTrain <- createDataPartition(y = d$y,   # outcome variable
                               p = .70,   # % of training data you want
                               list = F)
# create your partitions
train <- d[inTrain,]  # training data set
test <- d[-inTrain,]  # test data set

################################################################################
# Specify cross-validation design
################################################################################
ctrl <- trainControl(method="cv",     # cross-validation set approach to use
                     number=3,        # k number of times to do k-fold
                     classProbs = T,  # if you want probabilities
                     summaryFunction = twoClassSummary, # for classification
                     #summaryFunction = defaultSummary,  # for regression
                     allowParallel=T)

################################################################################
# Train different tree models
# there are so many different types of models you can try, go here to see them all
# http://topepo.github.io/caret/available-models.html
################################################################################
# train a C5.0 classificationt tree
myModel1 <- train(y ~ .,               # model specification
                  data = train,        # train set used to build model
                  method = "C5.0Tree",    # type of model you want to build
                  trControl = ctrl,    # how you want to learn
                  tuneLength = 1:15,   # vary tree size
                  metric = "ROC"       # performance measure
)
myModel1

# train another type of classificationt tree: Boosted classification tree
myModel2 <- train(y ~ .,               # model specification
                  data = train,        # train set used to build model
                  method = "ada",      # type of model you want to build
                  trControl = ctrl,    # how you want to learn
                  #tuneLength = 1:15,   # vary tree size
                  metric = "ROC"       # performance measure
)
myModel2

# Capture the train and test estimated probabilities and predicted classes
# model 1 
tree1_trp <- predict(myModel1, newdata=train, type='prob')[,1]
tree1_trc <- predict(myModel1, newdata=train)
tree1_tep <- predict(myModel1, newdata=test, type='prob')[,1]
tree1_tec <- predict(myModel1, newdata=test)
# model 2 
tree2_trp <- predict(myModel2, newdata=train, type='prob')[,1]
tree2_trc <- predict(myModel2, newdata=train)
tree2_tep <- predict(myModel2, newdata=test, type='prob')[,1]
tree2_tec <- predict(myModel2, newdata=test)

################################################################################
# Now use those predictions to assess performance on the train set and testing
# set. Be on the lookout for overfitting
# model 1 - tree model
(cm <- confusionMatrix(data=tree1_trc, train$y))
(testCM <- confusionMatrix(data=tree1_tec, test$y))
# model 2 - another tree model
(cm2 <- confusionMatrix(data=tree2_trc, train$y))
(testCM2 <- confusionMatrix(data=tree2_tec, test$y))






