################################################################################
# Predicting Housing Prices
# 
# MGMT 474: Homework 3
################################################################################

################################################################################
# Data loading
################################################################################
# set memory limits
options(java.parameters = "-Xmx64048m") # 64048 is 64 GB

# connec to db server
# change the /home/lanhamm/postgresql-9.3-1104.jdbc41.jar to your username
library(RJDBC)
drv <- JDBC(driverClass="org.postgresql.Driver"
            ,classPath="/home/lanhamm/postgresql-9.3-1104.jdbc41.jar")
conn <- dbConnect(drv, url="jdbc:postgresql://datamine.rcac.purdue.edu:5432/lanhamm"
                  , user="khou_user", password="4fWaikwEQX6RnD")

# pull data for analysis
d <- dbGetQuery(conn, "
                SELECT *
                FROM khou.train
                ")
str(d)

# The data was saved in the database with missing values showing as 'NA' rather
# than being blank. We need to correct this as R will view 'NA' as a factor level
# when it is not.
d2 <- d
for(i in 1:ncol(d)) {
    d2[,names(d)[i]] <- ifelse(d2[,names(d)[i]] =="NA", NA, d2[,names(d)[i]])
}
d <- d2
rm(d2)

# coerce feature types to their correct types for analysis
d$MSSubClass <- as.factor(d$MSSubClass)   
d$MSZoning <- as.factor(d$MSZoning)    
d$LotFrontage <- as.numeric(d$LotFrontage)   
d$Street <- as.factor(d$Street)       
d$Alley <- as.factor(d$Alley)        
d$LotShape <- as.factor(d$LotShape)
d$LandContour <- as.factor(d$LandContour)
d$Utilities <- as.factor(d$Utilities)
d$LotConfig <- as.factor(d$LotConfig)
d$LandSlope <- as.factor(d$LandSlope)
d$Neighborhood <- as.factor(d$Neighborhood)
d$Condition1 <- as.factor(d$Condition1)
d$Condition2 <- as.factor(d$Condition2)
d$BldgType <- as.factor(d$BldgType)
d$HouseStyle <- as.factor(d$HouseStyle) 
d$RoofStyle <- as.factor(d$RoofStyle)  
d$RoofMatl <- as.factor(d$RoofMatl)
d$Exterior1st <- as.factor(d$Exterior1st)
d$Exterior2nd <- as.factor(d$Exterior2nd)
d$MasVnrType <- as.factor(d$MasVnrType)
d$MasVnrArea <- as.numeric(d$MasVnrArea)
d$ExterQual <- as.factor(d$ExterQual)   
d$ExterCond <- as.factor(d$ExterCond)
d$Foundation <- as.factor(d$Foundation)
d$BsmtQual <- as.factor(d$BsmtQual)      
d$BsmtCond <- as.factor(d$BsmtCond)
d$BsmtExposure <- as.factor(d$BsmtExposure)
d$BsmtFinType1 <- as.factor(d$BsmtFinType1)
d$BsmtFinType2 <- as.factor(d$BsmtFinType2)  
d$Heating <- as.factor(d$Heating)
d$HeatingQC <- as.factor(d$HeatingQC)
d$CentralAir <- as.factor(d$CentralAir)
d$Electrical <- as.factor(d$Electrical)
d$KitchenQual <- as.factor(d$KitchenQual)
d$Functional <- as.factor(d$Functional) 
d$FireplaceQu <- as.factor(d$FireplaceQu)   
d$GarageType <- as.factor(d$GarageType) 
d$GarageYrBlt <- as.numeric(d$GarageYrBlt)
d$GarageFinish <- as.factor(d$GarageFinish)  
d$GarageQual <- as.factor(d$GarageQual)  
d$GarageCond <- as.factor(d$GarageCond)  
d$PavedDrive <- as.factor(d$PavedDrive)    
d$PoolQC <- as.factor(d$PoolQC)       
d$Fence <- as.factor(d$Fence)        
d$MiscFeature <- as.factor(d$MiscFeature) 
d$SaleType <- as.factor(d$SaleType)
d$SaleCondition <- as.factor(d$SaleCondition)

str(d)
################################################################################
## Handling missing values
################################################################################
# % of rowing having missing values
dim(d[!complete.cases(d),])[[1]]/nrow(d)*100

# missing value report
source("DataQualityReport.R")
DataQualityReport(d)

# We observe there are some features that have many missing values. Often in such
# cases you will remove these features from consideration for your analysis. A
# rule of thumb is that if more than 50% of the records are missing, don't use
# that features. If less than 50% is missing and your intuition is the feature
# is predictive, then you can try to perform model-based imputation to fill in
# the missing values
#      Attributes    Type NumberMissing PercentComplete   Min      Avg Median    Max NumberLevels
#7          Alley  factor          1369            6.23     -        -      -      -            2
#73        PoolQC  factor          1453            0.48     -        -      -      -            3
#74         Fence  factor          1179           19.25     -        -      -      -            4
#75   MiscFeature  factor          1406            3.70     -        -      -      -            4
d$Alley <- NULL
d$PoolQC <- NULL
d$Fence <- NULL
d$MiscFeature <- NULL

# As shown below there are 15 features that have missing values that we can 
# consider to impute. Sometimes analysts will fill in missing values with the mean.
# This is often a bad idea as the real value is typically not the mean. A better
# approach is to use "model-based imputation". The means a predictive model is 
# built for each feature you want to impute using other Xs that do not have missing
# values. In this case, we'll have to build 15 models. This might take a bit of time,
# so keep this in mind when performing imputation on your projects' data.
# There is more info on model-based imputation in the notes posted on Blackboard.
#
#      Attributes    Type NumberMissing PercentComplete   Min      Avg Median    Max NumberLevels
#4    LotFrontage numeric           259           82.26    21    70.05     69    313            -
#26    MasVnrType  factor             8           99.45     -        -      -      -            4
#27    MasVnrArea numeric             8           99.45     0   103.69      0   1600            -
#31      BsmtQual  factor            37           97.47     -        -      -      -            4
#32      BsmtCond  factor            37           97.47     -        -      -      -            4
#33  BsmtExposure  factor            38           97.40     -        -      -      -            4
#34  BsmtFinType1  factor            37           97.47     -        -      -      -            6
#36  BsmtFinType2  factor            38           97.40     -        -      -      -            6
#43    Electrical  factor             1           99.93     -        -      -      -            5
#58   FireplaceQu  factor           690           52.74     -        -      -      -            5
#59    GarageType  factor            81           94.45     -        -      -      -            6
#60   GarageYrBlt numeric            81           94.45  1900  1978.51   1980   2010            -
#61  GarageFinish  factor            81           94.45     -        -      -      -            3
#64    GarageQual  factor            81           94.45     -        -      -      -            5
#65    GarageCond  factor            81           94.45     -        -      -      -            5

# Imputing missing values using predictive modeling (CART (Classificaton & Regression 
# Tree) is used in this example). This will run faster than using method="rf" for 
# Random Forest-based imputations
library(mice)
# creating a list of imputed values using a CART model
# you can see other models available to impute by looking at ?mice
# This should run for about 5-6 minutes 
imputedValues <- mice(data=d, m=3, method="cart", seed=2016)

# Compare the distribution of the features you are imputing before and after you
# imputed them. Ideally, if the imputation is "good", the distributions
# should be very similar. Sometimes people will fill in missing values with the
# mean. This can cause a spike in this distribution, suggesting that it is likely
# not correct. And if you use those features in your predictive modeling task
# will tend to lead to poor models.
# Here are some more examples if you are interested: 
#    (a) https://datascienceplus.com/imputing-missing-data-with-r-mice-package/
#    (b) https://datascienceplus.com/handling-missing-data-with-mice-package-a-simple-approach/

# Example: examining imputations for the 3 runs on numeric features
densityplot(imputedValues, layout=c(3, 1))

# Example: examining imputations for the MasVnrType categorical variable for 3 runs
# It appears the distributions are the same
test <- rbind(data.frame(value=complete(imputedValues,1)[,"MasVnrType"], set="1"),
              data.frame(value=complete(imputedValues,2)[,"MasVnrType"], set="2"),
              data.frame(value=complete(imputedValues,3)[,"MasVnrType"], set="3"))
barplot(table(test), xlab="Imputation set", ylab="count", main="MasVnrType", beside=T)

# Since the variables appear to be very close in distribution to the orginial
# data values that had missing values, this should give us some comfort that we
# can use the imputed values in our analysis. Here I will overwrite the original
d <- complete(imputedValues,1)

# While the imputation did not take too long to do. It might take much longer if
# you have more observations or more features that need imputed. So, you might
# save your imputed dataset after its finished, in case you need to come back later.
# save d data.frame
save(imputedValues, file="imputedValues.Rda")
save(d, file="d.Rda")
# load it back if needed
#load("d.Rda")
#load("imputedValues.Rda")

# if we re-run the data quality report, we should see all values are 100% complete
DataQualityReport(d)

# we can now delete some objects in our R environment that are no longer needed
rm(imputedValues, i, test)
################################################################################
## Data clean up
################################################################################
# the Id field is just the row id and would not make sense to include this feature
# as a predictor predictor so we can delete it
d$Id <- NULL

# the last column in our dataset is our target (SalesPrice). To more easily
# incorporate codes from other projects, labs, assignments, etc. that we have
# learned, we can change the name of that variable to just 'y'
names(d)[76] <- "y"

# also lets make 'y' the first column in our dataset to be consistent in our 
# studies
d <- d[,c(76,1:75)]

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

# we can investigate some of the features that the nearZeroVar() function has
# identified for removal
setdiff(names(d), names(d_filtered))
# Example 1: MSSubClass30 for example has 1391/1460 (95.3%) of its values as 0s
table(d$MSSubClass30)
# Example 2: Exterior1stAsphShn has 1459/1460 (99.9%) of its values as 0s
table(d$Exterior1stAsphShn)
# Thus, it is likely these features will not help us predict housing price and we
# we should reduce the dimensionality of our data to find those set of features
# that have the most potential.

# It appears that the nearZeroVar is doing what we expect it to which is identify
# variables that do not have much variation. You can always adjust the degree of
# 'low variance' within the nearZeroVar() function above by changing the uniqueCut=
# and freqCut= arguments within the function. See ?nearZeroVar to learn more.

# Here we will remove those features that nearZeroVar() identified for removal
# from our d dataset.
d <- cbind(d$y, d_filtered)   # combine y with the Xs
names(d)[1] <- "y"            # fix the y variable name

rm(d_filtered, nzv)           # clean up      
################################################################################
# Identify Correlated Predictors and remove them
################################################################################
# If you build a model that has highly correlated independent variables it can
# lead to unstable models because it will tend to weight those more even though
# they might not be that important

# calculate correlation matrix using Pearson's correlation formula
descrCor <-  cor(d[,2:ncol(d)])                           # correlation matrix
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .85) # number of Xs having a corr > some value
summary(descrCor[upper.tri(descrCor)])                    # summarize the correlations

# which columns in your correlation matrix have a correlation greater than some
# specified absolute cutoff?
highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.85)
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

rm(preProcValues, numcols, catcols, dNums, dCats)  # clean up
################################################################################
# Get the target variable how we want it for modeling with caret
################################################################################
# If you recall in binary classification problems we 'made names' and 'releveled'
# our y variable. We dont need to do that for regression-type problems

################################################################################
# Data partitioning
################################################################################
set.seed(1234) # set a seed so you can replicate your results
library(caret)

# identify records that will be used in the training set. Here we are doing a
# 85% train/ 15% test split. You might modify this.
inTrain <- createDataPartition(y = d$y,   # outcome variable
                               p = .85,   # % of training data you want
                               list = F)
# create your partitions
train <- d[inTrain,]  # training data set
test <- d[-inTrain,]  # test data set

################################################################################
# Automatic feature selection using using forward and backward selection
################################################################################
# As discussed in class, you'll likely want to perform some feature selection 
# approaches to reduce the data dimensionality. There are a couple reasons for this.
# First, using alot of features will often increase training time, and for some
# models (e.g. neural nets) that can take a long time! Secondly, the more features
# you have, the more likley you will overfit to the train data, and thus have poor
# performance on your holdout/test set.
#
# Here I show you how to do forward and backard selection AUTOMATICALLY using the
# regsubsets() function from the leaps package. This is especially useful if you
# have many features like in this example.

# Here I fit a linear regression on all the features. Look how many are insignificant!!
# You'd be spending alot of time doing backward selection by hand.
mf <- lm(y ~ ., data=train)
summary(mf)

# automatic backward selection
library(leaps)
mb <- regsubsets(y ~ ., data=train
                 , nbest=1
                 , intercept=T
                 , method='backward'
                 , really.big=T
)

# You can pick the model you want based on the performance measure you specify. 
# Here I used 'adjr2', but you could select based on 'cp', 'bic', 'rss'
vars2keep <- data.frame(summary(mb)$which[which.max(summary(mb)$adjr2),])
names(vars2keep) <- c("keep")  
head(vars2keep)
library(data.table)
vars2keep <- setDT(vars2keep, keep.rownames=T)[]
vars2keep <- c(vars2keep[which(vars2keep$keep==T & vars2keep$rn!="(Intercept)"),"rn"])[[1]]

# here are the final features found to be statistically significant
vars2keep
#[1] "OverallQual"      "YearBuilt"        "GrLivArea"        "BldgType1Fam"     "HouseStyle1Story" "BsmtQualEx"       "BsmtExposureGd"  
#[8] "KitchenQualEx"  

# Here we will fit a model on those features we found using backward selection
modelFormula <- paste("y ~ OverallQual + YearBuilt + GrLivArea + BldgType1Fam + 
                      HouseStyle1Story + BsmtQualEx + BsmtExposureGd + KitchenQualEx") 
mb <- lm(modelFormula, data=train)
summary(mb)

# automatic forward selection
library(leaps)
mf <- regsubsets(y ~ ., data=train
                 , nbest=1
                 , intercept=T
                 , method='forward'
                 , really.big=T
)

# You can pick the model you want based on the performance measure you specify. 
# Here I used 'adjr2', but you could select based on 'cp', 'bic', 'rss'
vars2keep <- data.frame(summary(mf)$which[which.max(summary(mf)$adjr2),])
names(vars2keep) <- c("keep")  
head(vars2keep)
library(data.table)
vars2keep <- setDT(vars2keep, keep.rownames=T)[]
vars2keep <- c(vars2keep[which(vars2keep$keep==T & vars2keep$rn!="(Intercept)"),"rn"])[[1]]

# here are the final features found to be statistically significant
vars2keep
#[1] "OverallQual"    "YearBuilt"      "BsmtFinSF1"     "GrLivArea"      "BldgType1Fam"   "BsmtQualEx"     "BsmtExposureGd"
#[8] "KitchenQualEx"  

modelFormula <- paste("y ~ OverallQual + YearBuilt + BsmtFinSF1 + GrLivArea + 
                      BldgType1Fam + BsmtQualEx + BsmtExposureGd + KitchenQualEx")
mf <- lm(modelFormula, data=train)
summary(mf)

################################################################################
# Simultaneous model building and feature selection using LASSO
################################################################################
# Here we train a LASSO model using 3-fold cross-validation. We also tune it
# using 15 different s values from s=0.05 to s=1.
library(caret)
ctrl <- trainControl(method="cv",     # cross-validation set approach to use
                     number=3,        # k number of times to do k-fold
                     classProbs = F,  
                     summaryFunction = defaultSummary,
                     allowParallel=T)
# train a LASSO
lassofit <- train(y ~ .,
                  data = train,
                  method = "lars",
                  trControl = ctrl,
                  #preProcess=c("center","scale"), # not needed; already transformed
                  tuneLength = 15,                # this specifies various s values
                  metric = "RMSE")

# the last line of the model shows you the s that is 'optimal' to use and the
# corresponding performance for that s tuning parameter based on the metric
# you specified that you wanted to maximize/minimize (in this case RMSE)
lassofit

# there is alot going on in the path plot because there are many features
plot(lassofit$finalModel)

# optimal s
lassofit$bestTune[[1]]

# the fraction vs the RMSE. This gives some support that the s obtained from
# grabbing the lowest RMSE is an okay choice, but probably not the best.
plot(x=lassofit$results$fraction, y=lassofit$results$RMSE
     , col="blue", pch=19
     , main="RMSE vs s from caret runs", xlab="S", ylab="RMSE")

# obtain the variables that have non-zero parameter coefficients
y <- train$y
library(lars)
lasso <-lars(x=as.matrix(train[,2:ncol(d)]), y=y, type='lasso', trace=F, normalize=T, intercept=T)
larsBetas <- data.frame(predict.lars(object=lasso
                                     ,s=lassofit$bestTune[[1]]   # best s from caret
                                     #,s=0.2535714                # best s using plot
                                     ,mode='fraction'
                                     ,type='coefficients')$coefficients) 
names(larsBetas)[1] <- "B"
library(data.table)
setDT(larsBetas, keep.rownames = TRUE)[]
(larsBetas <- larsBetas[which(larsBetas$B > 0), ])
larsBetas$rn

rm(inTrain, modelFormula, vars2keep, y, DataQualityReport)  # clean up
################################################################################
# Simultaneous model building and feature selection using Random Forests
################################################################################
# train a random forest. This ran 15 minutes for me. 12:13
rf <- train(y ~ .,
            data = train,
            method = "rf",
            importance=T,    # we add this in or it varImp cannot be computed
            trControl = ctrl,
            tuneLength = 10,
            metric = "RMSE"
)
rf

# Note: you can always save a model or dataset so you don't have to retrain or 
# recreate it later. To save a model try using the saveRDS() function from the
# mgcv package. You can load it back in exactly as it was created originally
# using the readRDS() function.
library(mgcv) # used to save models
saveRDS(rf, "474_hw3_rf.rds")     # saves model
#readRDS(file="474_hw3_rf.rds")    # loads back into R when ready to use

# capture variable importance from the model. Here the values are scaled between
# between 0 and 100. You can think of this number as the % of the time that this
# feature was important at reducing the error as opposed to not using it. We go 
# into more detail here when we discuss random forests and ensembling in class.
varImp(rf) 
plot(varImp(rf))

################################################################################
# Calcuate train and test performance for the models you generated 
################################################################################
# The postResample() function from caret can be used to estimate the root mean 
# squared error (RMSE), simple R2, and the mean absolute error (MAE) for numeric 
# outcomes.

# generate predictions on train set
mb_pred1 <- predict(mb, train)
mf_pred1 <- predict(mf, train)
lasso_pred1 <- predict(lassofit, train)
rf_pred1 <- predict(rf, train)

# generate predictions on test set
mb_pred2 <- predict(mb, test)
mf_pred2 <- predict(mf, test)
lasso_pred2 <- predict(lassofit, test)
rf_pred2 <- predict(rf, test)

# calculate performance
tr_results <- rbind(
    postResample(pred = mb_pred1, obs = train$y),
    postResample(pred = mf_pred1, obs = train$y),
    postResample(pred = lasso_pred1, obs = train$y),
    postResample(pred = rf_pred1, obs = train$y)
)
te_results <- rbind(
    postResample(pred = mb_pred2, obs = test$y),
    postResample(pred = mf_pred2, obs = test$y),
    postResample(pred = lasso_pred2, obs = test$y),
    postResample(pred = rf_pred2, obs = test$y)
)
Models <- c("LR-Backward","LR-Forward","Lasso","Random Forest")
Set <- c(rep("Train",4))
(tr_results <- data.frame(Models, Set, tr_results))
Set <- c(rep("Test",4))
(te_results <- data.frame(Models, Set, te_results))

# often you'll need to put your results in sort of a 'normal' form so you can
# easy plot the results in various ways.
library(reshape)
results1 <- melt(tr_results, id=c("Models","Set"))
results2 <- melt(te_results, id=c("Models","Set"))
(results <- rbind(results1, results2))

################################################################################
# Create a plot of that compares your model results
################################################################################
library(ggplot2)
theme_set(theme_classic())
g <- ggplot(results[results$variable=="Rsquared",], aes(fill=Set, y=value, x=Models)) 
g <- g + geom_bar(position="dodge", colour="black", stat="identity")
g <- g + labs(title="Statistical Performance by Model", y="R-Squared")
g <- g + theme(plot.title = element_text(color="black", face="bold", size=14, hjust=0.5))
g <- g + theme(axis.text.x=element_text(colour="black"),
               axis.text.y=element_text(colour="black"))
g <- g + scale_y_continuous(labels = scales::percent)
g <- g + scale_fill_manual("Partition", values=c("Train"="orange","Test"="#0072B2"))
g

################################################################################
# Create a plot of you best model to present to decision-makers 
################################################################################
library(ggplot2)
library(ggExtra)
bestmodel <- data.frame(Y=test$y, Yhat=lasso_pred2)
custom_subtitle <- paste0("R2 = ", round(subset(results, Set=="Test" & Models=="Lasso" & variable=="Rsquared","value")[[1]],4))

# Scatterplot
theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(bestmodel, aes(Yhat, Y)) + geom_count(colour="black", size=1)
g <- g + geom_smooth(method="lm", se=T)
g <- g + theme(legend.position="none")
g <- g + labs(y="Actual Sales Price ($)", 
              x="Predicted Sales Price ($)", 
              title="Best Model Prediction",
              subtitle = custom_subtitle)
g <- g + theme(plot.title = element_text(color="black", face="bold", size=14, hjust=0.5))
g <- g + theme(plot.subtitle = element_text(color="black", face="bold", size=11, hjust=0.5))
g <- g + theme(axis.text.x=element_text(colour="black"),
               axis.text.y=element_text(colour="black"))
g <- g + scale_x_continuous(labels = scales::dollar)
g <- g + scale_y_continuous(labels = scales::dollar)
g <- g + coord_cartesian(xlim=c(20000, 420000), ylim=c(20000, 420000)) 
g <- g + guides(fill=FALSE)
g
ggMarginal(g, type="histogram", fill="orange", size=8)

