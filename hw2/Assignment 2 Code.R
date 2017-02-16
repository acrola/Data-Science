#ID 1: 203056585
#ID 2: 301380416

# Set your working directory to be your assignment folder. In that folder create three sub-folders named: 'Carvana', 'Diabetes' and 'Movies' in this exact format.
# Download the Carvana, Diabetes and movies files from moodle and make sure that that your folder hierarchy is as such: 
# "../Carvana/CARVANA.csv" , "../Diabetes/diabetes.csv" and "../Movies/movies.csv". 
# The readme file can be wherever you wish.

# Set your working directory to be the assignment folder (not the nested folders).
################################
#setwd("C:\\Users\\Avi\\AppData\\Roaming\\SPB_16.6\\Data-Science\\hw2\\Carvana")
setwd("C:\\Users\\dor\\Documents\\Data-Science\\hw2")


################################

# All packages installetions:
##############
#some packages needed this specific long row...
install.packages('SDMTools', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages('caTools', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages('caret', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages("corrplot")
install.packages("pROC")
install.packages("DMwR")
install.packages("e1071")
install.packages("randomForest")
install.packages("fpc")
library(SDMTools)
library(caTools)
library(caret)
library(lattice)
library(corrplot)
library(class)
library(pROC)
library(DMwR)
library(e1071)
library(randomForest)
library(fpc)

###########
# CARVANA #
###########


# 1. PREPARATION
##############

# 1.a. (0) Load the data in the appropriate format and name it 'data.c':
################################
data.c <- read.csv(file = 'Carvana/CARVANA.csv',header = TRUE,na.strings = c("", "NA"))
# We chose to use only complete cases. Leaves us about a third of the data
data.c <- data.c[complete.cases(data.c),2:ncol(data.c)]


# 1.b. (1) Set your random seed to some value so that our model comparisons will not be affected by randomness.
#######################
set.seed(42);


# 1.c. (2) As the Carvana file is large, and will cause our models to run for a long time - take a sample of 30,000 rows,
# and save it with the name data.c.
##############
data.c <- data.c[sample(1:nrow(data.c), 30000, replace=FALSE),] 


# 1.d. (3) Split the data into test (30%) and train (70%) sets with
# respect to the target variable. Save them as train.c and test.c.
################################
# First take 70% with IsBadBuy == 1
data.c.isBadBuy = data.c[data.c$IsBadBuy == 1,]
V <- sample.split(data.c.isBadBuy, SplitRatio = 0.7, group = NULL )
train.c <- data.c.isBadBuy[V,]
test.c <- data.c.isBadBuy[!V,]
# Second add 30% with IsBadBuy == 0
data.c.isNotBadBuy = data.c[data.c$IsBadBuy == 0,]
V <- sample.split(data.c.isNotBadBuy, SplitRatio = 0.7, group = NULL )
train.c <- rbind(train.c, data.c.isNotBadBuy[V,])
test.c <- rbind(test.c, data.c.isNotBadBuy[!V,])

# 2. FEATURE SELECTION AND CORRELATION
###################

# 2.a. (2) Convert all factorial features to numeric.
#######################
factor.features.names <- colnames(data.c[,sapply(train.c, is.factor)])
data.c[, factor.features.names] <- sapply(data.c[,factor.features.names], as.numeric)
train.c[, factor.features.names] <- sapply(train.c[,factor.features.names], as.numeric)
test.c[, factor.features.names] <- sapply(test.c[,factor.features.names], as.numeric)

# 2.b. (1) Convert the lable feature('IsBadBuy') to factor with 2 levels (True/False)
#######################
data.c$IsBadBuy <-  sapply(data.c$IsBadBuy, as.factor)
train.c$IsBadBuy <-  sapply(train.c$IsBadBuy, as.factor)
test.c$IsBadBuy <-  sapply(test.c$IsBadBuy, as.factor)

# 2.c. (4) Display the correlation plot of the features. Make sure the plot is clearly visible.
#####################
data.c[,-1] <- scale(data.c[,-1])
par(mfrow = c(1,1))
corr_matrix = cor(data.c[,-1])
corrplot(corr_matrix,tl.cex = 0.7)


# 2.d. (2) find features that have a correlation of over 0.65
##################
Correlated.features <- rownames(which(0.65 < corr_matrix & corr_matrix  < 1, arr.ind = TRUE))
print (cat("Correlated features: ", Correlated.features))


# 2.e (1) Save new data frames that will hold data.c, train.c and test.c without the highly correlated features and name them with the suffix .noHighCor (see .R file)
#################
data.c.noHighCor <-  data.c[,!colnames(data.c) %in% Correlated.features]
train.c.noHighCor <-  train.c[,!colnames(train.c) %in% Correlated.features]
test.c.noHighCor <-  test.c[,!colnames(test.c) %in% Correlated.features]


# 3. KNN
##################

# 3.a (4) With the new train and test data frames - predict the test.c outcomes using knn, with k=1.
######################
#split to x (the features) and y, for train and test
train.c.noHighCor.x <- train.c.noHighCor[,-1]
train.c.noHighCor.y <- train.c.noHighCor[,1]
test.c.noHighCor.x <- test.c.noHighCor[,-1]
test.c.noHighCor.y <- test.c.noHighCor[,1]

knn.model <- knn1(train.c.noHighCor.x, test.c.noHighCor.x, train.c.noHighCor.y)


# 3.b (2) Display the confusion matrix:
##############
table(knn.model, test.c.noHighCor.y)

# 3.c (5) Using cross-validation train a knn model. Use input parameters to center and scale beforehand and 
# to have the model train a few different k's (but no more than 3). This has to run in less than 2 minutes.
############
#Using LOOCV. No additional input parameters needed. checking 5,10,15 
knn.5.model <- knn.cv(train.c.noHighCor.x, train.c.noHighCor.y, k=5) 
best_accuracy = sum(diag(table(knn.5.model, train.c.noHighCor.y)))/dim(train.c.noHighCor)[1]
best_k = 5

knn.10.model <- knn.cv(train.c.noHighCor.x, train.c.noHighCor.y, k=10)
if (best_accuracy < sum(diag(table(knn.10.model, train.c.noHighCor.y)))/dim(train.c.noHighCor)[1]){
  best_accuracy = sum(diag(table(knn.10.model, train.c.noHighCor.y)))/dim(train.c.noHighCor)[1]
  best_k = 10
}
knn.15.model <- knn.cv(train.c.noHighCor.x, train.c.noHighCor.y, k=15)
if (best_accuracy < sum(diag(table(knn.15.model, train.c.noHighCor.y)))/dim(train.c.noHighCor)[1]){
  best_accuracy = sum(diag(table(knn.15.model, train.c.noHighCor.y)))/dim(train.c.noHighCor)[1]
  best_k = 15
}
cat("Best K is: ", toString(best_k))

# 3.d (2) Use the model you trained to predict the test data's labels:
##############
#using the best model means using the best k
knn.model <- knn(train.c.noHighCor.x, test.c.noHighCor.x, train.c.noHighCor.y, k=best_k, prob = TRUE)

# 4. ROC
###################

# 4.a. (6) Display the ROC of the model you trained.
##################
ROC <- roc(test.c.noHighCor.y, attr(knn.model,"prob"))
plot(roc(test.c.noHighCor.y, attr(knn.model,"prob")), main = paste("ROC: ",toString(best_k),"-NN", sep = ""))


# 5. PCA 
###########

# 5.a (3) Use train.c to find its principal components (pay attention to input parameters):
##########
pc.model <- prcomp(train.c[,-1],center = TRUE, scale. = TRUE) 


# 5.b (2) plot the drop in the variance explained by the PC's:
##########
barplot(pc.model$sdev^2, xlab = "Principal components", ylab = "Variance",main = "Drop in the variance explained by the PC's")


# 5.c (6) Using the PC's you created above, create two new data frames named train.c.pca and test.c.pca in which the features
# are replaced by PCs (our 'new features').This is slightly difficult - so look for examples online.
###############
train.c.pca <- pc.model$x
#pc.model$rotation contains the eigenvectors. We use them with matrix multiplications
test.c.pca <- t(t(pc.model$rotation) %*% t(test.c[,-1]) )


# 5.d (3) Using only the first 3 PC's - fit a simple knn model (like the first one we did) with k=7:
###########
#using the needed data (train, test, cl) straight from our own data
knn.7.pc.model <- knn(train.c.pca[,1:3], test.c.pca[,1:3], train.c[,1], k=7)



# 5.e (2) Show the confusion matrix (use the same code from the first part of the CARVANA analysis but change the data):
##########
table( knn.7.pc.model, test.c[,1])



############
# DIABETES #
############


# 6. PREPARATIONS
##############

# 6.a (0) Load the data in the appropriate format and name it 'data.d:
#############
data.d <- read.csv('Diabetes/diabetes.csv', header =  TRUE)


# 6.b (2) We can notice that many rows have SkinThickness value of 0.This can't be possible and this are probably missing values.
# Transform all 0 values in this feature to NA.
#############
data.d[data.d$SkinThickness==0, 'SkinThickness'] <- NA



# 6.c.(2) Impute the missing values for the feature SkinThickness using the mean (disregard missing values of categoricals):
#############
data.d[is.na(data.d$SkinThickness), 'SkinThickness'] <- mean(data.d$SkinThickness, na.rm = TRUE)


# 7. LOF 
############

# 7.a (5) plot the density of the LOF scores using all features
#################
lof.model <- lofactor(data.d[,-9], 8) # chose an arbitrary number of k=8, and used all data except Outcome
plot(density(lof.model), main = "Density of the LOF scores using all features", xlab = "lof values")


# 7.b. (4) Based on the plot above - remove outliers above a certain LOF score threshold:
###########
# Visually a good threshold will be 1.5
# By using: sum(lof.model < 1.5) / dim(data.d.x)[1], we get that we remain with 96.48438% of the initial amount of data
data.d <- data.d[lof.model < 1.5,]


# 8. SVM 
############

# 8.a. (1) Split the data into test (30%) and train (70%) sets with respect to the target variable. Save them as train.d and test.d.
#############
# first take 70% with Outcome==1
data.d.outcome1 = data.d[data.d$Outcome == 1,]
V <- sample.split(data.d.outcome1, SplitRatio = 0.7, group = NULL )
train.d <- data.d.outcome1[V,]
test.d <- data.d.outcome1[!V,]
#Second add 30% with Outcome==0
data.d.outcome0 = data.d[data.d$Outcome == 0,]
V <- sample.split(data.d.outcome0, SplitRatio = 0.7, group = NULL )
train.d <- rbind(train.d, data.d.outcome0[V,])
test.d <- rbind(test.d, data.d.outcome0[!V,])

# 8.b. (4) Create an SVM model with as many features as possible. Your grade for this will be based on your error rate (computed below).
# Use it to predict the test labels and save the predictions with the name res.
############
#split to x (the features) and y, for train and test
train.d.x <- train.d[,-9]
train.d.y <- train.d[,9]
test.d.x <- test.d[,-9]
test.d.y <- test.d[,9]
# Used all features. no reason not to.
svm.model <- svm(x = train.d.x[,], y = train.d.y)
res <- predict(svm.model, test.d.x[,])
# 8.c. (1) compute the error rate:
############
1-mean(test.d$Outcome == round(res))
# got 0.2540984

# 8.d. (6) Tune the SVM model using no more than 5 different costs, 5 different gammas and 5 CV. Full points if you improve your error rate below 23%.
###########
#typical ranges are found online. base of gamma values is 0/25 because we chose 4 features and the its default is 1/ncol(x)
svm_tune <- tune(svm, train.x=train.d.x[,], train.y=train.d.y, ranges=list(cost=c(0.001, 0.01, 0.5, 2), gamma=0.25^(0:4)))


# 8.e. (3) display the best model found (its parameters) and use it to predict the test values - save the predictions with the name res2.
##################
svm_tune$best.parameters
svm.best.model <- svm(x = train.d.x[,],y = train.d.y, cost=svm_tune$best.parameters[1,1], gamma = svm_tune$best.parameters[1,2])
res2 <- predict(svm.best.model2, test.d.x[,])

# 8.f. (1) show if it improved by computing the new error rate: 
###########
1-mean(test.d$Outcome == round(res2))
# got 0.2295082


# 9. RANDOM FOREST
#################

# 9.a. (6) Create a random forest model with as many features as possible (but choose with logic and looking at the data). Use no more than 2000 trees.
###############
#Pregnancies, SkinThickness, and age seem less relevant. Taking the rest
selected.rf.features <- c("Glucose","BloodPressure","Insulin","BMI","DiabetesPedigreeFunction")
# It is probably a bit more proper to factorize the Outcome column (even a warning appears which states that)
# but we want to use the error rate exactly the same as before for comparison
rf.model <- randomForest(train.d.x[,selected.rf.features], train.d.y, importance = TRUE, ntree = 2000)

# 9.b. (1) Use your model to predict the test outcome and save your predictions as resForest.
###########
resForest <- predict(rf.model, test.d.x)


# 9.c. (1) display the error rate:
#####
1-mean(test.d$Outcome == round(resForest))


# 9.d. (3) Find a function that plots the importance of the variables to see how each variable, when taken out, affected the accuracy and gini measures:
##########
varImpPlot(rf.model, main = "Variables importance: accuracy(%IncMSE), gini measures(IncNodePurity)")


###########
# MOVIES #
###########


# 10. KMEANS
#############

# 10.a. (0) Load the data in the appropriate format and name it 'data.m:
#############
data.m <- read.csv('Movies/movies.csv', header =  TRUE)


# 10.b. (4) using the features: 'rating','year','votes','length'
# run kmeans using 6 centers.
########
selected.kmeans.features <- c("rating","year","votes","length")
data.m <- as.data.frame(scale(data.m[,selected.kmeans.features]))
kmeans.6.model <- kmeans(data.m, 6)


# 10.c. (3) plot the clusters:
#######
# for all plots use: plot(data.m,col=kmeans.6.model$cluster,main='data.m clusters by Kmeans(6) - all cross features')
# Clearly, the best discriminant features are the "rating" & "year", so we use:
plot(data.m[,c('rating','year')],col=kmeans.6.model$cluster,main='data.m clusters by Kmeans(6): year-rating')


# 10.d. (2) display the centers - do they seem to make sense?
###########
# for a plot of all the centers for all cross-features, can use:
# plot(as.data.frame(kmeans.6.model$centers),col=c(1:6),pch=23, bg='black', main='data.m centers by Kmeans(6) - all cross features')
# it shows that all of the centers are pretty close to each other (notice that the scale is much smaller). for our case, we display them
# on top of the 2 features plotted before.
points(as.data.frame(kmeans.6.model$centers[,c("rating","year")]),col=c(1:6),pch=23, bg=c(1:6),cex=4)
# Notice that even in the 2 best discriminant features, we can't find 6 seperated (or even closely seperated) clusters.
# This is shown even better by the closeness of the centers. So, this means, that the true prior of the data (assuming it is 
# constructed by centroids) consists of less than 6 centroids (probably more like 3 or 4).

