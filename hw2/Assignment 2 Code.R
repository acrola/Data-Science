#ID 1: 203056585
#ID 2: 301380416

# Set your working directory to be your assignment folder. In that folder create three sub-folders named: 'Carvana', 'Diabetes' and 'Movies' in this exact format.
# Download the Carvana, Diabetes and movies files from moodle and make sure that that your folder hierarchy is as such: 
# "../Carvana/CARVANA.csv" , "../Diabetes/diabetes.csv" and "../Movies/movies.csv". 
# The readme file can be wherever you wish.

# Set your working directory to be the assignment folder (not the nested folders).
################################
setwd("C:\\Users\\Avi\\AppData\\Roaming\\SPB_16.6\\Data-Science\\hw2\\Carvana")



################################

# All packages installetions:
##############
library(caTools)
library(lattice)
library(caret)




###########
# CARVANA #
###########


# 1. PREPARATION
##############

# 1.a. (0) Load the data in the appropriate format and name it 'data.c':
################################
data.c <- read.csv(file
                 = 'CARVANA.csv',
                 header = TRUE,
                 na.strings = c("", "NA")) # We replaced the null values with 'NA'.
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
V <- sample.split(data.c, SplitRatio = 0.7, group = NULL )
train.c <- data.c[V,]
test.c <- data.c[!V,]



# 2. FEATURE SELECTION AND CORRELATION
###################

# 2.a. (2) Convert all factorial features to numeric.
#######################
train.c <- data.frame(x=c("NaN","2"),y=c("NaN","3"),stringsAsFactors=FALSE)
train.c <- as.data.frame(sapply(train.c, as.numeric)) #<- sapply is here
test.c <- data.frame(x=c("NaN","2"),y=c("NaN","3"),stringsAsFactors=FALSE)
test.c <- as.data.frame(sapply(test.c, as.numeric)) #<- sapply is here


# 2.b. (1) Convert the lable feature('IsBadBuy') to factor with 2 levels (True/False)
#######################
train.c$IsBadBuy <- as.character(factor(findInterval(train.c$IsBadBuy, c(0, 1), rightmost.closed = TRUE),labels = c("True", "False")))


# 2.c. (4) Display the correlation plot of the features. Make sure the plot is clearly visible.
#####################



# 2.d. (2) find features that have a correlation of over 0.65
##################



# 2.e (1) Save new data frames that will hold data.c, train.c and test.c without the highly correlated features and name them with the suffix .noHighCor (see .R file)
#################




# 3. KNN
##################

# 3.a (4) With the new train and test data frames - predict the test.c outcomes using knn, with k=1.
######################




# 3.b (2) Display the confusion matrix:
##############




# 3.c (5) Using cross-validation train a knn model. Use input parameters to center and scale beforehand and 
# to have the model train a few different k's (but no more than 3). This has to run in less than 2 minutes.
############




# 3.d (2) Use the model you trained to predict the test data's labels:
##############



# 4. ROC
###################

# 4.a. (6) Display the ROC of the model you trained.
##################




# 5. PCA 
###########

# 5.a (3) Use train.c to find its principal components (pay attention to input parameters):
##########



# 5.b (2) plot the drop in the variance explained by the PC's:
##########



# 5.c (6) Using the PC's you created above, create two new data frames named train.c.pca and test.c.pca in which the features
# are replaced by PCs (our 'new features').This is slightly difficult - so look for examples online.
###############



# 5.d (3) Using only the first 3 PC's - fit a simple knn model (like the first one we did) with k=7:
###########




# 5.e (2) Show the confusion matrix (use the same code from the first part of the CARVANA analysis but change the data):
##########




############
# DIABETES #
############


# 6. PREPARATIONS
##############

# 6.a (0) Load the data in the appropriate format and name it 'data.d:
#############



# 6.b (2) We can notice that many rows have SkinThickness value of 0.This can't be possible and this are probably missing values.
# Transform all 0 values in this feature to NA.
#############




# 6.c.(2) Impute the missing values for the feature SkinThickness using the mean (disregard missing values of categoricals):
#############



# 7. LOF 
############

# 7.a (5) plot the density of the LOF scores using all features
#################



# 7.b. (4) Based on the plot above - remove outliers above a certain LOF score threshold:
###########



# 8. SVM 
############

# 8.a. (1) Split the data into test (30%) and train (70%) sets with respect to the target variable. Save them as train.d and test.d.
#############




# 8.b. (4) Create an SVM model with as many features as possible. Your grade for this will be based on your error rate (computed below).
# Use it to predict the test labels and save the predictions with the name res.
############




# 8.c. (1) compute the error rate:
############




# 8.d. (6) Tune the SVM model using no more than 5 different costs, 5 different gammas and 5 CV. Full points if you improve your error rate below 23%.
###########




# 8.e. (3) display the best model found (its parameters) and use it to predict the test values - save the predictions with the name res2.
##################




# 8.f. (1) show if it improved by computing the new error rate: 
###########
1-mean(test.d$Outcome == round(res2))



# 9. RANDOM FOREST
#################

# 9.a. (6) Create a random forest model with as many features as possible (but choose with logic and looking at the data). Use no more than 2000 trees.
###############



# 9.b. (1) Use your model to predict the test outcome and save your predictions as resForest.
###########



# 9.c. (1) display the error rate:
#####



# 9.d. (3) Find a function that plots the importance of the variables to see how each variable, when taken out, affected the accuracy and gini measures:
##########



###########
# MOVIES #
###########



# 10. KMEANS
#############

# 10.a. (0) Load the data in the appropriate format and name it 'data.m:
#############




# 10.b. (4) using the features: 'rating','year','votes','length'
# run kmeans using 6 centers.
########



# 10.c. (3) plot the clusters:
#######



# 10.d. (2) display the centers - do they seem to make sense?
###########






