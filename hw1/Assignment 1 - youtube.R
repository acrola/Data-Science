# 0-3807834-1, 0-3917605-2

# install.packages("ggplot2")
library(ggplot2)
# 1.a. Download and extract the data from moodle into a local folder designated for this assignment.


# 1.b. (2) Set your working directory to be your
# assignment folder for easy access.
################################
setwd("C:\\Users\\Ofer\\Dropbox\\Shared Folders\\Shany & Ofer\\Data Science\\ex1")



# 1.c. (4) Import the text file 2.txt into R and save it
# by the name 'data'. This is slightly tricky so here's a
# hint - make sure you're using relevant input parameters.
################################
data <- read.delim(file = '2.txt', header = FALSE)





# 1.d. (2) Use a command that shows you the first few
# rows of the data. Check that the function you used 
# above read the file appropriately.
################################
head(data)



# 2.a. (3) As you can see the names of the variables are 
# "V1", "V2", etc. and are not very informative. Rename 
# the first 9 columns to the following (use this code):
# c('Video_ID', 'uploader', 'age', 'category', 'length', 'views', 'rate', 'ratings', 'comments')
################################
names(data) <- c('Video_ID', 'uploader', 'age', 'category', 'length', 'views', 'rate', 'ratings', 'comments')





# 2.b. (4) Remove from the dataset columns 10 (inclusive) and
# above as we will not be using them (notice there is a long
# way and a short way of doing this - 2 points off for long). 
################################
data <- data[,1:9]


# 2.c. (2) Write a function that counts the number of
# rows that contain missing values.
###############################
sum(!complete.cases(data))



# 2.d. (2) According to the strategies you've learned - 
# why does it make sense to remove these rows? (no code)
###############################
# ANSWER: Imputation might distort the data. The incomplete cases are only 0.09%
# of the data, so the overall effect is negligible.





# 2.e. (2) Use a function to show the list of categories
# that exist - notice there's an empty category "" 
# (we'll return to it later).
###############################
levels(data$category)




# 2.f (2) Remove the rows that contain missing values.
###############################
data <- na.omit(data)




# 2.g (5) If you check category levels again you will see all
# categories including the empty "" category still exist (even
# though it's no longer in our data). Find a way to update this
# change so that the next time you run 'levels' on the category
# column you will see that "" disappeared. 
###############################
data$category <- factor(data$category)




# 3.a. (5) Display a bar chart plotting the number
# of uploads per category. Display the category
# names vertically.
##############################
args <- sort(table(data$category), decreasing = TRUE) 
plot <- barplot(args, las=2, ylim=c(0, 3000), main='Number of uploads per category')




# 3.b. (5) Use a function to show the values above each bar.
##############################
text(x=plot, y=args, label=args, pos=3)




# 3.c. Part of Google's revenue comes from advertisements on YouTube. Google 
# wants to provide uploaders with guidelines of how to make a good video so
# that ratings are higher, possibly leading to more views and therefore 
# more $$$ for Google. It is likely that different categories will have different
# guidelines, so as a pilot Google wants to choose only one category to start with.

# 3.c.i (4) To get a feel for which categories are doing well 
# and which not - plot the density of the ratings by category
# colored by category.
########################

ggplot(data, aes(rate)) +
  geom_density(aes(group=category, color=category, fill=category), alpha=1/5) +
  ggtitle('Density of rate per category')



# 3.c.ii (2) The density plot was only good enough for a general feel, but it's still
# hard to understand which category is a good candidate for the guidelines pilot. A 
# more informative plot is the boxplot. Run a function to show the boxplot of 
# rating by category, colored by category.
########################
ggplot(data, aes(category, rate, fill=category)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('Boxplot of rate per category')




# 3.d (6) Based solely on the boxplot above choose the right answer:
# If Google wants to provide potential uploaders with guidelines in the aim 
# of increasing the number of videos that get a high rating they should:

# A. Focus on Travel & Events category because its variance is very small.
# B. Focus on the Science & Technology category as more than 22% of the videos have a rating below 1.
# C. Focus on the Music category because there are too many outliers in the first quartile.
########################
# ANSWER: B



# 3.e (1) Use your code from 3.c.ii to show the boxplot of views by 
# category, colored by category. As you will see there's not much
# we can understand due to the outliers:
########################
ggplot(data, aes(category, views, fill=category)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('Boxplot of views per category')




# 3.f. (5) To make the plot clearer use the boxplot method to
# detect the samples that are outliers. 
########################
catEnumerated <- levels(data$category)

# build vectors for lower and upper limits
quantMin <- c()
quantMax <- c()

for (i in catEnumerated) {
  quantMin[i] <- (quantile(data$views[data$category==i], 0.25) - 1.5 * IQR(data$views[data$category==i]))
  quantMax[i] <- (quantile(data$views[data$category==i], 0.75) + 1.5 * IQR(data$views[data$category==i])) 
}

# create a DataFrame from the vectors
quantDF <- data.frame(min=quantMin, max=quantMax)

# add a boolean vector indicating whether an upload is an outlier IN RELATION TO 
# THE CATEGORY
data$is_outlier <- ((data$views > quantDF$max[data$category]) | (data$views < quantDF$min[data$category]))




# 3.g. (4) Plot the boxplot again using the same code from 3.e. but 
# this time the input data will be without the outlier rows. 
########################
ggplot(data[data$is_outlier==FALSE,], aes(category, views, fill=category)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('Number of views per category')





# 3.h. (2) Comparing this graph to the barplot (of number of uploads
# per category from the beginning of the exercise) answer the following:
# T/F - there is a mismatch between the 3 most uploaded categories and the 
# 3 categories with highest median number of views (i.e. people are 
# uploading a lot of videos to categories that are generally less viewed)
########################

# ANSWER:  T



# 4.a (6) If it's more views we're after - one of the recommendations 
# for creating a highly viewed video can be related to the length
# of the video. Fill in the following code. You are requested to fill
# in a vector of correlation values: each index in the vector represents 
# one category and its value is the correlation coefficient between 
# views and length for that category.
########################
catEnumerated <- levels(data$category)

corVec <- c()

for (i in catEnumerated) {
  catLength <- data$length[data$category==i]
  catViews <- data$views[data$category==i]
  corVec[i]<- cor(catLength, catViews)
  print(catEnumerated[i])
  print(corVec[i])
}



# 4.b (3) Find a way (using R code) to display the name
# of the category for which the correlation between
# views and length is maximal.
########################
names(corVec[corVec==max(corVec)])




# 4.c. Transformations and correlation:

# 4.c.i (3)  True/False - The correlation coefficient of
# two variables is affected by normalizing them first to 0 
# mean and unit variance. Prove your answer with code showing
# the before and after (you can use catLength and catViews from 3.e).
########################
# ANSWER: False

# PROOF: 
# normalize
data$length_norm <- (data$length - mean(data$length)) / sd(data$length)
data$views_norm <- (data$views - mean(data$views)) / sd(data$views)

corVecNorm <- c()

for (i in catEnumerated) {
  catLength <- data$length_norm[data$category==i]
  catViews <- data$views_norm[data$category==i]
  corVecNorm[i]<- cor(catLength, catViews)
}
# check whether all terms are equal
ans <- all.equal(corVec, corVecNorm)
print(c("Are the two vectors equal?", ans))


# 4.c.ii (3) -  T/F - The correlation coefficient of two variables is 
# affected by log-scaling them first. Prove your answer by showing
# the before and after (you can use catLength and catViews from 3.e).
########################
# ANSWER: T

# PROOF:
data$length_log <- log(data$length)
data$views_log <- log(data$views)

corVecLog <- c()

for (i in catEnumerated) {
  catLength <- data$length_log[data$category==i]
  catViews <- data$views_log[data$category==i]
  corVecLog[i]<- cor(catLength, catViews)
}

ans <- all.equal(corVec, corVecLog)
print(c("Are the two vectors equal?", ans))


# 4.c.iii (3) - T/F - correlation cannot be affected by outliers. 
# 5 bonus points for proving with code on any data.
########################
# ANSWER: F

# PROOF (bonus):
quantMinLen <- c()
quantMaxLen <- c()

for (i in catEnumerated) {
  quantMinLen[i] <- (quantile(data$length[data$category==i], 0.25) - 1.5 * IQR(data$length[data$category==i]))
  quantMaxLen[i] <- (quantile(data$length[data$category==i], 0.75) + 1.5 * IQR(data$length[data$category==i])) 
}

quantLenDF <- data.frame(min=quantMinLen, max=quantMaxLen)
data$is_outlier_len <- ((data$length > quantLenDF$max[data$category]) | (data$length < quantLenDF$min[data$category]))


corVecOut <- c()

dataOut <- data[data$is_outlier==FALSE & data$is_outlier_len==FALSE,]


for (i in catEnumerated) {
  catLength <- dataOut$length[dataOut$category==i]
  catViews <- dataOut$views[dataOut$category==i]
  corVecOut[i]<- cor(catLength, catViews)
}

ans <- all.equal(corVec, corVecOut)
print(c("Are the two vectors equal?", ans))



# 5.a. (3) Let's take a look at how much Google profits from 
# YouTube ads versus how much they need to pay uploaders:
# In order to do so first run the following code and
# explain in one short sentence what do the first and 
# third lines of code do:
###########################
subsetOfData = data[(data$category == "Music") | (data$category == "Gaming") | (data$category == "Sports"),]
levels(subsetOfData$category)
subsetOfData$category<-factor(subsetOfData$category)
levels(subsetOfData$category)

#ANSWER: The first line of code subsets only rows with categories equal to Music /
# Gaming / Sports (with all the columns), and the third line updates the actual
# factor levels



# 5.b. (6) Use a function to partition the length of the videos into 3 categories:
# short (up to 60 seconds)
# medium (between 60 seconds and 15 minutes) and
# long (above 15 minutes) and name them "short", "medium", "long":
###########################

data$length_C <- as.character(factor(findInterval(data$length, c(0, 60, 900), rightmost.closed = TRUE),labels = c("short", "medium", "long")))


# 5.c. (2) Use the function above to partition the number of views into 2 categories:
# <=10000
# > 10000
# and name them "<=10000" and ">10000".
###########################

data$views_C <- as.character(factor(findInterval(data$views, c(0, 10000), rightmost.closed = TRUE),labels = c("<=10000", ">10000")))


# 5.d. (3) Run a mosaicplot displaying the relation
# between category, length category and view category
# (the two you created above) with data = subsetOfData provided in 5.a.
###########################
subsetOfData$length_C <- as.character(factor(findInterval(subsetOfData$length, c(0, 60, 900), rightmost.closed = TRUE),labels = c("short","medium","long")))
subsetOfData$views_C <- as.character(factor(findInterval(subsetOfData$views, c(0, 1000), rightmost.closed = TRUE),labels = c("<=10000", ">10000")))
mosaicplot(~category + length_C + views_C, data = subsetOfData, main = 'Category by length and views', color=TRUE)


# 5.e. (2) Show that the mean number of videos per uploader is close to 1.
###########################
(dim(data)[1]) / (length(levels(data$uploader)))



# 5.d. Answer the following based on the mosaicplot and the fact that people
# usually upload only one video (as seen above):

# 5.d.i (2) Assuming it takes a minimum of around 10,000 views per video to gain
# enough ad views before Google starts paying you, which category, length cut,
# and view cut are the most profitable for Google? Your answer should 
# be in the following format: (category, lengthCut, viewCut)
###########################
# ANSWER: (Music, medium, <=10000)





# 5.d.ii (2) T/F - In the music category when comparing short and medium length 
# videos uploaders who want to earn money form ads are generally 
# better off creating short videos. (If you think about it your answer
# should not only match the mosaicplot but it should also make some sense).
###########################
# ANSWER: F




# A final thought - following these results, does Google run a good business
# model with YouTube? Is it wise to think you can make a solid earning
# out of uploading? (answer these to yourself).



