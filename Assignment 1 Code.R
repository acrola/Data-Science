#ID 1: 203056585 
#ID 2: 301380416 


# 1.a. Download and extract the data from moodle into a local folder designated for this assignment.


# 1.b. (2) Set your working directory to be your
# assignment folder for easy access. From now on do not use the full path,
# but only the name of the file within this path.
################################
setwd("C:\\Users\\Avi\\AppData\\Roaming\\SPB_16.6\\Data-Science")
#setwd("C:\\Users\\dor\\Documents\\Data-Science")


# 1.c. (3) Import the CSV file "movies.csv" into R and save it
# by the name data. Notice the data in the file already has row numbers(so pay attention to the function arguments).
################################
data <- read.csv(file = 'movies.csv', header = TRUE, na.strings=c("","NA"))


# 1.d. (1) The features "r1","r2"...r10" will not be relevant for us, remove them from the data.
################################
drops <- c("r1","r2","r3","r4","r5","r6","r7","r8","r9","r10")
data <- data[ , !(names(data) %in% drops)]


# 1.e. (2) show all features names and the data type each one contains. 
################################
sapply(data, class)

# 2.a. (2) Use a command that shows you the first few
# rows of the data. Check that the function you used 
# above read the file appropriately.
################################
head(data)


# 2.b.(3) This data contains many movies with a few number of votes, delete all movies with less then 100 votes
#for better understanding of the data.
################################
data <- data[data$votes>100,]


# 2.c. (4) Categorical features: We can see (from 1.e) that the features "Action","Animation",...,"short"
# are all from the type integer, but they should be logical (True/False).
# Transform those features to logical type features.
# Why we can't combine  all this features into one categorical feature?
################################
#ANSWER:Because some movies can belong to more than 1 category.
for (category in names(data[9:15])){
  data[[category]] <- as.logical(data[[category]])
}


# 2.d. (5) Numeric features: Create a summary statistics: mean, standard deviation, variance, min, max, median, range, and quantile
#for each numeric feature in the data.
################################
data.summary <- data[,c("length", "budget", "rating", "votes","year")]
data.summary <- do.call(data.frame, list(mean = apply(data.summary, 2, na.rm=TRUE, mean),
                                           sd = apply(data.summary, 2, na.rm=TRUE, sd),
                                           variance = apply(data.summary, 2, na.rm=TRUE, var),
                                           min = apply(data.summary, 2, na.rm=TRUE, min),
                                           quantile.25 = apply(df_stat, 2, na.rm=TRUE, quantile, prob=0.25),
                                           median = apply(data.summary, 2, na.rm=TRUE, median),
                                           quantile.75 = apply(df_stat, 2, na.rm=TRUE, quantile, prob=0.75),
                                           max = apply(data.summary, 2, na.rm=TRUE, max),
                                           range = apply(data.summary, 2, na.rm=TRUE, max)-apply(data.summary, 2, na.rm=TRUE, min)))
t(data.summary)


# 3.a. (3) Count the number of rows that have missing values.
# write the name off the column with the most missing values
################################
#missing rows counts
sum(!complete.cases(data))

#column with most missing values
max.missing_values <- 0
max.name <- ''
for (category in names(data)){
  if (sum(!complete.cases(data[[category]])) > max.missing_values){
    max.name <- category
    max.missing_values <- sum(!complete.cases(data[[category]]))
  }
}
max.name


# 3.b. (6)  According to the strategies you've learned -
# Does it makes sense to remove the rows with missing values??? Explain
# if so, remove all rows with the missing values
# if not, find different way to deal with the missing values and implement it.
################################
# ANSWER: There are 12009 rows with missing values and only 3704 without.
#         removing those rows will cause losing too much data.
#         The missing values are only at the "budget" and "mpaa" columns so we will just remove them.
apply(data["budget"], 2, function(col)sum(is.na(col))/length(col))
apply(data["mpaa"], 2, function(col)sum(is.na(col))/length(col))
# With the command above, we can see that there is 76.4% of missing values in the budget columns, and 75.7% in the mpaa columns. 
# Therefore, we will remove them:
data <- data[,!names(data) %in% c("budget","mpaa")]

# 4.a. (4) Make a qq plot for each of the following features: year,rating,votes.
# Explain what we can learn from a qq plot about the distribution of the data.
################################
# ANSWER: The quantile-quantile or q-q plot is an exploratory graphical device used to check the validity of a distributional assumption for a data set.
#         In general, the basic idea is to compute the theoretically expected value for each data point based on the distribution in question.
#         We can see from the qq-plots that rating and year features are properly sampled normally, but votes doe's not, and is quit skewed.
par(mfrow=c(1,3))
qqnorm(data$rating, main = "Normal Q-Q Plot - year")
qqline(y=data[,"year"])
qqnorm(data$year, main = "Normal Q-Q Plot - rating")
qqline(y=data[,"rating"])
qqnorm(data$votes, main = "Normal Q-Q Plot - votes")
qqline(y=data[,"votes"])

# 4.b. (7) According to the qq plots,Do we need to normalize all the featuress? Which feature 
# must be normalize? which normalization function should we use for this feature?
# create new column with the normalized data (eg. norm.votes)
################################
# ANSWER: According to the qq plots we've got, we should normalize just the "votes" feature,
#         because it's qq plot extreamly not linear.
data["norm.votes"]
data$norm.votes <- (data$votes - mean(data$votes)) / sd(data$votes)
#data$norm.votes <- (data$votes - min(data$votes)) / (max(data$votes) - min(data$votes))

# 4.c. (3) Give 3 ways to normalize data and explain why it is important to do so.
################################
# ANSWER: it is important to normalize so that the different scales of the feature won't count in our model
# in our 3 ways, denote z as the normalized sample of x:
# way 1 - re-center: scales the data to have mean 0 and sd 1. z = (x-mu)/sigma
# way 2 - rescale: scales all the data between 0 and 1. z = ( x-min(x) ) / ( max(x)-min(x) )
# way 3 - Median/MAD: a robust way to re-center and scale. z = ( x-median(x) ) / ( Median Absolute Deviation )


# 5.a. (2) Create one plot containing all box plots for each of the numeric features of the data, 
# (except the feature you normalized, insted used the normalized data).
################################
to_plot = data[,c("year","length", "rating", "norm.votes")]
boxplot(to_plot, col=topo.colors(4))

# 5.b. (4) Make a box plot for each feature individually and
# give 2 observations you can see from the box plots (there is more than 2).
################################
# ANSWER: years-      We can see that most of the years values are in the range of the 70's until the late 90's 
#                     (the median, upper quartile and lower quartile are there).
#                     We see that there are many outliers in the early years (under the minimum values)
#         rating-     We can see that most of the rating values are in the range of the 5 to 7. 
#                     (the median, upper quartile and lower quartile are there).
#                     We see that there are many outliers in the small values of rating, smaller then 3 (under the minimum values)
#         norm.votes -We see that there are many outliers in this whole feaure. probably due to the fact that this data is noisy.

boxplot(data$year)
title("year boxplot")
boxplot(data$length)
title("length boxplot")
boxplot(data$rating)
title("rating boxplot")
boxplot(data$norm.votes)
title("norm.votes boxplot")

# 5.c. (7) We can notice the feature 'length' has many suspected outliers.
# Remove those suspected outliers from the data using the box plot.
################################
# IQR = 110-90=20.removing outliers from 1.5IQR
length.boxplot <- boxplot(data$length)
data = data[!(data$length %in% length.boxplot$out),]


# 5.d. (7) Use the LOF measure to remove outliers using the following features: "votes","length","rating".
# Use k=20 and remove all instances that their LOF score is above 1.5
################################
library(DMwR)
lof.data <- data[,c("votes","length","rating")]
lof.data$length <- scale(data$length,center=TRUE,scale=TRUE)

l<- lofactor(data[,c("votes",
                     "length","rating")], k=20)
outliers <- l > 1.5

data <- data[!outliers,]


# 6.a. (5) Display a bar chart plotting the number
# of movies per genre. Display the genre names vertically, each genre column in a different color.
################################
movies.num <- c(sum(data$Action==TRUE),sum(data$Animation==TRUE),sum(data$Comedy==TRUE),sum(data$Drama==TRUE),
                sum(data$Documentary==TRUE),sum(data$Romance==TRUE),sum(data$Short==TRUE))
barplot(movies.num, main = "movies per genre", names.arg = c("Action","Animation","Comedy","Drama","Documentary","Romance","Short"),
        col = c('blue','red','green','yellow','purple','black','pink'))

# 6.b. (12) The length of a movie and the number of votes is a critical factor for determining the film rating.
# Create a Vector with 3 levels of a movie length (Long>110/ 90<=Medium<=110 / Short< 90)
# Create a Vector with 2 levels of votes number (Many votes>500 / Few Votes<500)
# Compute the average rating for each sub category (e.g: average rank of long movie with few votes)
# Can you spot a trend? 
################################
# ANSWER: The longer a movie is and the more votes it gets -> the higher it's rating is.
data$votes.desc = 'Many_votes'
data[data$votes < 500, 'votes.desc'] = 'Few_votes'
data$length.desc = 'Long'
data[data$length < 110, 'length.desc'] = 'Medium'
data[data$length < 90 , 'length.desc'] = 'Short'
votes.length <- aggregate(data$rating, by=list(data$votes.desc, data$length.desc), FUN=mean)
names(votes.length) <- c("votes","length","mean")
votes.length

# 6.c. (5) Make 2 density plots:
# 1. density plot of ratings by the length levels colored by the length levels(the one you created in 6.b.)
# 2. density plot of ratings by the votes levels colored by the votes levels(the one you created in 6.b.)
# Does this plots support your claim from 6.b?
################################
# ANSWER: Yes, from the first density plot we see that the confidance is bigger as the movie gets longer,
#         and the "peak" is more on the right as the movie gets longer, meaning as the movie gets longer, is gives a better rating.
#         from the second density plot we see that for votes number bigger from 500 we are more confident about
#         the raiting, and the "peak" is more on the right, meaning the biggest number of votes gives a better rating.
plot(density(data[data$length.desc == 'Long','rating']),xlab = "raiting",main="raiting by length",col='green')
lines(density(data[data$length.desc == 'Medium','rating']),xlab = "raiting",main="raiting by length",col='red')
lines(density(data[data$length.desc == 'Short','rating']),xlab = "raiting",main="raiting by length",col='black')
legend(1,0.42, c("Long","Medium","Short"), lwd=c(2.5,2.5),col=c("green","red","black"), box.lty=0)

#density plot of ratings by votes
plot(density(data[data$votes.desc == 'Many_votes','rating']),xlab = "raiting",main="raiting by votes",col='green')
lines(density(data[data$votes.desc == 'Few_votes','rating']),xlab = "raiting",main="raiting by votes",col='red')
legend(0.5,0.2, c("Many votes","Few votes"), lwd=c(2.5,2.5),col=c("green","red"), box.lty=0)



# 7.a. (7) Display the correlation plot of the features. Make sure the plot is clearly visible 
# use  scaled features (as given in the code)
################################
scaled <- scale(data[,c('year','length','rating','votes')],center=TRUE,scale=TRUE)
library(corrplot)
correlations = cor(scaled.data)
corrplot(correlations, method="color")


# 7.b. (5) find features that have a correlation of over 0.5 and correlation of less than 0.
# Can you see a clear trend? Can we get rid of one of those features?
################################
# ANSWER: We can see clearly from the previous section's plot that there are no deep blue colored cubics, all are colored lightly,
#         which means there are no strong correlations between the given features. We see that we have a red colored corolation between year and
#         rating features, which means that probably older movies are rated higher than recent movies, but this corrolation 
#         is weak since the red color is not deep. In conclusion, we cannot get rid of any of the features, we should keep them all.
correlations

