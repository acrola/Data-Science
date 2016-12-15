#ID 1: 203056585 
#ID 2: 301380416 


# 1.a. Download and extract the data from moodle into a local folder designated for this assignment.


# 1.b. (2) Set your working directory to be your
# assignment folder for easy access. From now on do not use the full path,
# but only the name of the file within this path.
################################
#setwd("C:\\Users\\Avi\\AppData\\Roaming\\SPB_16.6\\Data-Science")
setwd("C:\\Users\\dor\\Documents\\Data-Science")


# 1.c. (3) Import the CSV file "movies.csv" into R and save it
# by the name data. Notice the data in the file already has row numbers(so pay attention to the function arguments).
################################
data <- read.csv(file = 'movies.csv', header = TRUE)


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
data <- data[!data$votes<100,]


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
summary(data[3:7]) #TODO this is not good enough. cant find a summry type which answers these questions exactly


# 3.a. (3) Count the number of rows that have missing values.
# write the name off the column with the most missing values
################################
sum(!complete.cases(data))
i <- 1
max.missing_values <- 0
max.name <- ''
for (category in names(data)){
  print ("the sum ",sum(!complete.cases(data[[category]])))
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
# ANSWER: there are 12009 rows with missing values and only 3704 without.
#         removing those rows will cause losing to much data.
# the missing values are only at the "budget" column so we will just remove it.
#TODO: might be better to take the median of budget
data <- data[,!names(data) == "budget"]


# 4.a. (4) Make a qq plot for each of the following features: year,rating,votes.
# Explain what we can learn from a qq plot about the distribution of the data.
################################
# ANSWER: TODO
qqnorm(data$rating, main = "Normal Q-Q Plot - rating")
qqnorm(data$year, main = "Normal Q-Q Plot - year")
qqnorm(data$votes, main = "Normal Q-Q Plot - votes")


# 4.b. (7) According to the qq plots,Do we need to normalize all the featuress? Which feature 
# must be normalize? which normalization function should we use for this feature?
# create new column with the normalized data (eg. norm.votes)
################################
# ANSWER: TODO
data$norm.votes <- scale(data$votes)
qqnorm(data$norm.votes)

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
#TODO: doesnt make sense
boxplot(data[ , (names(data) %in% c("year", "leangth", "rating", "norm.votes"))])


# 5.b. (4) Make a box plot for each feature individually and
# give 2 observations you can see from the box plots (there is more than 2).
################################
# ANSWER: TODO
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
data <- data[!(data$length>140 | data$length<60),]


# 5.d. (7) Use the LOF measure to remove outliers using the following features: "votes","length","rating".
# Use k=20 and remove all instances that their LOF score is above 1.5
################################
# TODO: cant install packages


# 6.a. (5) Display a bar chart plotting the number
# of movies per genre. Display the genre names vertically, each genre column in a different color.
################################"Short"
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
# ANSWER: The longer a movie is and the more votes it gets, the higher its rating.
length.binned <- ifelse(data$length>110, ('Long'), ifelse(data$length<90, ('Short'), ('Medium')))
votes.binned <- ifelse(data$votes>500, ('Many votes'), ('Few Votes')) 
average.byLength <- c(mean(data[data$length == 'Long',]$rating),mean(data[data$length == 'Medium',]$rating),mean(data[data$length == 'Short',]$rating))
average.byLength <- c(mean(data[data$length == 'Long',]$rating),mean(data[data$length == 'Medium',]$rating),mean(data[data$length == 'Short',]$rating))

level.length <- c('Long','Medium','Short')
level.votes <- c('Many votes', 'Few votes')
data.long <- data[data$length > 110,]
data.medium <- data[90 <= data$length & data$length <= 110,]
data.short <- data[data$length < 90,]
data.manyvotes <- data[data$votes > 500,]
data.fewvotes <- data[data$length <= 500,]
average.long <- c(mean(data.long[data.long$votes > 500,]$rating), mean(data.long[data.long$votes <= 500,]$rating))
average.medium <- c(mean(data.medium[data.medium$votes > 500,]$rating), mean(data.medium[data.medium$votes <= 500,]$rating))
average.short <- c(mean(data.short[data.short$votes > 500,]$rating), mean(data.short[data.short$votes <= 500,]$rating))

average.long
average.medium
average.short

# 6.c. (5) Make 2 density plots:
# 1. density plot of ratings by the length levels colored by the length levels(the one you created in 6.b.)
# 2. density plot of ratings by the votes levels colored by the votes levels(the one you created in 6.b.)
# Does this plots support your claim from 6.b?
################################
# ANSWER: ?????
length.rating.average <- c(mean(data.short$rating),mean(data.medium$rating),mean(data.long$rating))
votes.rating.average <- c(mean(data.short$rating),mean(data.medium$rating))



# 7.a. (7) Display the correlation plot of the features. Make sure the plot is clearly visible 
# use  scaled features (as given in the code)
################################




# 7.b. (5) find features that have a correlation of over 0.5 and correlation of less than 0.
# Can you see a clear trend? Can we get rid of one of those features?
################################



