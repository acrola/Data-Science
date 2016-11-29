#ID 1: 203056585 
#ID 2: 301380416 


# 1.a. Download and extract the data from moodle into a local folder designated for this assignment.


# 1.b. (2) Set your working directory to be your
# assignment folder for easy access. From now on do not use the full path,
# but only the name of the file within this path.
################################
setwd("C:\\Users\\Avi\\AppData\\Roaming\\SPB_16.6\\Data-Science")
setwd("C:\\Users\\Avi\\AppData\\Roaming\\SPB_16.6\\Data-Science")


# 1.c. (3) Import the CSV file "movies.csv" into R and save it
# by the name data. Notice the data in the file already has row numbers(so pay attention to the function arguments).
################################
data <- read.csv(file = 'movies.csv', header = FALSE)



# 1.d. (1) The features "r1","r2"...r10" will not be relevant for us, remove them from the data.
################################



# 1.e. (2) show all features names and the data type each one contains. 
################################


# 2.a. (2) Use a command that shows you the first few
# rows of the data. Check that the function you used 
# above read the file appropriately.
################################
head(data)


# 2.b.(3) This data contains many movies with a few number of votes, delete all movies with less then 100 votes
#for better understanding of the data.
################################


# 2.c. (4) Categorical features: We can see (from 1.e) that the features "Action","Animation",...,"short"
# are all from the type integer, but they should be logical (True/False).
# Transform those features to logical type features.
# Why we can't combine  all this features into one categorical feature?
################################



# 2.d. (5) Numeric features: Create a summary statistics: mean, standard deviation, variance, min, max, median, range, and quantile
#for each numeric feature in the data.
################################



# 3.a. (3) Count the number of rows that have missing values.
# write the name off the column with the most missing values
################################



# 3.b. (6)  According to the strategies you've learned -
# Does it makes sense to remove the rows with missing values??? Explain
# if so, remove all rows with the missing values
# if not, find different way to deal with the missing values and implement it.
################################



# 4.a. (4) Make a qq plot for each of the following features: year,rating,votes.
# Explain what we can learn from a qq plot about the distribution of the data.
################################



# 4.b. (7) According to the qq plots,Do we need to normalize all the featuress? Which feature 
# must be normalize? which normalization function should we use for this feature?
# create new column with the normalized data (eg. norm.votes)
################################



# 4.c. (3) Give 3 ways to normalize data and explain why it is important to do so.
################################


# 5.a. (2) Create one plot containing all box plots for each of the numeric features of the data, 
# (except the feature you normalized, insted used the normalized data).
################################




# 5.b. (4) Make a box plot for each feature individually and
# give 2 observations you can see from the box plots (there is more than 2).
################################




# 5.c. (7) We can notice the feature 'length' has many suspected outliers.
# Remove those suspected outliers from the data using the box plot.
################################




# 5.d. (7) Use the LOF measure to remove outliers using the following features: "votes","length","rating".
# Use k=20 and remove all instances that their LOF score is above 1.5
################################



# 6.a. (5) Display a bar chart plotting the number
# of movies per genre. Display the genre names vertically, each genre column in a different color.
################################



# 6.b. (12) The length of a movie and the number of votes is a critical factor for determining the film rating.
# Create a Vector with 3 levels of a movie length (Long>110/ 90<=Medium<=110 / Short< 90)
# Create a Vector with 2 levels of votes number (Many votes>500 / Few Votes<500)
# Compute the average rating for each sub category (e.g: average rank of long movie with few votes)
# Can you spot a trend? 
################################




# 6.c. (5) Make 2 density plots:
# 1. density plot of ratings by the length levels colored by the length levels(the one you created in 6.b.)
# 2. density plot of ratings by the votes levels colored by the votes levels(the one you created in 6.b.)
# Does this plots support your claim from 6.b?
################################




# 7.a. (7) Display the correlation plot of the features. Make sure the plot is clearly visible 
# use  scaled features (as given in the code)
################################




# 7.b. (5) find features that have a correlation of over 0.5 and correlation of less than 0.
# Can you see a clear trend? Can we get rid of one of those features?
################################



