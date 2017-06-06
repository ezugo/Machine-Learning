
#  ------------------------------------------------------------------
#  |FILE NAME:      RecommenderSystem.R
#  |DATE:           06/05/17
#  |CREATED BY:     Ezugo Nwosu 
#  |DATA SoURCE:    http://grouplens.org/datasets/movielens/   
#  |DATA FILE Name: MOvieLens
#  |----------------------------------------------------------------


#  |------------------------------------------------------------------
#  |STEPS:               
#  |
#  |  STEP 1:  Install and load the required packages and libraries 
#  |  STEP 2:  Get the data 
#  |  STEP 3:  View and examine the data 
#  |  STEP 4:  Preprocessing and data inspection
#  |  STEP 5:  Split the data into training and test data
#  |  STEP 6:  Train the recommender model
#  |  STEP 7:  Applying the recommender model on the test set
#  |  STEP 8:  Compare the number of times each movie got recommended using the IBCF and UBCF
#  |  STEP 9:  Call out the name of the movie instead of just the ID
#  |  STEP 10: Recommender System model evaluation 
#  |------------------------------------------------------------------

#  |------------------------------------------------------------------
#  |OTHER REFERENCES:               
#  |
#  |  1:  http://2.droppdf.com/files/OonAl/building-a-recommendation-system-with-r.pdf 
#  |  2:  http://blog.yhat.com/posts/recommender-system-in-r.html 
#  |  3:  https://cran.r-project.org/web/packages/recommenderlab/vignettes/recommenderlab.pdf
#  |  4:  http://bigdata-doctor.com/recommender-systems-101-practical-example-in-r/
#  |------------------------------------------------------------------

#  *------------------------------------------------------------------*
#  | STEP 1: Install and load the required packages and libraries 
#  *------------------------------------------------------------------*

#if(!"recommenderlab" %in% rownames(installed.packages())){ install.packages("recommenderlab")}
#library("recommenderlab") 

unavailable_packages <- setdiff(c("recommenderlab", "reshape2","dplyr", "useful", "data.table", "proxy", "Matrix", "irlba" ), rownames(installed.packages()))
if (length(unavailable_packages)>0){
  install.packages(unavailable_packages)
}

##load the libraries
library(recommenderlab)
library(reshape2)
library(dplyr)
# to display the corners for large matrices
library(useful) 
library(data.table)
library(proxy)
library(Matrix)
#for svd
library(irlba)
#  *------------------------------------------------------------------*
#  | STEP 2: Get the data 
#  *------------------------------------------------------------------*
# First read the file into R as a data frame. Change this path to the file path on your computer.
# View the contents of the downloaded directory
dir("C:/Users/enwosu/Documents/RStudio_WorkFile/ML-R-Code/RecommenderSystems/ml-100k")

# Now read in the u.data file. It contains 100,000 ratings by 943 users on 1,682 movies. 
MovieLense_data = read.table("C:/Users/enwosu/Documents/RStudio_WorkFile/ML-R-Code/RecommenderSystems/ml-100k/u.data", header=FALSE, sep="\t", col.names = c("UserID", "MovieID", "Rating", "Timestamp"))
MovieLense_data$Timestamp <- as.POSIXct(MovieLense_data$Timestamp, origin="1970-01-01")
head(MovieLense_data)

#  *------------------------------------------------------------------*
#  | STEP 3: View and examine the data 
#  *------------------------------------------------------------------*

# Just review the first 6 data rows
head(MovieLense_data)

# Just review the last 6 data rows
tail(MovieLense_data)

# Check to see if it has NA for missing values.
#is.na(wine_data)
which (is.na(MovieLense_data))

# Data Summary
summary(MovieLense_data)

# Data Structure
str(MovieLense_data)

# Data Dimensions
dim(MovieLense_data)

# Data attributes
attributes(MovieLense_data)

# Data class
class(MovieLense_data)

#  *------------------------------------------------------------------*
#  | STEP 4: Preprocessing and inspection
#  *------------------------------------------------------------------*
#Convert the MovieLense_data from a data.frame to realRatingMatrix
MovieLense_data_realRatingMatrix <- as(MovieLense_data, "realRatingMatrix")
MovieLense_data_realRatingMatrix
head(as(MovieLense_data_realRatingMatrix, "data.frame"), 100)

# The ratings are integers in the range 0-5. Let's count the occurrences of each of them. 
vectorized_realRatingMatrix <- as.vector(MovieLense_data_realRatingMatrix@data) 
unique(vectorized_realRatingMatrix) 
table_ratings <- table(vectorized_realRatingMatrix) 
table_ratings

# Let's see the distribution
vectorized_realRatingMatrix <- factor(vectorized_realRatingMatrix) 
qplot(vectorized_realRatingMatrix) + ggtitle("Distribution of the ratings with values from 0 - 5")

# We can see that a majority of the ratings = 0. A rating equal to 0 represents a missing value, so we can remove them.
vectorized_realRatingMatrix <- vectorized_realRatingMatrix[vectorized_realRatingMatrix != 0] 
vectorized_realRatingMatrix <- factor(vectorized_realRatingMatrix) 
qplot(vectorized_realRatingMatrix) + ggtitle("Distribution of the ratings with values from 1 - 5")

# Find the most viewed movies
#----------------------------
# colCounts: This is the number of non-missing values for each column
views_per_movie <- colCounts(MovieLense_data_realRatingMatrix)
table_views <- data.frame(  
  movie = names(views_per_movie),  
  views = views_per_movie  
) 
table_views <- table_views[order(table_views$views, decreasing = TRUE), ]

# Let's visualize the first six rows and build a histogram
ggplot(table_views[1:6, ], aes(x = movie, y = views)) +  geom_bar(stat="identity") + theme(axis.text.x =  element_text(angle = 45, hjust = 1)) + ggtitle("Number of views  of the top movies")


# Let's explore the average ratings per movie
#--------------------------------------------
# colMeans is the average value for each column. It automatically ignores the 0s, since they represent missing values.
# colMeans can be used to identify the top-rated movies by computing the average rating of each of them.
average_ratings <- colMeans(MovieLense_data_realRatingMatrix) 
qplot(average_ratings) + stat_bin(binwidth = 0.1) + ggtitle("Distribution of the average movie rating")


# Let's explore the average ratings by user
#------------------------------------------
# rowMeans is the average value for each row. We use it to examine the distribution of the average rating by user
average_ratings_per_user <- rowMeans(MovieLense_data_realRatingMatrix)
qplot(average_ratings_per_user) + stat_bin(binwidth = 0.1) + ggtitle("Distribution of the average rating per user")

# Let's visualize the top percentile (top 1%) of users and movies
#----------------------------------------------------------------

# By selecting hte top percentile, we are selecting the most relevant users and items. Otherwise, the plot will be difficult 
# to read due to the size of the data.

#  minimum number of movies per user. 
min_n_movies <- quantile(rowCounts(MovieLense_data_realRatingMatrix), 0.99) 
min_n_movies
# minimum number of users per movie. 
min_n_users <- quantile(colCounts(MovieLense_data_realRatingMatrix), 0.99) 
min_n_users

image(MovieLense_data_realRatingMatrix[rowCounts(MovieLense_data_realRatingMatrix) > min_n_movies, colCounts(MovieLense_data_realRatingMatrix) > min_n_users], main = "Heatmap of the top users and movies")


#  *------------------------------------------------------------------*
#  | STEP 5: Split the data into training and test data 
#  *------------------------------------------------------------------*

# The below code randomly selects 3/4 of the data to be the training set, and places the rest into the testing set. 
positions <- sample(nrow(MovieLense_data_realRatingMatrix),size=floor((nrow(MovieLense_data_realRatingMatrix)/4)*3))  
training_set_ratings<- MovieLense_data_realRatingMatrix[positions,]  
testing_set_ratings<- MovieLense_data_realRatingMatrix[-positions,]  


#  *------------------------------------------------------------------*
#  STEP 6: Train the recommender model
#  *------------------------------------------------------------------*

#  We are now  ready to build a recommender model:

# IBCF
#-----
recc_model_IBCF <- Recommender(data = training_set_ratings, method = "IBCF", parameter = list(k = 30)) 
recc_model_IBCF 


# UBCF
#-----
recc_model_UBCF <- Recommender(data = training_set_ratings, method = "UBCF") 
recc_model_UBCF

#  *------------------------------------------------------------------*
#  STEP  7: Applying the recommender model on the test set
#  *------------------------------------------------------------------*
# IBCF
#-----
# Let's recommend movies to the users in the test set.
recc_predicted_IBCF <- predict(object = recc_model_IBCF, newdata = testing_set_ratings, n = 6) 
recc_predicted_IBCF

# Recommendations for the first user
recc_predicted_IBCF@items[[1]]

# Let's define a matrix with the recommendations for each user
recc_matrix_IBCF <- sapply(recc_predicted_IBCF@items, function(x){  
  colnames(MovieLense_data_realRatingMatrix)[x] 
})

dim(recc_matrix_IBCF)
# Recommendations for the first 4 users using IBCF
recc_matrix_IBCF[, 1:4]
# Recommendations for the all users using IBCF
as(recc_predicted_IBCF, "list")


# UBCF
#-----
# Let's determine the top six recommendations for each new user
recc_predicted_UBCF <- predict(object = recc_model_UBCF,  newdata = testing_set_ratings, n = 6) 
recc_predicted_UBCF

# Recommendations for the first user
recc_predicted_UBCF@items[[1]]

# Let's define a matrix with the recommendations to the test set users
recc_matrix_UBCF <- sapply(recc_predicted_UBCF@items, function(x){   
  colnames(MovieLense_data_realRatingMatrix)[x] 
}) 

dim(recc_matrix_UBCF)

# Recommendations for the first 4 users using UBCF
recc_matrix_UBCF[, 1:4]
# Recommendations for the all users using UBCF
as(recc_predicted_UBCF, "list")


#  *------------------------------------------------------------------*
#  STEP 8: Compare the number of times each movie got recommended using 
#          the IBCF and UBCF
#  *------------------------------------------------------------------*
# We can see that the UBCF has a longer tail than the IBCF version
require(gridExtra) # useful when the two plots are not based on the same data
number_of_items_IBCF <- factor(table(recc_matrix_IBCF)) 
chart_title <- "Distribution of the number of items for IBCF"
plot1 <- qplot(number_of_items_IBCF) + ggtitle(chart_title)

number_of_items_UBCF <- factor(table(recc_matrix_UBCF)) 
chart_title <- "Distribution of the number of items for UBCF"
plot2  <- qplot(number_of_items_UBCF) + ggtitle(chart_title)
grid.arrange(plot1, plot2, ncol=2)



#  *------------------------------------------------------------------*
#  STEP 9: Call out the name of the movie instead of just the ID
#  *------------------------------------------------------------------*
# Read in the movie genre's matix (u.genre) and use it to populate the the genre portion of the movie item matrix (u.item)
MovieLense_genres <- as.data.table(read.table("C:/Users/enwosu/Documents/RStudio_WorkFile/ML-R-Code/RecommenderSystems/ml-100k/u.genre", header=FALSE, sep = '|',  quote = ""))
setnames(MovieLense_genres, c('Name', 'ID'))
knitr::kable(MovieLense_genres)

# Read in the movie lense data
MovieLense_item <- as.data.table(read.table("C:/Users/enwosu/Documents/RStudio_WorkFile/ML-R-Code/RecommenderSystems/ml-100k/u.item", header=FALSE,  sep = '|', quote = ""))
# removing the columns in which all values are NA 
MovieLense_item = MovieLense_item[, colSums(is.na(data.movies))<nrow(data.movies), with = F]
corner(MovieLense_item)

# Populate the title of the movie item matrix including the movie genre portion
setnames(MovieLense_item, c('MovieID', 'MovieName', 'ReleaseDate', 'URL', as.character(MovieLense_genres$Name)))
head(MovieLense_item)

#  *------------------------------------------------------------------*
#  STEP 10: Recommender System model evaluation (TBD)
#  *------------------------------------------------------------------*

