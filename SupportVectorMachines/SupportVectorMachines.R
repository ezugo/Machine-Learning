###########################################################################################################################
# Support Vector Machines		
# Study the SVM  R coding/tutorials at the below links:
# (a) http://www.svm-tutorial.com/2014/10/support-vector-regression-r/
# (b) http://www.r-bloggers.com/learning-kernels-svm/
# Then, use the below data set and apply SVM using R libraries to classify (SEPARATELY) (i) Flowers and (ii) Mushrooms 
# using the Iris and Mushroom data sets. 
# (a) https://archive.ics.uci.edu/ml/datasets/Iris
# (b) https://archive.ics.uci.edu/ml/datasets/Mushroom 
###########################################################################################################################
################################
#Question 2 - Part 1: Iris Data#
################################
###################################################################################################################################
# Install and call the appropriate library packages
###################################################################################################################################
install.packages("e1071")
install.packages("mlbench")
install.packages("pbkrtest", dependencies = T)
install.packages("caret", dependencies = T)
install.packages("colorspace", dependencies = T)
install.packages("ggplot2", dependencies = T)
install.packages("AppliedPredictiveModeling", dependencies = T)

library("e1071")
library("mlbench")
library(caret)
library(AppliedPredictiveModeling)
###################################################################################################################################
# Read in the data
###################################################################################################################################

# Read in the data
Iris_Data = read.csv("C:/Users/Desktop/UW_Course/ML_2017/ML-310/HW3/iris.csv", header=TRUE)

###################################################################################################################################
# Plot out the data to gain some understanding about relevant features and feature relationships
###################################################################################################################################

# Overview plot of the data
plot(Iris_Data)

# Now, let's compare the different classes for each features
featurePlot(x = Iris_Data[, 1:4], y = Iris_Data$Species, plot = "box", 
            ## Pass in options to bwplot() 
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)),  
            layout = c(4,1 ), 
            auto.key = list(columns = 2))

# Let's see a color coded plot for the sepal width vs. the sepal length for the different species
plot(Iris_Data$sepal_length_in_cm, Iris_Data$sepal_width_in_cm, col = Iris_Data$Species)

# Let's see a color coded plot for the petal width vs. the sepal length for the different species
plot(Iris_Data$sepal_length_in_cm, Iris_Data$petal_width_in_cm, col = Iris_Data$Species)

# Let's see a color coded plot for the sepal width vs. the petal length for the different species
plot(Iris_Data$petal_length_in_cm, Iris_Data$sepal_width_in_cm, col = Iris_Data$Species)

# Let's see a color coded plot for the petal width vs. the petal length for the different species
plot(Iris_Data$petal_length_in_cm, Iris_Data$petal_width_in_cm, col = Iris_Data$Species)


###################################################################################################################################
# View and examine the data 
###################################################################################################################################

# Just review the first 6 data rows
head(Iris_Data)

# Just review the last 6 data rows
tail(Iris_Data)

# Retrieve the names of the columns
names(Iris_Data)

# Check to see if it has NA for missing values.
is.na(Iris_Data)

# Data Summary
summary(Iris_Data)

# Data Structure
str(Iris_Data)

# Number of rows and columns
ncol(Iris_Data)
nrow(Iris_Data)

# Data Dimensions
dim(Iris_Data)

# Data attributes
attributes(Iris_Data)

# Data class
class(Iris_Data)


## From the inspection of the data, we see the following:
#### Number of Instances: 150 (50 in each of three classes)
##### Number of Attributes: 4 numeric, predictive attributes and the class
##### Attribute Information:
###### sepal length in cm
###### sepal width in cm
###### petal length in cm
###### petal width in cm
###### class: 
########  Iris Setosa
########  Iris Versicolour
########  Iris Virginica

###################################################################################################################################
# Create training and test sets
###################################################################################################################################
# The below code randomly selects 3/4 of the data to be the training set, and places the rest into the testing set. 
positions <- sample(nrow(Iris_Data),size=floor((nrow(Iris_Data)/4)*3))  
training_set_iris<- Iris_Data[positions,]  
testing_set_iris<- Iris_Data[-positions,]  

###################################################################################################################################
# Train the SVM model using the training set
###################################################################################################################################

# SVM MOdel. 100 is arbitrary selected as the cost value. Later, we would tune the svm model to find the best cost value
svm_model_iris <- svm(Species ~ ., data=training_set_iris, type='C-classification', kernel = "linear", cost = 100, scale = FALSE)

# Let's see the output
print(svm_model_iris)

# A summary of the svm model
summary(svm_model_iris)

###################################################################################################################################
# Tuning to find the best values for the cost and gamma data points
###################################################################################################################################

tuned_iris <- tune(svm, Species ~ ., data=training_set_iris, type='C-classification', kernel = "linear", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

summary(tuned_iris)

###################################################################################################################################
# SVM model after tuning
###################################################################################################################################

# New SVM MOdel. 
svm_model_iris_after_tuning <- svm(Species ~ ., data=training_set_iris, type='C-classification', kernel = "linear", cost = 1, gamma = 0.5, scale = FALSE)

# Let's see the output
print(svm_model_iris_after_tuning)

# A summary of the svm model
summary(svm_model_iris_after_tuning)


###################################################################################################################################
# Predictive Analysis based on the testing set
###################################################################################################################################

svm_prediction_iris <- predict(svm_model_iris_after_tuning,testing_set_iris, type = "class")

print(svm_prediction_iris)

plot(svm_prediction_iris)

###################################################################################################################################
# Confusion Matrix
###################################################################################################################################

table(svm_prediction_iris, testing_set_iris$Species)

mean(svm_prediction_iris == testing_set_iris$Species)


#################################################
#Repeat for a different dataset - Mushroom Data#
#################################################
###################################################################################################################################
# Install and call the appropriate library packages
###################################################################################################################################
library("e1071")

###################################################################################################################################
# Read in the data
###################################################################################################################################

# Read in the data
Mushroom_Data = read.csv("C:/Users/Desktop/UW_Course/ML_2017/ML-310/HW3/mushroom.csv", header=TRUE)

colnames(Mushroom_Data) <- c('Classes', 'cap_shape', 'cap_surface', 'cap_color', 'bruises', 'odor', 'gill_attachment', 'gill_spacing', 'gill_size', 'gill_color', 'stalk_shape', 'stalk_root', 'stalk_surface_above_ring', 'stalk_surface_below_ring', 'stalk_color_above_ring', 'stalk_color_below_ring', 'veil_type', 'veil_color', 'ring_number', 'ring_type', 'spore_print_color', 'population', 'habitat')

###################################################################################################################################
# View and examine the data 
###################################################################################################################################

# Just review the first 6 data rows
head(Mushroom_Data)

# Just review the last 6 data rows
tail(Mushroom_Data)

# Retrieve the names of the columns
names(Mushroom_Data)

# Check to see if it has NA for missing values.
is.na(Mushroom_Data)

# Data Summary
summary(Mushroom_Data)

# Data Structure
str(Mushroom_Data)

# Number of rows and columns
ncol(Mushroom_Data)
nrow(Mushroom_Data)

# Data Dimensions
dim(Mushroom_Data)

# Data attributes
attributes(Mushroom_Data)

# Data class
class(Mushroom_Data)

###################################################################################################################################
# Some more pre-processing to eliminate constant variables
###################################################################################################################################
# From the str() command, it appears that one of the predictors "veil_type" has only one factor level and hence is a constant.
# So let's drop that "veil_type"

Mushroom_Data_Without_veil_type = subset(Mushroom_Data, select=-veil_type)

# Data Structure
str(Mushroom_Data_Without_veil_type)

###################################################################################################################################
# Create training and test sets
###################################################################################################################################
# The below code randomly selects 3/4 of the data to be the training set, and places the rest into the testing set. 
positions <- sample(nrow(Mushroom_Data_Without_veil_type),size=floor((nrow(Mushroom_Data_Without_veil_type)/4)*3))  
training_set_mushroom<- Mushroom_Data_Without_veil_type[positions,]  
testing_set_mushroom<- Mushroom_Data_Without_veil_type[-positions,]  


###################################################################################################################################
# Train the SVM model using the training set
###################################################################################################################################

# SVM MOdel
svm_model_mushroom <- svm(Classes ~ ., data=training_set_mushroom, type='C-classification', kernel = "linear", cost = 100, scale = FALSE)

# Let's see the output
print(svm_model_mushroom)

# A summary of the svm model
summary(svm_model_mushroom)

###################################################################################################################################
# Tuning to find the best values for the cost and gamma data points
###################################################################################################################################

tuned_mushroom <- tune(svm, Classes ~ ., data=training_set_mushroom, type='C-classification', kernel = "linear", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

summary(tuned_mushroom)

###################################################################################################################################
# SVM model after tuning
###################################################################################################################################

# New SVM MOdel. 
svm_model_mushroom_after_tuning <- svm(Classes ~ ., data=training_set_mushroom, type='C-classification', kernel = "linear", cost = 1, gamma = 0.5, scale = FALSE)

# Let's see the output
print(svm_model_mushroom_after_tuning)

# A summary of the svm model
summary(svm_model_mushroom_after_tuning)

###################################################################################################################################
# Predictive Analysis based on the testing set
###################################################################################################################################

svm_prediction_mushroom <- predict(svm_model_mushroom_after_tuning,testing_set_mushroom, type = "class")

print(svm_prediction_mushroom)

plot(svm_prediction_mushroom)

###################################################################################################################################
# Confusion Matrix
###################################################################################################################################

table(svm_prediction_mushroom, testing_set_mushroom$Classes)

mean(svm_prediction_mushroom == testing_set_mushroom$Classes)