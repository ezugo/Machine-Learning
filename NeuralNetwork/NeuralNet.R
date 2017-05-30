
#  ------------------------------------------------------------------
#  |FILE NAME:      NeuralNet
#  |DATE:           05/14/17
#  |CREATED BY:     Ezugo Nwosu 
#  |DATA SoURCE:    http://archive.ics.uci.edu/ml/datasets/Wine     
#  |DATA FILE Name: wine.csv
#  |----------------------------------------------------------------


#  |------------------------------------------------------------------
#  |STEPS:               
#  |
#  |  STEP 1: Install and load the required packages and libraries 
#  |  STEP 2: Get the data 
#  |  STEP 3: View and examine the data 
#  |  STEP 4: Preprocessing
#  |  STEP 5: Split the data into training and test data
#  |  STEP 6: Train the network
#  |  STEP 7: Plot and visualize the converged trained network
#  |  STEP 8: Predict the values for the test set 
#  |  STEP 9: Compare predicted and actual results
#  |  STEP 10: Model predictive accuracy based on the test data 
#  |------------------------------------------------------------------


#  *------------------------------------------------------------------*
#  | STEP 1: Install and load the required packages and libraries 
#  *------------------------------------------------------------------*
install.packages("neuralnet")
library("neuralnet")
require(ggplot2)
require(nnet) # Required for one-hot encoding
#  *------------------------------------------------------------------*
#  | STEP 2: Get the data 
#  *------------------------------------------------------------------*
# First read the csv files into R as a data frame. Change this path to the file path on your computer.
wine_data = read.csv("C:/Users/Documents/RStudio_WorkFile/NeuralNet/wine.csv", header=TRUE)

# Since this is a classification exercise, let's make sure that the label has a "factor" type
str(wine_data)

# Transform the Class variable to a factor variable
wine_data$Class <- as.factor(wine_data$Class)

# Check to make sure that the Class variable is now a "factor" variable
str(wine_data)
#  *------------------------------------------------------------------*
#  | STEP 3: View and examine the data 
#  *------------------------------------------------------------------*
# Let's visualize some of the variables
alcohol_vs_magnesium <- ggplot(wine_data, aes(x = Alcohol, y = Proanthocyanins, colour = as.factor(Class))) +
  geom_point(size=3) +
  ggtitle("wine_data")
alcohol_vs_magnesium

Magnesium_vs_MalicAcid <- ggplot(wine_data, aes(x = Magnesium, y = Malic_acid, colour = as.factor(Class))) +
  geom_point(size=3) +
  ggtitle("wine_data")
Magnesium_vs_MalicAcid

Flavanoids_vs_ColorIntensity <- ggplot(wine_data, aes(x = Flavanoids, y = Color_intensity, colour = as.factor(Class))) +
  geom_point(size=3) +
  ggtitle("wine_data")
Flavanoids_vs_ColorIntensity

# Just review the first 6 data rows
head(wine_data)

# Just review the last 6 data rows
tail(wine_data)

# Check to see if it has NA for missing values.
#is.na(wine_data)
which (is.na(wine_data))

# Data Summary
summary(wine_data)

# Data Structure
str(wine_data)

# Data Dimensions
dim(wine_data)

# Data attributes
attributes(wine_data)

# Data class
class(wine_data)

#  *------------------------------------------------------------------*
#  | STEP 4: Preprocessing 
#  *------------------------------------------------------------------*
# The neural net package cannot work with factors. Let's encode the factor
# variable with one-hot encoding. Encode as a one hot vector multilabel data

# One-Hot Encoding Stage 1: Remove the Class variable
pre_processed_wine_data <- cbind(wine_data[, 2:14], class.ind(as.factor(wine_data$Class)))

# One-Hot Encoding Stage 2: Replace the Class label with one-hot encoded classes. Since we have 3 classes,
# we need 3 variables to properly encode the 3 tier class
names(pre_processed_wine_data) <- c(names(wine_data)[2:14],"l1","l2","l3")

# Now let's standardize the predictors (not the one-hot enocded class) in the [0???1] interval by leveraging 
# the lapply function Scale data
scl <- function(x){ (x - min(x))/(max(x) - min(x)) }
pre_processed_wine_data[, 1:13] <- data.frame(lapply(pre_processed_wine_data[, 1:13], scl))

#  *------------------------------------------------------------------*
#  | STEP 5: Split the data into training and test data 
#  *------------------------------------------------------------------*

# The dataset will be split up in a subset used for training the neural network and another set
# used for testing. The code below places all of our variables into a data frame, then randomly 
# selects 3/4 of the data to be the training set, and places the rest into the testing set. 
positions_wine <- sample(nrow(pre_processed_wine_data),size=floor((nrow(pre_processed_wine_data)/4)*3))  
training_wine<- pre_processed_wine_data[positions_wine,]  
testing_wine<- pre_processed_wine_data[-positions_wine,]  

#  *------------------------------------------------------------------*
#  STEP 6: Train the network
#  *------------------------------------------------------------------*

# For some reason the neural net formula y~. is not accepted in the neuralnet() function. You need to 
# first write the formula and then pass it as an argument in the fitting function.
#
# The hidden argument accepts a vector with the number of neurons for each hidden layer, while the 
# argument linear.output is used to specify whether we want to do regression linear.output=TRUE or 
# classification linear.output=FALSE

# Set up formula
feature_training_wine <- names(training_wine)
modelFormula <- as.formula(paste("l1 + l2 + l3 ~", paste(feature_training_wine[!feature_training_wine %in% c("l1","l2","l3")], collapse = " + ")))

# Configuration of neurons: 13 (input layer):5(1st hidden layer):3(second hidden layer):1(output layer)
# 
# From the above, The input layer has 13 inputs, the two hidden layers have 5 and 3 neurons and the output 
# layer has, of course, a single output since we are doing regression.
#
# Setting the Linear.output = FALSE allows me to select the activation function. I chose the defualt (logistic)
neural_net_wine <- neuralnet(modelFormula, data = training_wine, act.fct = "logistic", hidden=c(5,3), linear.output=F)

neural_net_wine$result.matrix

#  *------------------------------------------------------------------*
#  STEP 7: Plot and visualize the converged trained network
#  *------------------------------------------------------------------*
# The results of the training process can be visualized by two different plots. 
# 
# First, the trained neural network topology can simply be plotted. The black 
# lines show the connections between each layer and the weights on each connection 
# while the blue lines show the bias term added in each step.
plot(neural_net_wine)

# The second possibility to visualize the results is to plot generalized weights. gwplot uses the calculated 
# generalized weights provided by neural_net_wine$generalized.weights
par(mfrow=c(4,4)) 

gwplot(neural_net_wine,selected.covariate="Alcohol", min=-20, max=50) 
gwplot(neural_net_wine,selected.covariate="Malic_acid", min=-20, max=50) 
gwplot(neural_net_wine,selected.covariate="Ash", min=-20, max=50) 
gwplot(neural_net_wine,selected.covariate="Alcalinity_of_ash", min=-20, max=50) 
gwplot(neural_net_wine,selected.covariate="Magnesium", min=-20, max=50) 
gwplot(neural_net_wine,selected.covariate="Total_phenols", min=-20, max=50) 
gwplot(neural_net_wine,selected.covariate="Flavanoids", min=-20, max=50) 
gwplot(neural_net_wine,selected.covariate="Nonflavanoid_phenols", min=-20, max=50) 
gwplot(neural_net_wine,selected.covariate="Proanthocyanins", min=-20, max=50) 
gwplot(neural_net_wine,selected.covariate="Color_intensity", min=-20, max=50) 
gwplot(neural_net_wine,selected.covariate="Hue", min=-20, max=50) 
gwplot(neural_net_wine,selected.covariate="OD280.OD315_of_diluted_wines", min=-20, max=50) 
gwplot(neural_net_wine,selected.covariate="Proline", min=-20, max=50) 

# Reset the mfrow parameter
par(mfrow=c(1,1))

#  *------------------------------------------------------------------*
#  STEP  8: Predict the values for the test set 
#  *------------------------------------------------------------------*
# You need to call the compute function with the output of the trained neural network
# and the test data set (without the dependent variable you aim to predict)

# Make the predictions for the test data
neural_net_predictions <- compute(neural_net_wine, testing_wine[,1:13])


#  *------------------------------------------------------------------*
#  STEP 9: Compare predicted and actual results
#  *------------------------------------------------------------------*

# Compare the predictions to the actualy one-hot encoded classes
results <- data.frame(actual_class = testing_wine[,14:16], predicted_class = round(neural_net_predictions$net.result))
results

#  *------------------------------------------------------------------*
#  STEP 10: Model predictive accuracy based on the test data 
#  *------------------------------------------------------------------*

# # Transform the one-hot actual test data classes to non one-hot encoded classes by selecting the max columns  
actual_test_data_classes <- max.col(testing_wine[, 14:16])

# Transform the one-hot predicted test data classes to non one-hot encoded classes by selecting the max columns 
predicted_test_data_classes <- max.col(neural_net_predictions$net.result)

# Compare the actual and predicted test data classes 
prediction_vs_actual_test_data = data.frame(actual_class = predicted_test_data_classes, predicted_class = predicted_test_data_classes)
prediction_vs_actual_test_data

# Calculate the accuracy of the actual and predicted test data classes
accuracy_test_data_prediction = mean(actual_test_data_classes == predicted_test_data_classes)
accuracy_test_data_prediction




