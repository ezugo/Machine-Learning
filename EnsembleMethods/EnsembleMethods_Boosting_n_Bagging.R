###########################################################################################################################
# Ensemble Methods: Bagging and Boosting 
#
# Apply one algorithm only  (e.g. C4.5) each of Bagging and Boosting on the mtcars dataset for Classification.  
# Explain your results.
###########################################################################################################################


###########################################################################################################################
# BAGGING
###########################################################################################################################

# Load the dataset
data(mtcars)
dataset <- mtcars
str(dataset)
# Create training and test sets
# The below code randomly selects 3/4 of the data to be the training set, and places the rest into the testing set. 

positions <- sample(nrow(dataset),size=floor((nrow(dataset)/4)*3))  
training<- dataset[positions,]  
testing<- dataset[-positions,]  

# Let's leverage the CART algorithm from the "rpart" package
library(rpart)
install.packages("rpart.plot")
library("rpart.plot")

# Next, generate a model using the training dataset
rt_mpg<-rpart(mpg ~ .,data=training)
rpart_predictions<-predict(rt_mpg, newdata=testing)

# Next, let's determine the error using 2 different methodologies for just a single RPart classification algorithm
# Mean square error
error<-sqrt((sum((testing$mpg-rpart_predictions)^2))/nrow(testing))
error

# Mean absolute error
MAE <- function(actual, predicted) {mean(abs(actual - predicted))}
MAE(testing$mpg, rpart_predictions)

# Let's see the data plot
plot(rt_mpg)
text(rt_mpg)
printcp(rt_mpg)
plotcp(rt_mpg)
rpart.plot(rt_mpg)


# Let's repeat for 5000 bags
library(foreach)
length_divisor<-6
iterations<-5000
predictions<-foreach(m=1:iterations,.combine=cbind) %do% {
  training_positions <- sample(nrow(training), size=floor((nrow(training)/length_divisor)))
  train_pos<-1:nrow(training) %in% training_positions
  rpart_fit<-rpart(mpg~.,data=training[train_pos,])
  predict(rpart_fit,newdata=testing)
}
predictions<-rowMeans(predictions)
error<-sqrt((sum((testing$mpg-predictions)^2))/nrow(testing))
error

MAE(testing$mpg, predictions)


###########################################################################################################################
# BOOSTING
###########################################################################################################################
#############################################
#Method: Boosting using the mboost package
#############################################

#install.packages("mboost")
library(mboost)

###########################################################################################################################
# Load the dataset
###########################################################################################################################
data(mtcars)
dataset <- mtcars
str(dataset)

###########################################################################################################################
# Create training and test sets
# The below code randomly selects 3/4 of the data to be the training set, and places the rest into the testing set. 
###########################################################################################################################
positions_mboost <- sample(nrow(dataset),size=floor((nrow(dataset)/4)*3))  
training_mboost<- dataset[positions_mboost,]  
testing_mboost<- dataset[-positions_mboost,]  

###########################################################################################################################
# Generate a model based on  mboost package
###########################################################################################################################
# Next, generate a model (based on boosting) using the training dataset
boost_mpg<-glmboost(mpg ~ .,data=training_mboost, center = TRUE)

#####################################################################################################################
# Generate the summary statistic
#####################################################################################################################
summary(boost_mpg)
#coef(boost_mpg)
#mstop(aic <- AIC(boost_mpg))

###########################################################################################################################
# mpg Prediction based on the  mboost package
###########################################################################################################################
boost_predictions<-predict(boost_mpg, newdata=testing_mboost)

###########################################################################################################################
# Next, let's determine the error
# Mean square error
###########################################################################################################################
error<-sqrt((sum((testing_mboost$mpg-boost_predictions)^2))/nrow(testing_mboost))
error

# Mean absolute error
MAE <- function(actual, predicted) {mean(abs(actual - predicted))}
MAE(testing_mboost$mpg, boost_predictions)