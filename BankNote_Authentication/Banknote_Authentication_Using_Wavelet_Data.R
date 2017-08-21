
#  ------------------------------------------------------------------
#  |FILE NAME:      Banknote_Authentication
#  |DATE:           08/05/17
#  |CREATED BY:     Ezugo Nwosu 
#  |DATA SoURCE:    https://archive.ics.uci.edu/ml/machine-learning-databases/00267/  
#  |DATA FILE Name: data_banknote_authentication.txt
#  |----------------------------------------------------------------

#  ------------------------------------------------------------------
#  |Data Set Information:
#  |=====================
#  |Data were extracted from images that were taken from genuine and 
#  |forged banknote-like specimens. For digitization, an industrial  
#  |camera usually used for print inspection was used. The final 
#  |images have 400x 400 pixels. Due to the object lens and distance 
#  |to the investigated object gray-scale pictures with a resolution  
#  |of about 660 dpi were gained. Wavelet Transform tools were used 
#  |to extract features from images.
#  | 
#  | So essentially, the following was done:
#  | (1) Images were taken of 1372 banknotes, some counterfeit and some genuine. 
#  | (2) Wavelet tranformation tools were used to extract the following descriptive 
#  |     features of the images: Variance, Skewness, Kurtosis, Entropy. 
#  | (3) We also have the true label for whether or not a banknote is genuine 
#  |     (Yes = 1, No = 0). 
#  |----------------------------------------------------------------

#  ------------------------------------------------------------------
#  |Attribute Information:
#  |1: Variance of Wavelet Transformed image (continuous) 
#  |2: Skewness of Wavelet Transformed image (continuous) 
#  |3: Curtosis of Wavelet Transformed image (continuous)  
#  |4: Entropy of image (continuous)
#  |5: Class (integer) 
#  |----------------------------------------------------------------

#  |------------------------------------------------------------------
#  |STEPS:               
#  |
#  |  STEP 1:  Install and load the required packages and libraries 
#  |  STEP 2:  Get the data 
#  |  STEP 3:  View and examine the data (Exploratory Data Analysis)
#  |  STEP 4:  Preprocessing and data inspection
#  |  STEP 5:  Split the data into training and test data
#  |  STEP 6:  Train the neural net model (NN)
#  |  STEP 7:  Plot and visualize the converged trained network
#  |  STEP 8:  Predict the values for the test set 
#  |  STEP 9:  Convert predictions to a class value of to 0's and 1's
#  |  STEP 10: Compare predicted and actual results (confusion matrix)
#  |  STEP 11: Plot the ROC curve for this classification
#  |  STEP 12: Calculate the AUC metric
#  |  STEP 13: Repeat steps 6, 8 and 10 - 12 using Random Forests (RF)
#  |  STEP 14: Repeat steps 6, 8 and 10 - 12 using Support Vector Machines (SVM)
#  |  STEP 15: Compare AUC and Mean for NN, RF and SVM models
#  |------------------------------------------------------------------

#  |------------------------------------------------------------------
#  |OTHER REFERENCES:               
#  |
#  |  1:  https://en.wikipedia.org/wiki/Wavelet_transform
#  |  2:  http://web.iitd.ac.in/~sumeet/WaveletTutorial.pdf
#  |  3:  http://inside.mines.edu/~whoff/courses/EENG510/lectures/24-Wavelets.pdf
#  |  4:  http://bme2.aut.ac.ir/mhmoradi/wavelet/Reference%20Book/In%20sight%20into%20wavelets%20from%20theory%20to%20practice%20,%20Soman%20K.P.%20,Ramachandran%20K.I.%20,%20Ch.1-Ch.9.pdf
#  |  5:  http://www-scf.usc.edu/~hbalan/wavelets.pdf
#  |  6:  https://sundoc.bibliothek.uni-halle.de/diss-online/02/03H033/t4.pdf
#  |  7:  http://www.stat.cmu.edu/~rnugent/PCMI2016/code.html#banknote
#  |------------------------------------------------------------------

#  *------------------------------------------------------------------*
#  | STEP 1: Install and load the required packages and libraries 
#  *------------------------------------------------------------------*
unavailable_packages <- setdiff(c("randomForest", "caTools", "ggplot2","neuralnet", "mlbench", "e1071", "pbkrtest", "Hmisc", "lattice", "gridExtra", "grid"), rownames(installed.packages()))
if (length(unavailable_packages)>0){
  install.packages(unavailable_packages)
}

##load the libraries
library(caTools)
library(ggplot2)
library(neuralnet)
library(randomForest)
library("e1071")
library("mlbench")
library("pbkrtest")
library("Hmisc")
library("lattice")
# For multiple ggplots
library(gridExtra)
library(grid)
#  *------------------------------------------------------------------*
#  | STEP 2: Get the data 
#  *------------------------------------------------------------------*
# First read the file into R as a data frame.
Bank_Data = read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00267/data_banknote_authentication.txt", stringsAsFactors = F)

# Table headers appear to be messed up. So, re-wrtie the table column header names
# The class column indicates whether or not the Bank Note is authentic.
colnames(Bank_Data) <- c('Variance_of_Wavelet_Transformed_image', 'Skewness_of_Wavelet_Transformed_image', 'Curtosis_of_Wavelet_Transformed_image', 'Entropy_of_image', 'Class')
head(Bank_Data,20)

#  *------------------------------------------------------------------*
#  | STEP 3: View and examine the data (Exploratory Data Analysis)
#  *------------------------------------------------------------------*

# First, look att he class distribution
hist(Bank_Data$Class, prob = T, main='Histogram of image entropy')
lines(density(Bank_Data$Class,na.rm=T))
rug(jitter(Bank_Data$Class))
qqPlot(Bank_Data$Class,main='Normal QQ plot of image Class')


# Next, let's plot some histograms of the dataset. 
par(mfrow=c(2,2)) 
hist(Bank_Data$Variance_of_Wavelet_Transformed_image, prob = T, main='Histogram of image variance')
lines(density(Bank_Data$Variance_of_Wavelet_Transformed_image,na.rm=T))
rug(jitter(Bank_Data$Variance_of_Wavelet_Transformed_image))
qqPlot(Bank_Data$Variance_of_Wavelet_Transformed_image,main='Normal QQ plot of image variance')


hist(Bank_Data$Skewness_of_Wavelet_Transformed_image, prob = T, main='Histogram of image skewness')
lines(density(Bank_Data$Skewness_of_Wavelet_Transformed_image,na.rm=T))
rug(jitter(Bank_Data$Skewness_of_Wavelet_Transformed_image))
qqPlot(Bank_Data$Skewness_of_Wavelet_Transformed_image,main='Normal QQ plot of image skewness')


hist(Bank_Data$Curtosis_of_Wavelet_Transformed_image, prob = T, main='Histogram of image curtosis')
lines(density(Bank_Data$Curtosis_of_Wavelet_Transformed_image,na.rm=T))
rug(jitter(Bank_Data$Curtosis_of_Wavelet_Transformed_image))
qqPlot(Bank_Data$Curtosis_of_Wavelet_Transformed_image,main='Normal QQ plot of image curtosis')


hist(Bank_Data$Entropy_of_image, prob = T, main='Histogram of image entropy')
lines(density(Bank_Data$Entropy_of_image,na.rm=T))
rug(jitter(Bank_Data$Entropy_of_image))
qqPlot(Bank_Data$Entropy_of_image,main='Normal QQ plot of image entropy')


par(mfrow=c(1,1)) 

# Next, lets construct an enriched box plot different variables variable.

par(mfrow=c(2,2)) 

boxplot(Bank_Data$Variance_of_Wavelet_Transformed_image, ylab = "Image Variance") 
rug(jitter(Bank_Data$Variance_of_Wavelet_Transformed_image), side = 2) 
abline(h = mean(Bank_Data$Variance_of_Wavelet_Transformed_image, na.rm = T), lty = 2)

boxplot(Bank_Data$Skewness_of_Wavelet_Transformed_image, ylab = "Image Skewness") 
rug(jitter(Bank_Data$Skewness_of_Wavelet_Transformed_image), side = 2) 
abline(h = mean(Bank_Data$Skewness_of_Wavelet_Transformed_image, na.rm = T), lty = 2)

boxplot(Bank_Data$Curtosis_of_Wavelet_Transformed_image, ylab = "Image Curosis") 
rug(jitter(Bank_Data$Curtosis_of_Wavelet_Transformed_image), side = 2) 
abline(h = mean(Bank_Data$Curtosis_of_Wavelet_Transformed_image, na.rm = T), lty = 2)


boxplot(Bank_Data$Entropy_of_image, ylab = "Image Entropy") 
rug(jitter(Bank_Data$Entropy_of_image), side = 2) 
abline(h = mean(Bank_Data$Entropy_of_image, na.rm = T), lty = 2)

par(mfrow=c(1,1)) 


# Next, let's visualize some of the variables based on their class distribution
# For some reason, par(mfrow=c(n,m)) does not work for ggplot, so we will use "grid.arrange" below

variance_vs_curtosis <- ggplot(Bank_Data, aes(x = Variance_of_Wavelet_Transformed_image, y = Curtosis_of_Wavelet_Transformed_image, colour = as.factor(Class))) + geom_point(size=3) + ggtitle("variance_vs_curtosis") + xlab("Variance") + ylab("Curtosis")
variance_vs_Skewness <- ggplot(Bank_Data, aes(x = Variance_of_Wavelet_Transformed_image, y = Skewness_of_Wavelet_Transformed_image, colour = as.factor(Class))) + geom_point(size=3) + ggtitle("variance_vs_Skewness") + xlab("Variance") + ylab("Skewness")
variance_vs_Entropy  <- ggplot(Bank_Data, aes(x = Variance_of_Wavelet_Transformed_image, y = Entropy_of_image, colour = as.factor(Class))) + geom_point(size=3) + ggtitle("variance_vs_Entropy") + xlab("Variance") + ylab("entropy")
curtosis_vs_Skewness <- ggplot(Bank_Data, aes(x = Curtosis_of_Wavelet_Transformed_image, y = Skewness_of_Wavelet_Transformed_image, colour = as.factor(Class))) + geom_point(size=3) + ggtitle("curtosis_vs_Skewness") + xlab("Curtosis") + ylab("Skewness")
curtosis_vs_Entropy  <- ggplot(Bank_Data, aes(x = Curtosis_of_Wavelet_Transformed_image, y = Entropy_of_image, colour = as.factor(Class))) + geom_point(size=3) + ggtitle("curtosis_vs_Entropy") + xlab("Variance") + ylab("Entropy")
skewness_vs_Entropy  <- ggplot(Bank_Data, aes(x = Skewness_of_Wavelet_Transformed_image, y = Entropy_of_image, colour = as.factor(Class))) + geom_point(size=3) + ggtitle("skewness_vs_Entropy") + xlab("Variance") + ylab("Entropy")
grid.arrange(variance_vs_curtosis,variance_vs_Skewness,variance_vs_Entropy,curtosis_vs_Skewness,curtosis_vs_Entropy,skewness_vs_Entropy, ncol=1, top=textGrob("Multiple Plots", gp=gpar(fontsize=12, font = 2)))


# Next, let's take a look at some conditioned plots (graphical representations that 
# depend on certain factors) using a box-percentile plots

#factorized_Class = equal.count(na.omit(Bank_Data$Class), number=4,overlap=1/5)

#stripplot(Bank_Data$Entropy_of_image ~ Bank_Data$Skewness_of_Wavelet_Transformed_image|factorized_Class,data=Bank_Data[!is.na(Bank_Data$Class),])


# Examine the some data rows and statistics
head(Bank_Data)
str(Bank_Data)
attributes(Bank_Data)
class(Bank_Data)
summary(Bank_Data)
dim(Bank_Data)
tail(Bank_Data)

#  *------------------------------------------------------------------*
#  | STEP 4: Preprocessing and inspection
#  *------------------------------------------------------------------*
# The wavelet transform is considered pre-processing on the raw image signal. 
# Wavelet transform is capable of providing the time and frequency information 
# simultaneously, hence giving a time-frequency representation of the signal. 

# Wavelets represent the scale of features in an image, as well as their position.
#
#
#

# Every transformation technique has its own area of application, with 
# advantages and disadvantages, and the wavelet transform (WT) is no
# exception. 
#  *------------------------------------------------------------------*
#  | STEP 5: Split the data into training and test data 
#  *------------------------------------------------------------------*

# The below code randomly selects 3/4 of the data to be the training set, and places the rest into the testing set. 
positions <- sample(nrow(Bank_Data),size=floor((nrow(Bank_Data)/4)*3))  
training_data<- Bank_Data[positions,]  
testing_data<- Bank_Data[-positions,]  

#  *------------------------------------------------------------------*
#  STEP 6: Train the neural net model
#  *------------------------------------------------------------------*
# For some reason the neural net formula y~. is not accepted in the neuralnet() function.
neural_net_bank_note <-neuralnet(Class ~ Variance_of_Wavelet_Transformed_image + Skewness_of_Wavelet_Transformed_image + Curtosis_of_Wavelet_Transformed_image + Entropy_of_image, data = training_data, act.fct = "logistic", hidden = c(5,3), linear.output = FALSE)

neural_net_bank_note$result.matrix

#  *------------------------------------------------------------------*
#  STEP 7: Plot and visualize the converged trained network
#  *------------------------------------------------------------------*
# The results of the training process can be visualized by two different plots. 
# 
# First, the trained neural network topology can simply be plotted. The black 
# lines show the connections between each layer and the weights on each connection 
# while the blue lines show the bias term added in each step.
plot(neural_net_bank_note)

# The second possibility to visualize the results is to plot generalized weights. gwplot uses the calculated 
# generalized weights provided by neural_net_bank_note$generalized.weights

par(mfrow=c(2,2)) 
gwplot(neural_net_bank_note,selected.covariate="Variance_of_Wavelet_Transformed_image", min=-20, max=50) 
gwplot(neural_net_bank_note,selected.covariate="Skewness_of_Wavelet_Transformed_image", min=-20, max=50) 
gwplot(neural_net_bank_note,selected.covariate="Curtosis_of_Wavelet_Transformed_image", min=-20, max=50) 
gwplot(neural_net_bank_note,selected.covariate="Entropy_of_image", min=-20, max=50) 

# Reset the mfrow parameter
par(mfrow=c(1,1))


#  *------------------------------------------------------------------*
#  STEP  8: Predict the values for the test set 
#  *------------------------------------------------------------------*

neural_net_predictions <- compute(neural_net_bank_note, testing_data[1:4])

#  *------------------------------------------------------------------*
#  STEP  9: Convert predictions to a class value of to 0's and 1's
#  *------------------------------------------------------------------*
# By viewing a sample of the predictions, we can see that they are still probabilities.
# So instead of a "0", we get something like "0.00004236".
head(neural_net_predictions$net.result)

# Let's apply the "round" function to convert to 0's and 1's
predicted_classes_neural_net <- sapply(neural_net_predictions$net.result, round)

# Now check the results again. We sould have 0's and 1's
head(predicted_classes_neural_net)

#  *------------------------------------------------------------------*
#  STEP  10: Let's examine the confusion matrix 
#  *------------------------------------------------------------------*
table(predicted_classes_neural_net, testing_data$Class)

########################################################################################################
## STEP 11: Plot the ROC curve for this classification
########################################################################################################
roc = function(pred, dat){
  # Sort the dat list in descending order
  dat_order = order(pred,decreasing=T)
  dat = dat[dat_order]
  false_positive_rate=cumsum(!dat)/sum(!dat)
  true_positive_rate=cumsum(dat)/sum(dat)
  data.frame(false_positive_rate,true_positive_rate)
}


roc_neural_net = plot(roc(predicted_classes_neural_net,testing_data$Class), main = "NN ROC")


########################################################################################################
## STEP 12: Calculate the AUC metric
########################################################################################################

auc = function(pred, dat){
  roc_calc = roc(pred,dat)
  delta_x = c(diff(roc_calc$false_positive_rate),0)
  delta_y = c(diff(roc_calc$true_positive_rate),0)
  sum(roc_calc$true_positive_rate * delta_x) - sum(delta_x * delta_y/2)
}
auc_neural_net = auc(predicted_classes_neural_net,testing_data$Class)
auc_neural_net

# Calculate the mean error
mean_neural_net = mean(predicted_classes_neural_net == testing_data$Class)

########################################################################################################
## STEP 13: Repeat steps 6, 8 and 10 - 12 using Random Forests (RF)
########################################################################################################
# Set the "Class" attribute as a "factor" variable for the trainnig dataset (not the test dataset)
# Before
str(training_data)
str(testing_data)
# You need to convert the response variable(target) to factor if you want to perform classification.
training_data$Class <- factor(training_data$Class)
#testing_data$Class <- factor(testing_data$Class)
# After
str(training_data)
str(testing_data)

# Train the random forest model
random_forest_bank_note <- randomForest(Class ~ ., data = training_data)
# Predict the values for the test set (random forest)
random_forest_predictions <- predict(random_forest_bank_note, testing_data)

# No need to apply the round functions since we get 0's and 1's

# Next, let's examine the confusion matrix (random forest)
table(random_forest_predictions, testing_data$Class)

# Plot the ROC curve for this classification (random forest)
roc_random_forest <- plot(roc(random_forest_predictions,testing_data$Class), main = "RF ROC")

# Calculate the AUC metric (random forest)
auc_random_forest <- auc(random_forest_predictions,testing_data$Class)
auc_random_forest

# Calculate the mean error
mean_random_forest <- mean(random_forest_predictions == testing_data$Class)

########################################################################################################
## STEP 14: Repeat steps 6, 8 and 10 - 12 using Support Vector Machines (SVM)
########################################################################################################
# Remember, the "Class" attribute was set as a "factor" variable for the trainnig dataset  (not the test dataset)
str(training_data)
str(testing_data)

# Train the random forest model
# SVM MOdel. 100 is arbitrary selected as the cost value. Later, we would tune the svm model to find the best cost value
svm_bank_note <- svm(Class ~ ., data=training_data, type='C-classification', kernel = "linear", cost = 100, scale = FALSE)

# Let's see the output
print(svm_bank_note)

# A summary of the svm model
summary(svm_bank_note)

# Tuning to find the best values for the cost and gamma data points
tuned_svm_bank_note <- tune(svm, Class ~ ., data=training_data, type='C-classification', kernel = "linear", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

summary(tuned_svm_bank_note)

# SVM model after tuning
svm_model_bank_note_after_tuning <- svm(Class ~ ., data=training_data, type='C-classification', kernel = "linear", cost = 1, gamma = 0.5, scale = FALSE)

# Let's see the output
print(svm_model_bank_note_after_tuning)

# A summary of the svm model
summary(svm_model_bank_note_after_tuning)


# Predict the values for the test set (random forest)
svm_prediction <- predict(svm_model_bank_note_after_tuning, testing_data, type = "class")

print(svm_prediction)

plot(svm_prediction)

# No need to apply the round functions since we get 0's and 1's

# Next, let's examine the confusion matrix (random forest)
table(svm_prediction, testing_data$Class)

# Plot the ROC curve for this classification (random forest)
roc_SVM <- plot(roc(svm_prediction,testing_data$Class), main = "SVM ROC")

# Calculate the AUC metric (random forest)
auc_svm <- auc(svm_prediction,testing_data$Class)
auc_svm

# Calculate the mean error
mean_svm <- mean(svm_prediction == testing_data$Class)
########################################################################################################
## STEP 15: Compare AUC and Mean for NN, RF and NN models
########################################################################################################
NN_vs_RF_SVM_AUC <- data.frame(NN_AUC = auc_neural_net, RF_AUC = auc_random_forest, SVM_AUC = auc_svm)
NN_vs_RF_SVM_AUC

NN_vs_RF_SVM_MEAN <- data.frame(NN_MEAN = mean_neural_net, RF_MEAN = mean_random_forest, SVM_MEAN = mean_svm)
NN_vs_RF_SVM_MEAN
