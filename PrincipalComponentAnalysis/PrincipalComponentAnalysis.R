###########################################################################################################################
# Principal Components Analysis  
# Study the Principal Components Analysis tutorial in R here:  http://www.r-bloggers.com/computing-and-visualizing-pca-in-r/ 
# Then conduct PCA on the modified States.XX data set.  Explain what PCA is in the context of this data set, what it does, 
# and interpret your results.  Why is it useful?
###########################################################################################################################

###################################################################################################################################
# Install and call the appropriate library packages
###################################################################################################################################

install.packages("MASS")
install.packages("rJava",type = "source")
install.packages("RWeka")
install.packages("ggplot2")
install.packages("devtools")
library(devtools)
#install_github("vqv/ggbiplot", force = TRUE)
install_github("ggbiplot", "vqv")
library(ggbiplot)
library(caret)
library(RWeka)
library(ggplot2)
library(MASS)
library("rJava")

###################################################################################################################################
# Read in the data
###################################################################################################################################

# Read in the data
States_Data = read.csv("C:/Users/Desktop/UW_Course/ML_2017/ML-310/HW3/modifiedStates.csv", header=TRUE)

# Table headers appear to be messed up. So, re-wrtie the table column header names
colnames(States_Data) <- c('State', 'Population',	'Income',	'Illiteracy',	'Life_Exp',	'Murder', 'HS_Grad',	'Frost', 'Area', 'State_Initial', 'Region', 'SATV', 'SATM', 'Percent', 'Dollars', 'Pay')


###################################################################################################################################
# View and examine the data 
###################################################################################################################################

# Just review the first 6 data rows
head(States_Data)

# Just review the last 6 data rows
tail(States_Data)

# Retrieve the names of the columns
names(States_Data)

# Check to see if it has NA for missing values.
is.na(States_Data)

# Data Summary
summary(States_Data)

# Data Structure
str(States_Data)

# Number of rows and columns
ncol(States_Data)
nrow(States_Data)

# Data Dimensions
dim(States_Data)

# Data attributes
attributes(States_Data)

# Data class
class(States_Data)


# From the analysis above, the data consists of 16 variables (3 categorical, 8 int and 5 numerical variables)

# From inpection, we see that the Area Code variable (Area) is currently stored as an int. Let's change this to a factor varibble
States_Data$Area = as.factor(States_Data$Area)
str(States_Data)
###################################################################################################################################
# METHOD 1: PCA by hand
###################################################################################################################################


# We will apply PCA to the ontly the continuous variables (num and int) and use the categorical variable to visualize the PCs later.

States_Data_Continous_Variables_Only = States_Data[ ,c('Population',	'Income',	'Illiteracy',	'Life_Exp',	'Murder', 'HS_Grad',	'Frost','SATV', 'SATM', 'Percent', 'Dollars', 'Pay')]

# Find the covariance matrix 
States_Data_Covariance = cov(States_Data_Continous_Variables_Only)

# View the cov matrix
head(States_Data_Covariance)

# Find the Eigen vector and corresponding eigen value
States_Data_EigenValues = eigen(States_Data_Covariance)$values
States_Data_EigenVectors = eigen(States_Data_Covariance)$vectors

States_Data_Principal_Component = as.matrix(States_Data_Continous_Variables_Only) %*% States_Data_EigenVectors
head(States_Data_Principal_Component)

# To double check the results, lets compute the covariance matrix of the Principle Component
# The variances of cov(PC) = EigenValues
# cov(PC) = 0 (except for rounding errors) since hte Principal Components have to be uncorrelated
cov(States_Data_Principal_Component)

# Let's do this for the first three Eigenvalues
EigenValue_By_Hand = States_Data_EigenValues[1:3]
cov(States_Data_Principal_Component)[1:3,1:3]

###################################################################################################################################
# Let's calculate the proportions of the variation explained by the various components
###################################################################################################################################

print(round(States_Data_EigenValues/sum(States_Data_EigenValues) * 100, digits = 2))

round(cumsum(States_Data_EigenValues)/sum(States_Data_EigenValues) * 100, digits = 2)


###################################################################################################################################
# METHOD 2: PCA using PrComp
###################################################################################################################################

States_Data_PrComp = prcomp(States_Data_Continous_Variables_Only,
                            center = TRUE,
                            scale. = TRUE)

States_Data_PrComp

# Lets extract the variances of the components by doing the following:

States_Data_PrComp_var = States_Data_PrComp$sdev^2
EigenValue_By_PrComp = States_Data_PrComp_var[1:3]


###################################################################################################################################
# METHOD 3: PCA using Box-Cox transformation 
# It is possible to first apply a Box-Cox transformation to correct for skewness, center and scale each variable and then apply 
# PCA in one call to the preProcess function of the caret package.
###################################################################################################################################

trans = preProcess(States_Data_Continous_Variables_Only, 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))
States_Data_BoxCox = predict(trans, States_Data_Continous_Variables_Only)

# Retained PCs (first 9 rows)
head(States_Data_BoxCox, 9)

# Loadings
trans$rotation

# By default, the function keeps only the PCs that are necessary to explain at least 95% of the variability in the data, but 
# this can be changed through the argument thresh.
###################################################################################################################################
# Let's compare the EigenValues calculated by the Hand and by using the PrComp function
###################################################################################################################################
EigenValue_By_Hand
EigenValue_By_PrComp

# Let's analyze the results
# print method
print(States_Data_PrComp)

###################################################################################################################################
# Analyze and Visualize the principal components (use the results from method 3 i.e. based on hte prcomp method)
###################################################################################################################################

# The summary method describe the importance of the PCs.
summary(States_Data_PrComp)
# The first row describe again the standard deviation associated with each PC. The second row shows the proportion of the variance 
# in the data explained by each component while the third row describe the cumulative proportion of explained variance. We can see 
# there that the first two PCs accounts for more than  of the variance of the data.


# Proportions of hte variances explained by hte components of the states data
plot(States_Data_PrComp)

# A plot of the variances (y-axis) associated with the PCs (x-axis) 
plot(States_Data_PrComp, type = "l")
# The Figure below is useful to decide how many PCs to retain for further analysis. We can see that the first 7 PCs explain most of hte variability in the data

# A biplot generated by the function biplot.
biplot(States_Data_PrComp)

# A biplot generated by the function ggbiplot of the ggbiplot package available on github.

# I used the factor varible "Region" to display hte first 2 principle components

g <- ggbiplot(States_Data_PrComp, obs.scale = 1, var.scale = 1, groups = States_Data$Region, ellipse = TRUE, circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g)

# Let's repeat with the plot States factor variable. Since there is only one observation for each of your factors 
# in States_Data$States, we need to remove the ellipses

display_based_on_states_factor_variable <- ggbiplot(States_Data_PrComp, obs.scale = 1, var.scale = 1, groups = States_Data$States, circle = TRUE)
display_based_on_states_factor_variable <- display_based_on_states_factor_variable + scale_color_discrete(name = '')
display_based_on_states_factor_variableg <- display_based_on_states_factor_variable + theme(legend.direction = 'horizontal', legend.position = 'top')
print(display_based_on_states_factor_variable)

# Let's repeat with the plot State_Initial factor variable. Since there is only one observation for each of your factors 
# in States_Data$States, we need to remove the ellipses

display_based_on_stateInitial_factor_variable <- ggbiplot(States_Data_PrComp, obs.scale = 1, var.scale = 1, groups = States_Data$State_Initial, circle = TRUE)
display_based_on_stateInitial_factor_variable <- display_based_on_stateInitial_factor_variable + scale_color_discrete(name = '')
display_based_on_stateInitial_factor_variable <- display_based_on_stateInitial_factor_variable + theme(legend.direction = 'horizontal', legend.position = 'top')
print(display_based_on_stateInitial_factor_variable)


# Plot each variables coefficients inside a unit circle to get insight on a possible interpretation for PCs
#require(ggplot2)
theta <- seq(0,2*pi,length.out = 100)
circle <- data.frame(x = cos(theta), y = sin(theta))
p <- ggplot(circle,aes(x,y)) + geom_path()
loadings <- data.frame(States_Data_PrComp$rotation, .names = row.names(States_Data_PrComp$rotation))
p + geom_text(data=loadings, mapping=aes(x = PC1, y = PC2, label = .names, colour = .names)) + coord_fixed(ratio=1) + labs(x = "PC1", y = "PC2")



