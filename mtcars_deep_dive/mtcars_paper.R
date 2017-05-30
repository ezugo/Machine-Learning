##########################################################################################################################
# Data Exploration and familiarization
##########################################################################################################################
# Load the mtcars data set
data(mtcars)

# Inspect the different attributes
names(mtcars)

# Inspect attibutes
attributes(mtcars)

# Number of rows and columns of the data
nrow(mtcars)
ncol(mtcars)

# Is the dataset a dataframe?
class(mtcars)

# Inspect all observations
mtcars

##########################################################################################################################
# First, let's see if there are any missing values in the dataset. The is.na() command will return TRUE for any missing values.
##########################################################################################################################
is.na(mtcars)

##########################################################################################################################
# Another idea is to examine the statistical properties of the data can be obtained through a summary of its descriptive statistics:
##########################################################################################################################
summary(mtcars)

##########################################################################################################################
# Next, let's display the structure of the mtcars object to confirm that we only have numeric values and no categorical variable. 
##########################################################################################################################
str(mtcars)

##########################################################################################################################
# Next, lets plot the histogram of the "mpg" and "am"  variable to graphical observe the distribution
##########################################################################################################################
par(mfrow=c(1,2))
hist(mtcars$mpg, prob = T)
hist(mtcars$am, prob = T)

##########################################################################################################################
# Next, lets plot the histogram of the "mpg" and "am"  variable to graphical observe the distribution
##########################################################################################################################
library(car)
par(mfrow=c(1,2))
hist(mtcars$mpg, prob=T, xlab='', main='Histogram of maximum mpg value',ylim=0:1)
lines(density(mtcars$mpg,na.rm=T))
rug(jitter(mtcars$mpg))
qqPlot(mtcars$mpg,main='Normal QQ plot of maximum mpg')
par(mfrow=c(1,1))


# Let's repeat for other variables that have outliers
par(mfrow=c(1,6))

hist(mtcars$hp, prob=T, xlab='', main='Histogram of maximum hp value',ylim=0:1)
lines(density(mtcars$hp,na.rm=T))
rug(jitter(mtcars$hp))
qqPlot(mtcars$hp,main='Normal QQ plot of maximum hp')

hist(mtcars$wt, prob=T, xlab='', main='Histogram of maximum wt value',ylim=0:1)
lines(density(mtcars$wt,na.rm=T))
rug(jitter(mtcars$wt))
qqPlot(mtcars$wt,main='Normal QQ plot of maximum wt')

hist(mtcars$qsec, prob=T, xlab='', main='Histogram of maximum qsec value',ylim=0:1)
lines(density(mtcars$qsec,na.rm=T))
rug(jitter(mtcars$qsec))
qqPlot(mtcars$qsec,main='Normal QQ plot of maximum qsec')

par(mfrow=c(1,1))

##########################################################################################################################
# Next, lets construct an enriched box plot of the "mpg" variable.
##########################################################################################################################
boxplot(mtcars$mpg, ylab = "Miles per gallon (mpg)") 
rug(jitter(mtcars$mpg), side = 2) 
abline(h = mean(mtcars$mpg, na.rm = T), lty = 2)


# Let's repeat for other variables that have outliers
par(mfrow=c(1,3))
boxplot(mtcars$hp, ylab = "Gross horsepower (hp)") 
rug(jitter(mtcars$hp), side = 2) 
abline(h = mean(mtcars$hp, na.rm = T), lty = 2)

boxplot(mtcars$wt, ylab = "Weight (1000 lbs) (wt)") 
rug(jitter(mtcars$wt), side = 2) 
abline(h = mean(mtcars$wt, na.rm = T), lty = 2)

boxplot(mtcars$qsec, ylab = "1/4 mile time (qsec)") 
rug(jitter(mtcars$qsec), side = 2) 
abline(h = mean(mtcars$qsec, na.rm = T), lty = 2)

par(mfrow=c(1,1))

##########################################################################################################################
# Let's take a look at some conditioned plots (graphical representations that depend on certain factors)
##########################################################################################################################
install.packages("lattice")
library(lattice)
bwplot(mtcars$am ~ mtcars$mpg, data=mtcars, ylab='Trasmission (am)',xlab='Miles per gallon (mpg)')

# Since "am" is a categorical variable, let's transform it to a factor data.
mtcars$am = as.factor(mtcars$am)

mtcars$am

# From the help documentation i.e. help(mtcars) or ?mtcars, we can see that "am" represents Transmission 
# with the following levels (0 = automatic, 1 = manual)
levels(mtcars$am) = c("Automatic", "Manual")

# Now, lets re-plot the box plot
bwplot(mtcars$am ~ mtcars$mpg, data=mtcars, outpch = 19, ylab='Trasmission (am)',xlab='Miles per gallon (mpg)', main="mpg vs transmission type")


##########################################################################################################################
# Let's take a look at some conditioned plots (graphical representations that depend on certain factors) using a 
# box-percentile plots
##########################################################################################################################
install.packages("Hmisc")
library(Hmisc)
bwplot(mtcars$am ~ mtcars$mpg, data=mtcars,panel=panel.bpplot, probs=seq(.01,.49,by=.01), datadensity=TRUE, ylab='Trasmission (am)',xlab='Miles per gallon (mpg)')

##########################################################################################################################
# This type of conditioned plot can also be done for continuous variables. Let's take a look at "mpg" conditioned by
# "qsec" and "am"
##########################################################################################################################

factorized_mpg = equal.count(na.omit(mtcars$mpg), number=4,overlap=1/5)

stripplot(mtcars$am ~ qsec|factorized_mpg,data=mtcars[!is.na(mtcars$mpg),])

          
##########################################################################################################################
# Next, since we plan to investigate the relationship between "mpg" and "am", why don't we see the correlation 
# between "mpg" and each of the other variables including "am"
##########################################################################################################################
cor(mtcars$mpg,mtcars[,-1])
##########################################################################################################################
# Next, since we plan to investigate the relationship between "mpg" and "am", why don't we see the covariance
# between "mpg" and each of the other variables including "am"
##########################################################################################################################
cov(mtcars$mpg,mtcars[,-1])

##########################################################################################################################
# MULTIPLE LINEAR REGRESSION - LM
##########################################################################################################################
data(mtcars)
lm_mpg_1 = lm(mpg ~ .,data=mtcars)
summary(lm_mpg_1)
final_lm = step(lm_mpg_1)
summary(final_lm)

##########################################################################################################################
# MULTIPLE LINEAR REGRESSION - GLM
##########################################################################################################################
data(mtcars)
glm_mpg_1 = glm(formula=mpg ~ .,family=binomial, data=mtcars)
summary(glm_mpg_1)
final_glm = step(glm_mpg_1)
summary(final_glm)

##########################################################################################################################
# REGRESSION TREES
##########################################################################################################################

install.packages("rpart")
library(rpart)
install.packages('abind')
install.packages('zoo')
install.packages('xts')
install.packages('quantmod')
install.packages('ROCR')
install.packages("DMwR")
library("DMwR")

data(mtcars)
rt_mpg = rpart(mpg ~ .,data=mtcars)
rt_mpg

# Print out hte tree
prettyTree(rt_mpg)

# provide some results about the tree
summary(rt_mpg)

#Prune the tree
printcp(rt_mpg)

# Alternatively, we can use the rpartXSE() function that comes with the DMwR package (Togo). 
rt_mpg_new = rpartXse(mpg ~ .,data=mtcars)
rt_mpg_new
##########################################################################################################################
# MODEL EVALUATION
##########################################################################################################################

# Get predictions
lm_predictions_mpg <- predict(final_lm, mtcars) 
rt_predictions_mpg <- predict(rt_mpg_new, mtcars)

# Get MAE and MSE and NMSE
# MAE
(mae.a1.lm = mean(abs(lm_predictions_mpg - mtcars[, "mpg"])))

(mae.a1.rt = mean(abs(rt_predictions_mpg - mtcars[, "mpg"])))

#MSE
(mse.a1.lm = mean((lm_predictions_mpg - mtcars[, "mpg"])^2))

(mse.a1.rt = mean((rt_predictions_mpg - mtcars[, "mpg"])^2))

#NMSE
(nmse.a1.lm <- mean((lm_predictions_mpg - mtcars[, "mpg"])^2)/ + mean((mean(mtcars[, "mpg"])-mtcars[, "mpg"])^2))

(nmse.a1.rt <- mean((rt_predictions_mpg - mtcars[, "mpg"])^2)/ + mean((mean(mtcars[, "mpg"])-mtcars[, "mpg"])^2))

#	Alternatively, we can use the regr.eval() function from the Togo books package to calculate a set of regression evaluation metrics.
regr.eval(mtcars[, "mpg"], lm_predictions_mpg, train.y = mtcars[, "mpg"])

regr.eval(mtcars[, "mpg"], rt_predictions_mpg, train.y = mtcars[, "mpg"])


# Let's use a scatter plot to visuallly inspect the predictions of the model

old.par = par(mfrow = c(1, 2)) 
plot(lm_predictions_mpg, mtcars[, "mpg"], main = "Linear Model", xlab = "Predictions", ylab = "True Values") 
abline(0, 1, lty = 2) 
plot(rt_predictions_mpg, mtcars[, "mpg"], main = "Regression Tree", xlab = "Predictions", ylab = "True Values") 
abline(0, 1, lty = 2) 
par(old.par)

# So far, we have validated the model with data that the model has used fortraining. This will lead to overfitting. So, lets resolve this using k-folds
cv.rpart <- function(form,train,test,...) {
  m <- rpartXse(form,train,...)
  p <- predict(m,test)
  mse <- mean((p-resp(form,test))^2)
  c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2))
}
cv.lm <- function(form,train,test,...) {
  m <- lm(form,train,...)
  p <- predict(m,test)
  p <- ifelse(p < 0,0,p)
  mse <- mean((p-resp(form,test))^2)
  c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2))
}

# The variants() function generates a set of alternative models resulting from all possible combinations of the parameters values. 
# In this example call, we are using the"cv.lm"only with Data Mining with R: Learning with Case Studies its default parameters, and 
# for the "cv.rpart" we are specifying di???erent alternative values for the parameter se. This means that the experiment includes three 
# variants of regression trees
res <- experimentalComparison(c(dataset(mpg ~ .,mtcars,'mpg')), c(variants('cv.lm'), variants('cv.rpart',se=c(0,0.5,1))),   cvSettings(3,10,1234))


summary(res)


plot(res)

# Get the parameter settings corresponding to each variant

getVariant("cv.rpart.v1", res)
getVariant("cv.rpart.v2", res)
getVariant("cv.rpart.v3", res)

