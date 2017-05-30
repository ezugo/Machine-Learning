##################################################################################################################################################
##################################################################################################################################################
# Study/Practice R Basics from the R Primer  						[10 points]
# Load the Wine Data Set available here as a CSV file into R and conduct 10 significant statistical operations as below, on any (combination) of 
# continuous variables (i. e., Numerical) from the dataset.  
# https://archive.ics.uci.edu/ml/datasets/Wine       
# 1.	Cumulative Frequency Distribution
# 2.	Cumulative Relative Frequency Graph
# 3.	Stem-and-Leaf Plot
# 4.	Box Plot
# 5.	Standard Deviation
# 6.	Covariance
# 7.	Correlation Coefficient
# Report your results with a brief explanation (2 sentences) of what they mean.
# Note:
#1) 1st attribute is class identifier (1-3)
#2) 13 Attributes which are 13 constituents of wine
##################################################################################################################################################
##################################################################################################################################################

##################################################################################################################################################
# Read the data in from the web and save it
##################################################################################################################################################

#wine_data = read.table("C:/Users/enwosu/Desktop/UW_Course/ML_2017/ML-310/HW1/wine_data.txt")
#wine_data
#summary(wine_data)

wine = read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", header=F) 

##################################################################################################################################################
# Add the column names to the data
##################################################################################################################################################

colnames(wine) <- c('Type', 'Alcohol', 'Malic', 'Ash', 'Alcalinity', 'Magnesium', 'Phenols', 'Flavanoids', 'Nonflavanoids','Proanthocyanins', 'Color', 'Hue', 'Dilution', 'Proline')

##################################################################################################################################################
# Print the data
##################################################################################################################################################

wine

##################################################################################################################################################
# Transform the Type into a categorical variable
##################################################################################################################################################

wine$Type <- as.factor(wine$Type)

##################################################################################################################################################
# View some infromation basic about the data
##################################################################################################################################################

dim(wine)

str(wine)

names(wine)

summary(wine)

wine$Type

class(wine$Type)

sapply(wine, class)

lapply(wine, class)

nrow(wine)

ncol(wine)

length(which(wine != "NA")) 

ncol(wine) * nrow(wine)

length(which(wine != "NA")) 

##################################################################################################################################################
# Show all data EXCLUDING the class categorical column (TYPE)
##################################################################################################################################################
# Show first 5 rows
wine[c(1:5),2:14]

# Show all rows
wine[,2:14]


##################################################################################################################################################
# Show all data EXCLUDING the class categorical column (TYPE)
##################################################################################################################################################
# 1.	Cumulative Frequency Distribution (for the following attributes = Alcohol and Proanthocyanins)
# Cumulative Frequency Distribution for the Alcohol attribute
alcohol_attribute = wine$Alcohol
range_alcohol = range(alcohol_attribute)
range_alcohol
intervals_Alcohol = seq(11, 15, by = 0.75)
intervals_Alcohol
alcohol_cut = cut(alcohol_attribute, intervals_Alcohol, right=FALSE)
alcohol_frequency = table(alcohol_cut)
alcohol_cum_freq = cumsum(alcohol_frequency)
cbind(alcohol_cum_freq)


# Cumulative Frequency Distribution for the Proanthocyanins attribute
Proanthocyanins_attribute = wine$Proanthocyanins
range_Proanthocyanins = range(Proanthocyanins_attribute)
range_Proanthocyanins
intervals_Proanthocyanins = seq(0.4, 4, by = 0.25)
intervals_Proanthocyanins
Proanthocyanins_cut = cut(Proanthocyanins_attribute, intervals_Proanthocyanins, right=FALSE)
Proanthocyanins_frequency = table(Proanthocyanins_cut)
Proanthocyanins_cum_freq = cumsum(Proanthocyanins_frequency)
cbind(Proanthocyanins_cum_freq)


# 2.	Cumulative Relative Frequency Graph  (for the following attributes = Alcohol and Proanthocyanins)
# Cumulative Relative Frequency Graph for the Alcohol attribute
alcohol_cum_rel_freq = alcohol_cum_freq/nrow(wine)
cbind(alcohol_cum_freq, alcohol_cum_rel_freq)
length(intervals_Alcohol)
length(alcohol_cum_rel_freq)
#In order to plot this out, add a starting zero element since the length of the alcohol_cum_rel_freq < length og the interval
new_alcohol_cum_rel_freq = c(0,alcohol_cum_freq)/nrow(wine)
plot(intervals_Alcohol, new_alcohol_cum_rel_freq, xlab = "Intervals", ylab = "Cumulative Relative Frequency For Alcohol", main = "Cumulative Relative Frequency Graph")
#Join the dots
lines(intervals_Alcohol,new_alcohol_cum_rel_freq)


# Cumulative Relative Frequency Graph for the Proanthocyanins attribute
Proanthocyanins_cum_rel_freq = Proanthocyanins_cum_freq/nrow(wine)
cbind(Proanthocyanins_cum_freq, Proanthocyanins_cum_rel_freq)
length(intervals_Proanthocyanins)
length(Proanthocyanins_cum_rel_freq)
#In order to plot this out, add a starting zero element since the length of the Proanthocyanins_cum_rel_freq < length og the interval
new_Proanthocyanins_cum_rel_freq = c(0,Proanthocyanins_cum_freq)/nrow(wine)
plot(intervals_Proanthocyanins, new_Proanthocyanins_cum_rel_freq, xlab = "Intervals", ylab = "Cumulative Relative Frequency For Proanthocyanins", main = "Cumulative Relative Frequency Graph")
#Join the dots
lines(intervals_Proanthocyanins,new_Proanthocyanins_cum_rel_freq)


# 3.	Stem-and-Leaf Plot (see notes)
names(wine)
stem(wine$Alcohol)
stem(wine$Malic)
stem(wine$Ash)
stem(wine$Alcalinity)
stem(wine$Magnesium)
stem(wine$Phenols)
stem(wine$Flavanoids)
stem(wine$Nonflavanoids)
stem(wine$Proanthocyanins)
stem(wine$Proanthocyanins)
stem(wine$Dilution)
stem(wine$Hue)
stem(wine$Proline)

# 4.	Box Plot
boxplot(wine[,2:14], main="All 13 attributes for the wine data")

# 5.	Standard Deviation
lapply(wine[,2:14], sd, na.rm=T)

# 6.	Covariance
cov(wine$Dilution, wine$Alcohol)
# COvariance coefficient for each combination of the variable
cov(wine[,2:14])

# 7.	Correlation Coefficient
cor(wine$Dilution, wine$Alcohol)
# Correlation coefficient for each combination of the variable
cor(wine[,2:14])

# Another method
install.packages("polycor")
library(polycor)
hetcor(wine[,2:14]) #provides info on error and p-values