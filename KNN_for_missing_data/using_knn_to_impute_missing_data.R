###################################################################################################################################
#7.	Study the KNN Algorithm, and how it is used to do missing value imputation, here.     
# Then, Using the KNN Algorithm, estimate for the missing value (?) for the Lease Prices XLS dataset.
###################################################################################################################################

# There are 2 sets of libraries that allow Knn imputation namly VIM and DMwr. Pick one.
# For each of them, the function is called kNN i.e. ?kNN (for VIM) or ??kNN (for DMwr) for more info
# For the KNN package
install.packages("VIM")
library(VIM)

#install.packages("DMwR")
#library(DMwR)

###################################################################################################################################
# Read in data
###################################################################################################################################

# By generating the matrix in R
#Property_Table = matrix(c(45, 52, 52, 54, 55, 57, 62, 62, 10000, 13000, 11000, NA, 18000, 16000, 13000, 12000), nrow = 8, ncol = 2, byrow = FALSE)
#colnames(Property_Table) <- c('floor area [m2] ', 'lease price [$]')

# Alternatively, you can read the table from disk
Property_Table = read.csv("C:/Users/Desktop/UW_Course/ML_2017/ML-310/HW2/LeasePricesKNN.csv", header=TRUE)

# Table headers appear to be messed up. So, re-wrtie the table column header names
colnames(Property_Table) <- c('floor area [m2] ', 'lease price [$]')
# Display the matrix
Property_Table

# Convert "?" to NA since "?" is used to represent misisng data in the source data
Property_Table[Property_Table=="?"] <- NA 
# Display the matrix
Property_Table
###################################################################################################################################
# Inspect the data
###################################################################################################################################

# Number of rows and columns of the data
nrow(Property_Table)
ncol(Property_Table)

# Display Attributes
attributes(Property_Table)

# Display data class
class(Property_Table)

# Display the summary stats
summary(Property_Table)

# Display column names
names(Property_Table)

###################################################################################################################################
# Detect row with missing value
###################################################################################################################################
# Display the matrix
Property_Table

# Detect the row with NA data
manyNAs(Property_Table)

# Detect the actual record with missing value
complete.cases(Property_Table)
is.na(Property_Table)
Property_Table_Missing = which(is.na(Property_Table))
Property_Table_Missing
boxplot(Property_Table$`floor area [m2] `, Property_Table$`lease price [$]`,  names = c('floor area [m2]','lease price [$]'), main="Comparing the box plots of 'floor area [m2]' and 'lease price [$]'")

###################################################################################################################################
# Make sure that the data has the right type. Here, we need each column to be numeric
###################################################################################################################################
# Make sure the data types is as expected
str(Property_Table)
#str(as.numeric(as.character(Property_Table$`lease price [$]`)), Property_Table$`lease price [$]`)

# Check if the "floor area [m2]" attribute is numeric
is.numeric(Property_Table$`floor area [m2] `)
# Check if the "lease price [$]" attribute is numeric
is.numeric(Property_Table$`lease price [$]`)

# We see that the "lease price [$]" attribute is NOT numeric. Now convert it to numeric and recheck
# Here we convert the factor variable to a numeric variable but this is not really necesary
#Property_Table$`lease price [$]` = as.numeric(as.character(Property_Table$`lease price [$]`))

# Make sure the data types is as expected
str(Property_Table)

Property_Table

boxplot(Property_Table$`floor area [m2] `, Property_Table$`lease price [$]`, names = c('floor area [m2]','lease price [$]'), main="Comparing the box plots of 'floor area [m2]' and 'lease price [$]'")

###################################################################################################################################
# Impute missing data using KNN
###################################################################################################################################

# IF you wanted to remove the missing value
#Property_Table = Property_Table[-manyNAs(Property_Table),]
#Property_Table[4, 2]

# Do the KNN imputation to find the generate and replace the NA value in the following column
#Property_Table_1 = kNN(Property_Table, variable = c("lease price [$]"), k=2)

# Alternatively, you could have easily just used the matrix instead ofhte column
Property_Table_1 = kNN(Property_Table, variable = c("lease price [$]"), k=1)
Property_Table_1

# Try other values for K
Property_Table_1 = kNN(Property_Table, variable = c("lease price [$]"), k=2)
Property_Table_1

Property_Table_1 = kNN(Property_Table, variable = c("lease price [$]"), k=3)
Property_Table_1


Property_Table_1 = kNN(Property_Table, variable = c("lease price [$]"), k=4)
Property_Table_1


Property_Table_1 = kNN(Property_Table, variable = c("lease price [$]"), k=5)
Property_Table_1

# Let's select KNN = 4
Property_Table_1 = kNN(Property_Table, variable = c("lease price [$]"), k=4)
Property_Table_1

# Now, the KNN algorithm adds some extra columns and we want to remove it.
summary(Property_Table_1)
str(Property_Table_1)

# Let's plot this out to get a sence of the data pictorially
plot(Property_Table_1$`floor area [m2] `, Property_Table_1$`lease price [$]`, main = 'Plot of Lease Price ($) vs. Floor Area (m2) when K = 4', xlab="Floor Area [m2]", ylab="Lease Price [$]")

# Now lets drop the new extra colums added by hte KNN algorithm
Property_Table_Final = Property_Table_1[ , 1:2]
Property_Table_Final
