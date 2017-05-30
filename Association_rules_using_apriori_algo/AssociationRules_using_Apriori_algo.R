###################################################################################################################################
#   	For the same data set, construct about 10 to 15 Association Rules which associate the key features with the animal type 
#     or other features together.    
###################################################################################################################################

###################################################################################################################################
# Install library for APriori Association rules
###################################################################################################################################

#install.packages("arules")
#install.packages("Matrix")
library(arules)
library(arulesViz)
library(datasets)

###################################################################################################################################
# Read in the data
###################################################################################################################################

# Read in the data
Animal_Table = read.csv("C:/Users/Desktop/UW_Course/ML_2017/ML-310/HW2/AnimalDataNew.csv", header=TRUE)

# Table headers appear to be messed up. So, re-wrtie the table column header names
colnames(Animal_Table) <- c('animal name', 'hair', 'feathers', 'eggs', 'milk', 'airborne', 'aquatic', 'predator', 'toothed', 'backbone', 'breathes', 'venomous', 'fins', 'legs', 'tail', 'domestic', 'catsize', 'type')

# Just review the first 6 data rows
head(Animal_Table)

# Display the matrix
# Animal_Table


###################################################################################################################################
# View the actual record with missing value
###################################################################################################################################
Animal_Table[98:101, ]

# Check to see if it has NA for missing values. This should return a false for the "type" attribute since contains "??" and not "NA"
is.na(Animal_Table[98:101, ])

###################################################################################################################################
# Replace all missing variables with "NA"
###################################################################################################################################
# Convert "?" to NA since "?" is used to represent misisng data in the source data
Animal_Table[Animal_Table=="??"] <- NA 

# Check again to see if it has NA for missing values. The "type" attribute with missing values should now contain "NA" and not just "??"
is.na(Animal_Table[98:101, ])
Animal_Table[98:101, ]

###################################################################################################################################
# Inspect data
###################################################################################################################################

# Number of rows and columns of the data
nrow(Animal_Table)
ncol(Animal_Table)

# Display Attributes
attributes(Animal_Table)

# Display data class
class(Animal_Table)

# Display the summary stats. Here, we can see that the "type" variable has 4 NA values
summary(Animal_Table)

# Display column names
names(Animal_Table)

# Data structure
str(Animal_Table)

###################################################################################################################################
# Discretize non-factor or non-logical variables
###################################################################################################################################
# From our inspection, we can see that the "Legs" column should be discretized since the number of legs can be viewed as 
# a categorical variable. So, change nominal variables into factors (in some cases, but not here, you will also have to 
# convert binary variables into logical)
Animal_Table$legs = as.factor(Animal_Table$legs)

#See the levels for the "legs" variable for the Animal_Table data set
levels(Animal_Table$legs)

# Data structure. Now, we see that the "legs" variable has been converted from and int to a Factor variable
str(Animal_Table)

###################################################################################################################################
# Convert to transactions
###################################################################################################################################
Animal_Table_transaction = as(Animal_Table, 'transactions')

Animal_Table_transaction

summary(Animal_Table_transaction)

itemFrequencyPlot(Animal_Table_transaction, topN=50,  cex.names=.5)


###################################################################################################################################
# Run the Apriori function from the arules package on our data file
###################################################################################################################################

Animal_Table_Association_Rules = apriori(data = Animal_Table_transaction, parameter=list(supp=0.1,conf = 0.08))
#inspect(Animal_Table_Association_Rules)

Animal_Table_Association_Rules_Sorted = sort(Animal_Table_Association_Rules, by="lift")

#inspect(Animal_Table_Association_Rules_Sorted)

inspect(head(Animal_Table_Association_Rules_Sorted))

# Show the top 15 rules with a supp = 0.1 and conf = 0.08
inspect(Animal_Table_Association_Rules_Sorted[1:15])