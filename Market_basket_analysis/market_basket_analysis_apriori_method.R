###################################################################################################################################
# 8. Study the Apriori algorithm data mining and how discovers items that are frequently associated 
# together, for Market Basket Analysis. http://software.ucv.ro/~cmihaescu/ro/teaching/AIR/docs/Lab8-Apriori.pdf
# page 1 -2    (small dataset from Supermarket: Transaction ID/ milk /Bread /butter /beer/ 
# Conduct Market Basket Analysis on the same market data set using R.  Compare your notes.                          
# As with HW #1, please explain the data context and results adequately.
###################################################################################################################################

###################################################################################################################################
# Install library for APriori Association rules
###################################################################################################################################
# Load the libraries
install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)
library(datasets)

###################################################################################################################################
# Read in the data
###################################################################################################################################

# Read in the data
Market_Basket = read.csv("C:/Users/Desktop/UW_Course/ML_2017/ML-310/HW2/SuperMarketData.csv", header=TRUE, sep=",")

# Table headers appear to be messed up. So, re-wrtie the table column header names
#colnames(Market_Basket) <- c('animal name', 'hair', 'feathers', 'eggs', 'milk', 'airborne', 'aquatic', 'predator', 'toothed', 'backbone', 'breathes', 'venomous', 'fins', 'legs', 'tail', 'domestic', 'catsize', 'type')

# Display the data
Market_Basket

# Data structure
str(Market_Basket)

###################################################################################################################################
# Inspect original datasetdata
###################################################################################################################################

# Number of rows and columns of the data
nrow(Market_Basket)
ncol(Market_Basket)

# Display Attributes
attributes(Market_Basket)

# Display data class
class(Market_Basket)

# Display the summary stats
summary(Market_Basket)

# Display column names
names(Market_Basket)

# Data structure
str(Market_Basket)

###################################################################################################################################
# Discretize non-factor or non-logical variables
###################################################################################################################################
# From our inspection, we can see that all the column should be discretized as a categorical variable
Market_Basket$Transaction_ID = as.factor(Market_Basket$Transaction_ID)

# Convert the 1's and 0's from the remaining attributes into True or False (i.e. logical)
Market_Basket$Milk = as.logical(Market_Basket$Milk)
Market_Basket$Bread = as.logical(Market_Basket$Bread)
Market_Basket$Butter = as.logical(Market_Basket$Butter)
Market_Basket$Beer = as.logical(Market_Basket$Beer)

# Display the data
Market_Basket

# Data structure
str(Market_Basket)

# Sumamrize the data
summary(Market_Basket)

###################################################################################################################################
# Convert the data into a Market basket transaction format
###################################################################################################################################
# Convert the data into a Market basket transaction format. We want to exclude the Transaction_ID column
Market_Basket_transaction = as(Market_Basket[2:5], 'transactions')


summary(Market_Basket_transaction)

# Display the data
Market_Basket_transaction

# Data structure
str(Market_Basket_transaction)

###################################################################################################################################
# Frequency plot of the different products bought by different customers from this store
###################################################################################################################################

# Here, topN represents the most sold number you want to see in this plot. So topN = 4 will show the top 4 most purchased products.
# This is more useful when you have a lot of products. Since we have only 4 products, let's set topN = 4

itemFrequencyPlot(Market_Basket_transaction, topN = 4)
# Even if you notice, even if we set topN = 5, we still get only these four plots since we have only 4 products.
itemFrequencyPlot(Market_Basket_transaction, topN = 5)

###################################################################################################################################
# TRAIN THE APRIORI MODEL ON THE DATASET
###################################################################################################################################

# The rules variable below will contain the different rules for our business problem.
rules = apriori(data = Market_Basket_transaction, parameter = list(support = 0.26, confidence = 0.65))

# View the rules (not sorted)
inspect(rules)

# Next, let's sort the rules by their decreasing lift.
inspect(sort(rules, by = 'lift'))


# Let's relax the confidence and evaluate the results
rules = apriori(data = Market_Basket_transaction, parameter = list(support = 0.26, confidence = 0.2))
# Next, let's sort the rules by their decreasing lift.
inspect(sort(rules, by = 'lift'))


# Let's relax the support, increase the confidence and evaluate the reslts
rules = apriori(data = Market_Basket_transaction, parameter = list(support = 0.133, confidence = 0.8))
# Next, let's sort the rules by their decreasing lift.
inspect(sort(rules, by = 'lift'))

plot(rules)

plot(rules, method = "graph", control = list(type="itemsets"), interactive=TRUE,shading=NA)

plot(rules, shading="order", control=list(main = "Two-key plot"))

sel <- plot(rules, measure=c("support", "lift"), shading="confidence", interactive=TRUE)



subrules2 <- head(sort(rules, by="lift"), 5)
plot(subrules2, method="matrix3D", measure="lift", control=list(reorder=TRUE))
plot(subrules2, method="graph")
plot(subrules2, method="graph", control=list(type="itemsets"))
plot(subrules2, method="paracoord")
plot(subrules2, method="paracoord", control=list(reorder=TRUE))

plot(rules, method="paracoord", control=list(reorder=TRUE))

plot(rules, method = NULL, measure = "support", shading = "lift", interactive = FALSE, data = NULL, control = NULL)