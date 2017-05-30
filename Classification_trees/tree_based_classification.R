###################################################################################################################################
# Inspect the data set given in the AnimalData.xls. For this data set, construct about 10 to 15 
# Classification Rules which classify the data into a mammal, bird, reptile, fish, and amphibian, 
# insect or invertebrate. 
###################################################################################################################################



###################################################################################################################################
# Install library for RWeka
###################################################################################################################################

#library(caret)
##install.packages("rJava",type = "source")
#install.packages("RWeka")
#iibrary(RWeka)

###################################################################################################################################
# Read in the data
###################################################################################################################################

# Read in the data
Animal_Table = read.csv("C:/Users/Desktop/UW_Course_Stuff/AnimalData.csv", header=TRUE)

# Table headers appear to be messed up. So, re-wrtie the table column header names
colnames(Animal_Table) <- c('animal name', 'hair', 'feathers', 'eggs', 'milk', 'airborne', 'aquatic', 'predator', 'toothed', 'backbone', 'breathes', 'venomous', 'fins', 'legs', 'tail', 'domestic', 'catsize', 'type')

# Just review the first 6 data rows
head(Animal_Table)


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

#####################################################################################################################
# Model
###################################################################################################################################

train <- createFolds(Animal_Table$type, k=10)

ctreeFit <- train(type ~ ., method = "ctree", data = Animal_Table[0:97, ],
                  tuneLength = 5, na.action = na.omit,
                  trControl = trainControl(
                    method = "cv", indexOut = train))

generate_rules
plot(ctreeFit$finalModel)
predict(ctreeFit, Animal_Table[98:100, ])



generate_rules <- train(type ~., method = "PART", data = Animal_Table[0:97, ], tuneLength = 5, na.action = na.omit, trControl = trainControl(method = "cv", indexOut = train))
generate_rules
