# This is the solution file to the CMP3036M Data Science Assignment 1.

training <- read.csv("ds_training.csv", header=TRUE, sep=",")
View(training)

install.packages("ggplot2")
install.packages("FSelector")
install.packages("caret")

library(ggplot2)
library(FSelector)
library(caret)

target <- training$TARGET

qplot(training$TARGET, training$var15)

zeroData = c()

for(col in 1:NCOL(training))
{
  sum <- sum(as.numeric(training[,col]))
  if(sum == 0)
    zeroData <- c(zeroData, col)
}

training <- subset(training, select = -c(zeroData))


# Determine data weighting
weights <- information.gain(TARGET~., data = training)
weights <- subset(weights, weights!=0)
training <- subset(training, select = rownames(weights))


# Create Test and Training sets of data
training[,"TARGET"] <- target
ToSplit <- createDataPartition(training$TARGET, p = .7, list = FALSE)
trainSet <- training[ToSplit,]
testSet <- training[-ToSplit,]

summary(training)

# save target, remove target and ID - need ID for final thing
# Calculate weights - information gain for useful data
# Readd target to weighted data (17 variables)
# split 70/30 for testing and training
# make model
# 

#auc(trainData$TARGET,pred)

#====// DATA PREP NOTES //====#

#     Data Exploring
#         How many observations/variables
#           371 Columns/Variable Types & 38010 Observations/Rows
  
#         Variable Categories?
#           Integer and Numeric categrories
#         Extreme/Missing values?
#           Yes almost all of the fuckers

#         DISTRIBUTIONS???
#         Present findings - Tables/Plots

#     Data Pre-Processing
#         Clean the data set - Extreme/Missing values
#         Standardise/Normalise required? Why?

# MODEL TRAINING
#     Candidate Models?
#     Why?
#     Why split into training and testing sets
#     What method of splitting?
#     Are all variables required? Why?

# MODEL EVALUATION
#     How is a model chosen?
#     How can the AUC result be improved?
#     Other pre-processing methods that can improve the AUC results?