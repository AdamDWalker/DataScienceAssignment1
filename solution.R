# This is the solution file to the CMP3036M Data Science Assignment 1.

training <- read.csv("ds_training.csv", header=TRUE, sep=",")
View(training)

install.packages("ggplot2")
library(ggplot2)
install.packages("FSelector")
library(FSelector)

qplot(training$TARGET, training$var15)

zeroData = c()

for(col in 1:NCOL(trainSet2))
{
  sum <- sum(as.numeric(trainSet2[,col]))
  if(sum == 0)
    zeroData <- c(zeroData, col)
}

trainSet3 <- subset(trainSet2, select = -c(zeroData))

information.gain(TARGET~., data = training)

emptyAttribVals = c()
for(row in 1:NROW())

summary(trainSet3)

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