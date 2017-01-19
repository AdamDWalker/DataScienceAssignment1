# This is the solution file to the CMP3036M Data Science Assignment 1.

training <- read.csv("ds_training.csv", header=TRUE, sep=",")
View(training)

install.packages("ggplot2")
library(ggplot2)

qplot(training$TARGET, training$var15)

testCol = c()
for(row in 1:NROW(training))
{
  if(training$imp_op_var39_comer_ult1[row] == 0) testCol <- c(testCol, row)
}

trainSet2 <- training[-testCol,]

zeroData = c()

for(col in 1:NCOL(trainSet2))
{
  sum <- sum(as.numeric(trainSet2[,col]))
  if(sum == 0)
    zeroData <- c(zeroData, col)
}

trainSet3 <- subset(trainSet2, select = -c(zeroData))

summary(trainSet3)


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