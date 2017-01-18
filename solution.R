# This is the solution file to the CMP3036M Data Science Assignment 1.

training <- read.csv("ds_training.csv", header=TRUE, sep=",")
View(ds_training)

intall.packages("ggplot2")
library(ggplot2)

qplot(training$var3, training$var36)

plot(training$ID, training$var15)

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