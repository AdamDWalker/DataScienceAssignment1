# This is the solution file to the CMP3036M Data Science Assignment 1.

  # Load in the training dataset
    training <- read.csv("ds_training.csv", header=TRUE, sep=",")
    View(training)
  
  # Install R packages for doing stuff
    
    install.packages("ggplot2")
    install.packages("FSelector")
    install.packages("caret")
    install.packages("pROC")
    install.packages("caTools")
    install.packages("klaR")
    
    library(ggplot2)
    library(FSelector)
    library(caret)
    library(pROC)
    library(caTools)
    library(klaR)
    
    target <- training$TARGET
    
    summary(training)
    
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
  #training[,"TARGET"] <- target
    
    training$TARGET <- ifelse(target==1, "One", "Zero")
    ToSplit <- createDataPartition(training$TARGET, p = .7, list = FALSE)
    trainSet <- training[ToSplit,]
    testSet <- training[-ToSplit,]
    
    summary(training)
  
  # Set seed for use of same data for models
  
    set.seed(1)

  # GBM model
  
    GBMControl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)
  
    GBMFit <- train(as.factor(TARGET)~., data = trainSet, method = "gbm", metric = "ROC", trControl = GBMControl)
  
    GBMPredict <- predict(GBMFit, testSet, type = "prob")
    auc(testSet$TARGET, GBMPredict[,1])
    plot(GBMPredict, main = "GBM Predictor")

  # GLM Model
    
    GLMControl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)
    
    GLMFit <- train(as.factor(TARGET)~., data = trainSet, method = "glm", metric = "ROC", trControl = GLMControl)
    
    GLMPredict <- predict(GLMFit, testSet, type = "prob")
    
    auc(testSet$TARGET, GLMPredict[,1])
    plot(GLMPredict, main = "GLM Predictor")
    
  # Boosted Logistic Regression Model
    
    BLRControl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)
    
    BLRFit <- train(as.factor(TARGET)~., data = trainSet, method = "LogitBoost", metric = "ROC", trControl = BLRControl)
    
    BLRPredict <- predict(BLRFit, testSet, type = "prob")
    auc(testSet$TARGET, BLRPredict[,1])
    plot(BLRPredict, main = "BLR Predictor")
    
  # Naive Bayes Model
    
    NBControl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)
    NBFit <- train(as.factor(TARGET)~., data = trainSet, method = "nb", metric = "ROC", trControl = NBControl)
    
    NBPredict <- predict(NBFit, testSet, type = "prob")
    
    auc(testSet$TARGET, NBPredict[,1])
    plot(NBPredict, main = "Naive Bayes Predictor")
    
  # ROC Line Curves
    
    GBMLine <- roc(testSet$TARGET, GBMPredict[,1])
    GLMLine <- roc(testSet$TARGET, GLMPredict[,1])
    BLRLine <- roc(testSet$TARGET, BLRPredict[,1])
    NBLine <- roc(testSet$TARGET, NBPredict[,1])
    
  # Plot ROC Curves
    
    plot(GBMLine, col = "Red")
    plot(GLMLine, col = "Blue", add = TRUE)
    plot(BLRLine, col = "Green", add = TRUE)
    plot(NBLine, col = "Yellow", add = TRUE)
    
  # Apply to test data
    ds_test <- read.csv("~/GitHub/datascienceassignment1/ds_test.csv")
    View(ds_test)
    
    finalDataControl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)
    finalDataFit <- train(as.factor(TARGET)~., data = trainSet, method = "gbm", metric = "ROC", trControl = finalDataControl)
    
    finalDataPredict <- predict(finalDataFit, ds_test, type = "prob")
    
    output <- data.frame(ds_test$ID, round(finalDataPredict[,1]))
    colnames(output) <- c("ID", "TARGET")
    write.csv(output, file = "ds_submission_11357886.csv", row.names = FALSE)

# save target, remove target and ID - need ID for final thing
# Calculate weights - information gain for useful data
# Readd target to weighted data (17 variables)
# split 70/30 for testing and training
# make model
# 


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