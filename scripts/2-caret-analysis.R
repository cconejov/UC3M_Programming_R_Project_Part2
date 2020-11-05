## Instructions:

# 1. Use the library caret for analyzing the relationship of one of the categorical (dichotomic or not) variables that you have in your data set in terms of other variables.
# 2. Split the data set in a training set and a testing set.
# 3. Use three or four techniques included in caret.
# 4. Recommended ones: SVM and Random Forests.
# 5.Predict the data set labeled as testing set.
# 6. Show and interpret the confusion matrix.

# Load data
load("rda/bike_buyers.rda")

# libraries
library(tidyverse)
library(caret)
library(caretEnsemble)
library(skimr)
library(mice)
library(randomForest)
library(fastAdaboost)
library(earth)


set.seed(2020)

####################
# 0 str-summary 
####################

str(data)

summary(data)

# for debugging
#data <- data[1:60,]


####################
# 1 Relationship categorical 
####################

ggplot(data) +
  geom_bar(aes( x = Purchased.Bike, fill = Purchased.Bike)) +
  labs(title = "Distribution Purchased Bike") 

# Where 0 is not and 1 yes. Balanced proportions

####################
# 2 Split the data
####################

# Create the training and test datasets

# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(data$Purchased.Bike, p = 0.8, list = FALSE)

# Step 2: Create the training dataset

trainData <- data[trainRowNumbers, ]

# Step 3: Create the test dataset
testData <- data[-trainRowNumbers, ]

# Step 4: Pre-processing

## 4.1 Missing value imputation
## 4.2 One hot encoding
## 4.3 Range normalization

### a) training set

# Store y 
y <- trainData$Purchased.Bike

#### Descriptive stastistics

trainData.Skimmed <- skim(trainData)
trainData.Skimmed[,c(1:5,9:11,13,15)]
rm(trainData.Skimmed)

### Missing value imputation

#Numeric variables
missing.model <- preProcess(trainData, method = "knnImpute")
missing.model

trainData <- predict(missing.model, newdata = trainData)

anyNA(trainData)



# Factor variable mice package (Factor variable)

if(anyNA(trainData) == TRUE){
  print("Complete NAs Factor Variable")
  factor.imputation.trainData <- mice(trainData)
  trainData <- complete(factor.imputation.trainData)
  
} else {
  print("Not NAs in Factor Variable")
}

anyNA(trainData)

### One hot encoding

dummies.model <- dummyVars(Purchased.Bike ~ ., data = trainData)

trainData <- data.frame(predict(dummies.model, newdata = trainData))

##  Range normalization

range.model <- preProcess(trainData,  method = "range")
trainData <- predict(range.model, newdata = trainData)

# Append the Y variable

trainData$Purchased.Bike <- y


summary(trainData)

## Importance of variables

## Continuos

trainData.continuous <- trainData[,c(5,6,19,28,29)]


featurePlot(x = trainData.continuous[,-5], y = factor(trainData.continuous$Purchased.Bike), plot = "box",
            strip = strip.custom(par.strip.text = list(cex = 0.7)), scales = list(x = list(relation = "free"),
                                                                                  y = list(relation = "free")))

## Factors (Multiple plots)


# RFE

#Not run { Takes times

options(warn = -1)
subsets = c(1:6, 19, 24, 28)
ctrl = rfeControl(functions = rfFuncs, method = "repeatedcv", repeats = 5, verbose = FALSE)
lmProfile = rfe(x = trainData[, -29], y = trainData$Purchased.Bike, sizes = subsets,
                rfeControl = ctrl)

# Save the model
save(lmProfile, file = "rda/lmProfile.rda")

# Load model
load("rda/lmProfile.rda")

rm(lmProfile)
#}

### Training

# Algorithms: c("earth","rf","svmRadial")

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Earth

model_mars <- train(Purchased.Bike ~ .,
                    data = trainData,
                    method = "earth")

# Save the model
save(model_mars, file = "rda/model_mars.rda")

# Load model
load("rda/model_mars.rda")

## Importance or variables

plot(varImp(model_mars), main = "Variable Importance with MARS")


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Random Forest

model_rf <- train(Purchased.Bike ~ .,
                  data = trainData,
                  method = "rf",
                  tuneLength = 5)

# save model_rf
save(model_rf, file = "rda/model_rf.rda")


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SVM

model_SVM <- train(Purchased.Bike ~ .,
                  data = trainData,
                  method = "svmRadial",
                  tuneLength = 15)

# save model_SVM
save(model_SVM, file = "rda/model_SVM.rda")

######################################
# Prepare Test set and predict
######################################

# Step 1: Impute missing values
# Continuous
testData2 <-  predict(missing.model, testData)
anyNA(testData2)



if(anyNA(testData2) == TRUE){
  print("Complete NAs Factor Variable")
  factor.imputation.testData <- mice(testData2)
  testData2 <- complete(factor.imputation.testData)
  
} else {
  print("Not NAs in Factor Variable")
}

# Step 2: Create one-hot encodings (dummy variables)
testData3 <- data.frame(predict(dummies.model, testData2))

# Step 3: Transform the features to range between 0 and 1
testData4 <- predict(range.model, testData3)
rm(testData3, testData2)


str(testData4)


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Earth

predicted.model_mars <- predict(model_mars, testData4)

# Compute the confusion matrix


CM_mars <- confusionMatrix(reference = testData$Purchase, 
                           data = predicted.model_mars, 
                           mode = "everything",
                           positive = "Yes")

str(CM_mars)

CM_mars$table

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# rf

predicted.model_rf <- predict(model_rf, testData4)

# Compute the confusion matrix


CM_rf <- confusionMatrix(reference = testData$Purchase, 
                data = predicted.model_rf, 
                mode = "everything",
                positive = "Yes")


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SVMradial

predicted.model_SVM <- predict(model_SVM, testData4)

# Compute the confusion matrix


CM_SVM <- confusionMatrix(reference = testData$Purchase, 
                          data = predicted.model_SVM, 
                          mode = "everything",
                          positive = "Yes")



#+++++++++++++++++++++++++++++++++++++++++
# Caret Ensemble
#+++++++++++++++++++++++++++++++++++++++++

trainControl.models <- trainControl(method = "repeatedcv",
                                       number = 10,
                                       repeats = 3,
                                       savePredictions = TRUE,
                                       classProbs = TRUE)

#algorithmList <- c("rf","earth","svmRadial", "adaboost") #tahestime

algorithmList <- c("earth","rf","svmRadial")

models_ensemble <- caretList(Purchased.Bike ~ .,
                              data = trainData,
                              trControl = trainControl.models,
                              methodList = algorithmList)

#save models_ensemble
save(models_ensemble, file = "rda/models_ensemble.rda")

results_ensemble <- resamples(models_ensemble)
summary(results_ensemble)

str(results_ensemble)

# Box plots to compare models
scales = list(x = list(relation = "free"), y = list(relation = "free"))
bwplot(results_ensemble, scales = scales)



# Caret Emsemble

stack_glm_ensemble <- caretStack(models_ensemble, method = "glm")
save(stack_glm_ensemble, file = "rda/stack_glm_ensemble.rda")


#save stack.glm_ensemble

print(stack_glm_ensemble)


stack_predicts_ensemble <- predict(stack_glm_ensemble, newdata = testData4)


# Compute the confusion matrix
confusionMatrix(reference = testData$Purchase, 
                data = stack_predicts_ensemble, 
                mode = "everything",
                positive = "Yes")
