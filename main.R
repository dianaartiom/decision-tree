rm(list=ls())

library("caret")
library(rpart)
library(rpart.plot)

# Sets working directory
setwd("/Users/vitiok/University/IS/decision-tree")

nrOfFolds <- 5

# read data from csv file
data <- read.csv(file="churn.csv", header=TRUE, sep=",")

# remove the Phone column
data[["Phone"]] <- NULL

partitions <- createMultiFolds(y=data[, 1], k=nrOfFolds, 1)

# create a vector with all indexes of data
indexesOfInitialData <- c(1:length(data[, 1]))

# Find variables needed for creating linear model
factors <- c(names(data));

# This variable holds the column name we want to create model upon on 
attributeToWorkWith <- "Churn";

# Remove strength column... To avoid worning with lm() 
factors <- factors[factors != attributeToWorkWith];

formula <- as.formula(paste("Churn ~", paste(factors, collapse="+")));
print("========================= First =============================")
for (fold in partitions) {
  # find indexes of test data for the first data frame taken from createMultiFolds()
  testDataIndexes <- indexesOfInitialData[is.na(pmatch(indexesOfInitialData, fold))]
  
  # Extract train data from the indexes 
  trainData <- data[fold, ];
  
  # Extract test data from the indexes
  testData <- data[testDataIndexes, ];
  
  # Create decision tree model from the formula and data given as data.frame
  model <- rpart(formula, trainData);
  
  # Predict data based on the build linear model
  prediction <- predict(model, testData, type="class");
  
  # Calculate the % of hits over all the partitions
  sum = sum(prediction == testData[["Churn"]])
  print(sum / length(testData[["Churn"]]))
}


print("========================= Second =============================")
minsplitVas = c(1, 50, 100);
for(j in minsplitVas) {
  print(j)
  # sprintf("For j = %s", j )
  for (fold in partitions) {
    # find indexes of test data for the first data frame taken from createMultiFolds()
    testDataIndexes <- indexesOfInitialData[is.na(pmatch(indexesOfInitialData, fold))]
    
    # Extract train data from the indexes 
    trainData <- data[fold, ];
    
    # Extract test data from the indexes
    testData <- data[testDataIndexes, ];
    
    # Create decision tree model from the formula and data given as data.frame
    model <- rpart(formula, trainData, minsplit=j);
    
    # Predict data based on the build linear model
    prediction <- predict(model, testData, type="class");
    
    # Calculate the % of hits over all the partitions
    sum = sum(prediction == testData[["Churn"]])
    print(sum/666)
  }
}

print("========================= Third =============================")
model <- rpart(formula, data);
prp(model)
rpart.plot(model)
