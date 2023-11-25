library(MASS)
library(class)
library(ggplot2)

# Problem 1
weeklyData <- read.csv("Weekly.csv")
str(weeklyData)
summary(weeklyData)

weeklyData <- na.omit(weeklyData)

numericalVars <- weeklyData[, c("Lag1", "Lag2", "Lag3", "Lag4", "Lag5", "Volume", "Today")]

# Boxplot of variables
boxplot(numericalVars[, -9], main = "Boxplot of Weekly Data")

# Histogram of variables
par(mfrow = c(2, 3))  # 2 rows, 3 columns for multiple plots
for (i in 2:7) {
  hist(weeklyData[, i], main = colnames(weeklyData)[i], xlab = "")
}
par(mfrow = c(1, 1))  # Reset to a single plot

# Mark Direction as categorical data for logistic regression
weeklyData$Direction <- factor(weeklyData$Direction, levels = c("Down", "Up"))


# Perform logistic regression
logisticModel <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = weeklyData, family = "binomial")

# Print the summary
summary(logisticModel)

# Predicted probabilities
predictedProbabilities <- predict(logisticModel, type = "response")
predictedClasses <- ifelse(predictedProbabilities > 0.5, "Up", "Down")

# Create confusion matrix
confMatrix <- table(Actual = weeklyData$Direction, Predicted = predictedClasses)

# Display the confusion matrix
confMatrix

# Calculate overall fraction of correct predictions
accuracy <- sum(diag(confMatrix)) / sum(confMatrix)
print(paste("Accuracy:", accuracy))

# Create training and testing datasets
trainingData <- subset(weeklyData, Year < 2009)
testingData <- subset(weeklyData, Year >= 2009)

# Perform logistic regression on training data
logisticModel <- glm(Direction ~ Lag2, data = trainingData, family = "binomial")

# Make predictions on testing data
predictions <- predict(logisticModel, newdata = testingData, type = "response")

# Convert probabilities to binary predictions (Up/Down)
predictedDirection <- ifelse(predictions > 0.5, "Up", "Down")

# Create confusion matrix
confMatrix <- table(Actual = testingData$Direction, Predicted = predictedDirection)

# Print confusion matrix
confMatrix

calculateAccuracy <- function(confMatrix) {
  return(sum(diag(confMatrix)) / sum(confMatrix))
}

# Calculate overall fraction of correct predictions
accuracy <- sum(diag(confMatrix)) / sum(confMatrix)
print(paste("Accuracy:", accuracy))

# Perform Linear Discriminant Analysis (LDA) on training data
ldaModel <- lda(Direction ~ Lag2, data = trainingData)
ldaPredictions <- predict(ldaModel, newdata = testingData)
ldaConfMatrix <- table(Actual = testingData$Direction, Predicted = ldaPredictions$class)
print("Confusion Matrix for LDA:")
print(ldaConfMatrix)

# Calculate overall fraction of correct predictions
ldaAccuracy <- sum(diag(ldaConfMatrix)) / sum(ldaConfMatrix)
print(paste("Accuracy:", ldaAccuracy))

# Perform Quadratic Discriminant Analysis (QDA) on training data
qdaModel <- qda(Direction ~ Lag2, data = trainingData)
qdaPredictions <- predict(qdaModel, newdata = testingData)
qdaConfMatrix <- table(Actual = testingData$Direction, Predicted = qdaPredictions$class)
print("Confusion Matrix for QDA:")
print(qdaConfMatrix)

# Calculate overall fraction of correct predictions
qdaAccuracy <- sum(diag(qdaConfMatrix)) / sum(qdaConfMatrix)
print(paste("Accuracy:", qdaAccuracy))

# Perform k-Nearest Neighbors (KNN) with k = 1 on training data
knnPredictions <- knn(train = as.matrix(trainingData$Lag2), test = as.matrix(testingData$Lag2), cl = trainingData$Direction, k = 1)
knnConfMatrix <- table(Actual = testingData$Direction, Predicted = knnPredictions)
print("Confusion Matrix for KNN (k = 1):")
print(knnConfMatrix)

# Calculate overall fraction of correct predictions
knnAccuracy <- sum(diag(knnConfMatrix)) / sum(knnConfMatrix)
print(paste("Accuracy:", knnAccuracy))

# Experimenting with predictors and K values
# Function to evaluate a model and return confusion matrix
evaluateModel <- function(modelType, predictors, trainingData, testingData, k = NULL) {
  formula <- as.formula(paste("Direction ~", paste(predictors, collapse = "+")))
  
  if (modelType == "knn") {
    library(class)  # Load the 'class' package for knn
    
    # Ensure 'train' and 'test' have the same number of columns
    commonCols <- intersect(colnames(trainingData), colnames(testingData))
    trainData <- trainingData[, c(predictors, "Direction", commonCols)]
    testData <- testingData[, c(predictors, "Direction", commonCols)]
    
    knnPredictions <- knn(train = as.matrix(trainingData$Lag2), test = as.matrix(testingData$Lag2), cl = trainingData$Direction, k = 1)
    knnConfMatrix <- table(Actual = testingData$Direction, Predicted = knnPredictions)
    
    model <- knn(train = as.matrix(trainData[, predictors]), test = as.matrix(testData[, predictors]), cl = trainData$Direction, k = k)
    predictions <- as.factor(model)
  } else {
    model <- glm(formula, data = trainingData, family = "binomial")
    predictions <- predict(model, newdata = testingData, type = "response")
    predictions <- ifelse(predictions > 0.5, "Up", "Down")
  }
  
  confusionMatrix <- table(Actual = testingData$Direction, Predicted = predictions)
  return(list(model = model, confusionMatrix = confusionMatrix))
}

# Use Lag1 as predictor for logistic regression
model1 <- evaluateModel("logistic", c("Lag1"), trainingData, testingData)

# Use Lag2 and Lag1 for LDA
model2 <- evaluateModel("lda", c("Lag2", "Lag1"), trainingData, testingData)

# Use Lag2 and Lag1 for QDA
model3 <- evaluateModel("qda", c("Lag2", "Lag1"), trainingData, testingData)

# Use Lag2 for KNN with k = 3
model4 <- evaluateModel("knn", c("Lag2"), trainingData, testingData, k = 3)

# Print the results
print("Logistic Regression (Lag1):")
print(model1$confusionMatrix)

print(paste("Accuracy: ", calculateAccuracy(model1$confusionMatrix)))

print("LDA (Lag2, Lag1):")
print(model2$confusionMatrix)

print(paste("Accuracy: ", calculateAccuracy(model2$confusionMatrix)))

print("QDA (Lag2, Lag1):")
print(model3$confusionMatrix)

print(paste("Accuracy: ", calculateAccuracy(model3$confusionMatrix)))

print("KNN (Lag2, k = 3):")
print(model4$confusionMatrix)

print(paste("Accuracy: ", calculateAccuracy(model4$confusionMatrix)))

# Logistic Regression with Lag3, Lag4, and Lag5
confusionMatrixLogistic_Lag3_4_5 <- evaluateModel("logistic", c("Lag3", "Lag4", "Lag5"), trainingData, testingData)
print("Logistic Regression (Lag3, Lag4, Lag5):")
print(confusionMatrixLogistic_Lag3_4_5$confusionMatrix)
print(paste("Accuracy: ", calculateAccuracy(confusionMatrixLogistic_Lag3_4_5$confusionMatrix)))

# LDA with Lag1 and Lag2
confusionMatrixLDA_Lag1_2 <- evaluateModel("lda", c("Lag1", "Lag2"), trainingData, testingData)
print("LDA (Lag1, Lag2):")
print(confusionMatrixLDA_Lag1_2$confusionMatrix)
print(paste("Accuracy: ", calculateAccuracy(confusionMatrixLDA_Lag1_2$confusionMatrix)))

# QDA with Lag2 and Lag4
confusionMatrixQDA_Lag2_4 <- evaluateModel("qda", c("Lag2", "Lag4"), trainingData, testingData)
print("QDA (Lag2, Lag4):")
print(confusionMatrixQDA_Lag2_4$confusionMatrix)
print(paste("Accuracy: ", calculateAccuracy(confusionMatrixQDA_Lag2_4$confusionMatrix)))

# KNN with Lag3, Lag4, and Lag5 (k = 3)
confusionMatrixKNN_Lag3_4_5 <- evaluateModel("knn", c("Lag3", "Lag4", "Lag5"), trainingData, testingData, k = 3)
print("KNN (Lag3, Lag4, Lag5, k = 3):")
print(confusionMatrixKNN_Lag3_4_5$confusionMatrix)
print(paste("Accuracy: ", calculateAccuracy(confusionMatrixKNN_Lag3_4_5$confusionMatrix)))

# Problem 2

autoData <- read.csv("Auto.csv")
str(autoData)

# Calculate the median of 'mpg'
mpgMedian <- median(autoData$mpg)

# Create 'mpg01' based on the condition
autoData$mpg01 <- ifelse(autoData$mpg > mpgMedian, 1, 0)

str(autoData)

# Scatterplots of mpg01 against other variables
par(mfrow = c(2, 3))  # 2 rows, 3 columns for multiple plots

for (i in 2:7) {
  plot(autoData[, i], autoData$mpg01, main = paste("mpg01 vs", colnames(autoData)[i]), xlab = colnames(autoData)[i], ylab = "mpg01")
}

par(mfrow = c(1, 1))  # Reset to a single plot


# Boxplot for each variable against mpg01 to identify most associated features to mpg01
par(mfrow = c(3, 4))  
for (col in colnames(autoData)[1:11]) {
  boxplot(autoData[[col]] ~ autoData$mpg01, main = col, xlab = "mpg01", ylab = col, col = c("blue", "red"))
}

evaluateModels <- function(predictors, response, trainingData, testingData, k_values = 1:10) {
  results <- data.frame(Method = character(), K = integer(), TestError = numeric())
  
  # LDA
  ldaFit <- lda(as.formula(paste(response, "~", paste(predictors, collapse = "+"))), data = trainingData)
  ldaPredictions <- predict(ldaFit, testingData)$class
  ldaError <- mean(ldaPredictions != testingData[, response])
  results <- rbind(results, data.frame(Method = "LDA", K = NA, TestError = ldaError))
  
  # QDA
  qdaFit <- qda(as.formula(paste(response, "~", paste(predictors, collapse = "+"))), data = trainingData)
  qdaPredictions <- predict(qdaFit, testingData)$class
  qdaError <- mean(qdaPredictions != testingData[, response])
  results <- rbind(results, data.frame(Method = "QDA", K = NA, TestError = qdaError))
  
  # KNN with different values of K
  for (k in k_values) {
    knnFit <- knn(train = trainingData[, predictors], test = testingData[, predictors], cl = trainingData[, response], k = k)
    knnPredictions <- as.factor(knnFit)
    knnError <- mean(knnPredictions != testingData[, response])
    results <- rbind(results, data.frame(Method = "KNN", K = k, TestError = knnError))
  }
  
  return(results)
}

predictors <- c("cylinders", "displacement", "weight")
response <- "mpg01"

# Set seed for reproducibility
set.seed(123)

# Determine the size of the training set (e.g., 80% of the data)
trainSize <- floor(0.8 * nrow(autoData))

# Create a vector of indices
indices <- sample(seq_len(nrow(autoData)), size = trainSize)

# Split the data
trainSet <- autoData[indices, ]
testSet <- autoData[-indices, ]


results <- evaluateModels(predictors, response, trainSet, testSet, k_values = 1:10)
print(results)
