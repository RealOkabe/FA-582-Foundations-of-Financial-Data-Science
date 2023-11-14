# Problem 1
weeklyData <- read.csv("Weekly.csv")
str(weeklyData)
summary(weeklyData)

numericalVars <- weeklyData[, c("Lag1", "Lag2", "Lag3", "Lag4", "Lag5", "Volume", "Today")]

# Boxplot of variables
boxplot(numericalVars[, -9], main = "Boxplot of Weekly Data")

# Histogram of variables
par(mfrow = c(2, 3))  # 2 rows, 3 columns for multiple plots
for (i in 2:7) {
  hist(numericalVars[, i], main = colnames(numericalVars)[i], xlab = "")
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

