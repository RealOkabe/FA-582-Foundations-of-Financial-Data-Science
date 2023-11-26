library(rpart)
library(tree)

# Problem 1

ojData <- read.csv("OJ.csv")

# Convert catergorical data to factors
ojData$Purchase <- as.factor(ojData$Purchase)
ojData$Store7 <- as.factor(ojData$Store7)

# Set the seed for reproducibility
set.seed(123)

# Get the total number of observations
n <- nrow(ojData)

# Generate a random sample of 800 observation indices for the training set
trainIndices <- sample(1:n, 800)

# Create the training set
trainSet <- ojData[trainIndices, ]

# Create the test set with the remaining observations
testSet <- ojData[-trainIndices, ]

# Fit the training data into a decision tree
fit <- rpart(Purchase ~ ., data = trainSet, method = "class")
# Get the summary of the tree
summary(fit)


# Find out the error rate
predictions <- predict(fit, newdata = trainSet, type = "class")
trainingErrorRate <- mean(predictions != trainSet$Purchase)
print(trainingErrorRate)

# Get the number of terminal nodes
length(unique(fit$where))

# Check the full tree out
fit

# Plot the tree
par(mar = c(0, 4, 4, 2) + 0.1)
plot(fit, uniform = TRUE, main = "Classification Tree")
text(fit, use.n = TRUE, all = TRUE, cex = .8)

# Test the tree with testdata
testPredictions <- predict(fit, newdata = testSet, type = "class")

# Get the confusion matrix
confusionMatrix <- table(Predicted = testPredictions, Actual = testSet$Purchase)
confusionMatrix

# Calculate the test error rate
testErrorRate <- 1 - sum(diag(confusionMatrix)) / sum(confusionMatrix)
testErrorRate

fitTree <- tree(Purchase ~ ., data = trainSet)
cvFit <- cv.tree(fitTree)

# Plot the cross-validated classification error rate against the tree size
plot(cvFit$size, cvFit$dev, type = "b", xlab = "Tree Size", ylab = "Cross-Validated Error")
