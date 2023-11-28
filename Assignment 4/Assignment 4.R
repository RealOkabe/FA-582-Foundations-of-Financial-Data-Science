library(class)
library(gbm)
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
trainingErrorRate

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

# Problem 2

caravanData <- read.csv("CARAVAN.csv")

# Convert categorical variable to factor
caravanData$Purchase <- as.factor(caravanData$Purchase)

# Convert factor levels to 0 and 1
caravanData$Purchase <- as.numeric(caravanData$Purchase) - 1

# Set the seed for reproducibility
set.seed(123)

# Get the total number of observations
n <- nrow(caravanData)

# Generate a random sample of 800 observation indices for the training set
trainIndices <- sample(1:n, 1000)

# Create the training set
trainSet <- caravanData[trainIndices, ]

# Create the test set with the remaining observations
testSet <- caravanData[-trainIndices, ]

# Fit the boosting model
fit <- gbm(Purchase ~ ., data = trainSet, distribution = "bernoulli", n.trees = 1000, shrinkage = 0.01, interaction.depth = 1)

# Print a summary of the model
summary(fit)

# Predict the response on the test data
pred <- predict(fit, newdata = testSet, n.trees = 1000, type = "response")

predBinary <- ifelse(pred > 0.20, 1, 0)

# Create a confusion matrix
cm <- table(Predicted = predBinary, Actual = testSet$Purchase)
cm

# Perform KNN on the same data to check predicting performance
trainLabels <- trainSet$Purchase
knnPred <- knn(train = trainSet, test = testSet, cl = trainLabels, k = 3)
cmKnn <- table(Predicted = knnPred, Actual = testSet$Purchase)
cmKnn

# Perform Logistic Regression on the same data
logisticModel <- glm(Purchase ~ ., data = trainSet, family = binomial)
logisticPred <- predict(logisticModel, newdata = testSet, type = "response")

logisticPredBinary <- ifelse(logisticPred > 0.5, 1, 0)
cmLogistic <- table(Predicted = logisticPredBinary, Actual = testSet$Purchase)
cmLogistic

# Problem 3

# Set seed for reproducibility
set.seed(123)

# Generate data for three classes
class1 <- matrix(rnorm(20 * 50, mean = 0, sd = 1), ncol = 50)
class2 <- matrix(rnorm(20 * 50, mean = 3, sd = 1), ncol = 50)
class3 <- matrix(rnorm(20 * 50, mean = -3, sd = 1), ncol = 50)

# Combine data for all classes
simulatedData <- rbind(class1, class2, class3)

# True class labels
trueLabels <- rep(1:3, each = 20)

# Perform PCA
pcaResult <- prcomp(simulatedData, center = TRUE, scale. = TRUE)

# Extract the first two principal components
pcScores <- pcaResult$x[, 1:2]

# Plot the first two principal component score vectors
plot(pcScores, col = rep(1:3, each = 20), pch = 19, xlab = "PC1", ylab = "PC2", main = "PCA of Simulated Data")

# Add legend
legend("topright", legend = c("Class 1", "Class 2", "Class 3"), col = 1:3, pch = 19)

performKMeans <- function(data, trueLabels, K) {
  # Perform K-means clustering
  kmeansResult <- kmeans(data, centers = K, nstart = 20)
  
  # Extract cluster assignments
  clusterAssignments <- kmeansResult$cluster
  
  # Plot the first two principal component score vectors with cluster colors
  plot(pcScores, col = clusterAssignments, pch = 19, xlab = "PC1", ylab = "PC2", 
       main = paste("K-means Clustering (K =", K, ")"))
  
  # Add cluster centers to the plot
  points(kmeansResult$centers[, 1:2], col = 1:K, pch = 3, cex = 2)
  
  # Add legend
  legend("topright", legend = c(paste("Cluster", 1:K), "Cluster Centers"), 
         col = c(1:K, 1:K), pch = c(rep(19, K), 3))
  
  # Compare true class labels with K-means cluster assignments
  comparisonTable <- table(trueLabels, clusterAssignments)
  cat("Contingency Table (K =", K, "):\n", comparisonTable, "\n")
  
  # Calculate the accuracy (proportion of correctly classified instances)
  accuracy <- sum(diag(comparisonTable)) / sum(comparisonTable)
  cat("Accuracy:", accuracy, "\n\n")
  
  # Return the cluster assignments for further analysis if needed
  return(clusterAssignments)
}

# Perform k means clustering with diferrent values of k
performKMeans(simulatedData, trueLabels, 3)

performKMeans(simulatedData, trueLabels, 2)

performKMeans(simulatedData, trueLabels, 4)

# Perform K-means clustering with K = 3 on the first two principal components
kmeansResult <- kmeans(pcScores, centers = 3, nstart = 20)

# Extract cluster assignments
clusterAssignments <- kmeansResult$cluster

# Plot the first two principal component score vectors with cluster colors
plot(pcScores, col = clusterAssignments, pch = 19, xlab = "PC1", ylab = "PC2", main = "K-means Clustering (PC1 and PC2)")

# Add cluster centers to the plot
points(kmeansResult$centers[, 1:2], col = 1:3, pch = 3, cex = 2)

# Add legend
legend("topright", legend = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster Centers"), col = c(1:3, 1:3), pch = c(19, 19, 19, 3))

# Compare true class labels with K-means cluster assignments
comparisonTable <- table(trueLabels, clusterAssignments)
cat("Contingency Table:\n", comparisonTable, "\n")

# Calculate the accuracy (proportion of correctly classified instances)
accuracy <- sum(diag(comparisonTable)) / sum(comparisonTable)
cat("Accuracy:", accuracy, "\n")

# Scale the data to have standard deviation one for each variable
scaledData <- scale(simulatedData)

# Perform PCA on the scaled data
pcaResult <- prcomp(scaledData, center = TRUE, scale. = FALSE)  # Scaling is already done

# Extract the first two principal components
pcScores <- pcaResult$x[, 1:2]

# Plot the first two principal component score vectors
plot(pcScores, col = rep(1:3, each = 20), pch = 19, xlab = "PC1", ylab = "PC2", main = "PCA of Scaled Data")

# Add legend
legend("topright", legend = c("Class 1", "Class 2", "Class 3"), col = 1:3, pch = 19)

# Perform K-means clustering with K = 3 on the scaled data
kmeansResult <- kmeans(scaledData, centers = 3, nstart = 20)

# Extract cluster assignments
clusterAssignments <- kmeansResult$cluster

# Plot the first two principal component score vectors with cluster colors
plot(pcScores, col = clusterAssignments, pch = 19, xlab = "PC1", ylab = "PC2", main = "K-means Clustering on Scaled Data")

# Add cluster centers to the plot
points(kmeansResult$centers[, 1:2], col = 1:3, pch = 3, cex = 2)

# Add legend
legend("topright", legend = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster Centers"), col = c(1:3, 1:3), pch = c(19, 19, 19, 3))

# Compare true class labels with K-means cluster assignments
comparisonTable <- table(trueLabels, clusterAssignments)
cat("Contingency Table:\n", comparisonTable, "\n")

# Calculate the accuracy (proportion of correctly classified instances)
accuracy <- sum(diag(comparisonTable)) / sum(comparisonTable)
cat("Accuracy:", accuracy, "\n")

