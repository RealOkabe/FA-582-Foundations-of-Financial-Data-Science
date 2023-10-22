library(dplyr)
library(rvest)
library(proxy)

wikiUrl <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"

htmlCode <- read_html(wikiUrl)

# Extract data from html table into a dataframe
companiesTable <- html_element(htmlCode, "table.sortable") %>% html_table()

# Changing the name of column Security to Company Name
names(companiesTable)[2] = "Company Name"

summary(companiesTable)

companiesTable$`Headquarters State` <- sapply(strsplit(companiesTable$`Headquarters Location`, ","), function(x) {
  trimws(x[2])
}
  )

companiesTable <- companiesTable[!grepl(";", companiesTable$`Headquarters State`), ]


# Use table to count the number of companies in each state
stateCounts <- table(companiesTable$`Headquarters State`)
print(stateCounts)
# Plot the number of companies across states
barplot(stateCounts, main = "Number of Companies by State", xlab = "State", ylab = "Number of Companies", col = "skyblue", las = 2, cex.names = 0.7)

# Use table to count the number of companies in each GICS Sector
sectorCounts <- table(companiesTable$`GICS Sector`)
# Change margins to accomodate Sector names
par(mar=c(4,7,4,4))
# Plot Number of Companies by GISC Sector
barplot(sectorCounts, 
        main = "Number of Companies by GISC Sector",
        xlab = "GISC Sector",
        ylab = "Number of Companies",
        col = "red",
        las = 1,
        cex.names = 0.6,
        font.axis = 3,
        horiz = TRUE
        )

# Problem 2

# Read data
fundamentalsData <- read.csv("fundamentals.csv")
securitiesData <- read.csv("securities.csv")

# Select 100 random rows from securitiesData
securitiesData <- securitiesData[sample(nrow(securitiesData), 100), ]

# Rename Ticker.symbol to Ticker.Symbol for consistency
securitiesData <- securitiesData %>%
  rename(Ticker.Symbol = Ticker.symbol)

# Filtering fundamentalsData to include Tickers that are present in securitiesData
fundamentalsData <- fundamentalsData[ fundamentalsData$Ticker.Symbol %in% securitiesData$Ticker.Symbol, ]

# Merge fundamentalsData and securitiesData for categorical columns
mergedData <- fundamentalsData %>%
  left_join(securitiesData, by = "Ticker.Symbol")

# Select the relevant columns (GICS.Sector and GICS.Sub.Industry) from the merged data
selectedData <- mergedData[, c("Ticker.Symbol", "GICS.Sector", "GICS.Sub.Industry")]

# Add selectedData to fundamentalsData
fundamentalsData <- cbind(fundamentalsData, selectedData)

# Changing Period.Ending from character to Date
fundamentalsData$Period.Ending <- as.Date(fundamentalsData$Period.Ending, format = "%Y-%m-%d")

# Filtering fundamentalsData to include data from year 2012
fundamentalsData <- subset(fundamentalsData, format(fundamentalsData$Period.Ending, "%Y") == 2012)

# Select 10 Quantitative columns
quantitativeColumns <- c(
  "After.Tax.ROE", "Cash.Ratio", "Cash.and.Cash.Equivalents", 
  "Current.Ratio", "Gross.Margin", "Gross.Profit", 
  "Operating.Income", "Pre.Tax.ROE", "Profit.Margin", "Total.Revenue"
)

categoricalColumns <- c("GICS.Sector", "GICS.Sub.Industry")

quantitativeData <- fundamentalsData[, quantitativeColumns]
quantitativeData <- na.omit(quantitativeData)

normalizedQuantitativeData <- scale(quantitativeData)

categoricalData <- fundamentalsData[, categoricalColumns ]
categoricalData <- na.omit(categoricalData)

# Function to calculate Lp Norm across rows
calculateLpNormRow <- function(x, y, p) {
  return (sum(abs(x - y) ^ p, na.rm = TRUE) ^ (1 / p))
}

# Function to calculate Lp Similarity across rows
calculateLpSimilarityRow <- function(x, y, p) {
  return (1 / (1 + calculateLpNormRow(x, y, p)))
}

# Function to calculate Match based similarity across rows
calculateMatchSimilarityRow <- function(x, y) {
  # Convert x and y to numeric if they are not already
  x <- as.numeric(x)
  y <- as.numeric(y)
  
  # Create equi-width buckets for x and y
  xBuckets <- cut(x, breaks = 3, labels = FALSE)
  yBuckets <- cut(y, breaks = 3, labels = FALSE)
  
  # Calculate similarity based on matching buckets
  similarity <- sum(xBuckets == yBuckets) / 3
  
  return(similarity)
}

numberOfRows <- nrow(quantitativeData)

# Initialize matrices to store distances and similarities
l1Distances <- matrix(0, nrow = numberOfRows, ncol = numberOfRows)
l1Similarities <- matrix(0, nrow = numberOfRows, ncol = numberOfRows)

l2Distances <- matrix(0, nrow = numberOfRows, ncol = numberOfRows)
l2Similarities <- matrix(0, nrow = numberOfRows, ncol = numberOfRows)

l3Distances <- matrix(0, nrow = numberOfRows, ncol = numberOfRows)
l3Similarities <- matrix(0, nrow = numberOfRows, ncol = numberOfRows)

l10Distances <- matrix(0, nrow = numberOfRows, ncol = numberOfRows)
l10Similarities <- matrix(0, nrow = numberOfRows, ncol = numberOfRows)

minkowskiDistances <- matrix(0, nrow = numberOfRows, ncol = numberOfRows)

mahalanobisDistances <- matrix(0, nrow = numberOfRows, ncol = numberOfRows)

matchSimilarities <- matrix(0, nrow = numberOfRows, ncol = numberOfRows)

overlapMeasures <- matrix(0, nrow = numberOfRows, ncol = numberOfRows)

ifsMeasures <- matrix(0, nrow = numberOfRows, ncol = numberOfRows)

goodallMeasures <- matrix(0, nrow = numberOfRows, ncol = numberOfRows)

categoricalSimilarities <- matrix(0, nrow = numberOfRows, ncol = numberOfRows)

quantitativeData <- sapply(quantitativeData, as.numeric)

# Calculate distances and similarities across rows
for (i in 1:numberOfRows) {
  for (j in 1:numberOfRows) {
    if (i != j) {
      # Calculate Lp Distances and Similarities directly within the assignment
      l1Distances[i, j] <- as.numeric(calculateLpNormRow(quantitativeData[i, ], quantitativeData[j, ], 1))
      l1Similarities[i, j] <- as.numeric(calculateLpSimilarityRow(quantitativeData[i, ], quantitativeData[j, ], 1))
      
      l2Distances[i, j] <- as.numeric(calculateLpNormRow(quantitativeData[i, ], quantitativeData[j, ], 2))
      l2Similarities[i, j] <- as.numeric(calculateLpSimilarityRow(quantitativeData[i, ], quantitativeData[j, ], 2))
      
      l3Distances[i, j] <- as.numeric(calculateLpNormRow(quantitativeData[i, ], quantitativeData[j, ], 3))
      l3Similarities[i, j] <- as.numeric(calculateLpSimilarityRow(quantitativeData[i, ], quantitativeData[j, ], 3))
        
      l10Distances[i, j] <- as.numeric(calculateLpNormRow(quantitativeData[i, ], quantitativeData[j, ], 10))
      l10Similarities[i, j] <- as.numeric(calculateLpSimilarityRow(quantitativeData[i, ], quantitativeData[j, ], 10))
      
      # Calculate the Mahalanobis distance between row i and row j using normalized data because it was not possible using regular data
      mahalanobisDistances[i, j] <- mahalanobis(normalizedQuantitativeData[i, , drop = FALSE], normalizedQuantitativeData[j, , drop = FALSE], cov(normalizedQuantitativeData))
      
      # Calculate Match Similarity directly within the assignment
      matchSimilarities[i, j] <- calculateMatchSimilarityRow(quantitativeData[i, ], quantitativeData[j, ])
      
      overlapMeasures[i, j] <- as.numeric(all(quantitativeData[i, ] == quantitativeData[j, ]))
      
      commonValues <- intersect(quantitativeData[i, ], quantitativeData[j, ])
      uniqueValuesCount <- length(unique(quantitativeData[i, ]))
      ifsMeasures[i, j] <- length(commonValues) / uniqueValuesCount
      
      # Calculate Goodall similarity directly within the assignment
      commonValues <- intersect(quantitativeData[i, ], quantitativeData[j, ])
      goodallMeasures[i, j] <- length(commonValues)
      
      # Calculate categorical data similarity
      if (categoricalData[i, ]$GICS.Sector == categoricalData[j, ]$GICS.Sector & 
          categoricalData[i, ]$GICS.Sub.Industry == categoricalData[j, ]$GICS.Sub.Industry) {
        categoricalSimilarities[i, j] = 2
      } else if (categoricalData[i, ]$GICS.Sector != categoricalData[j, ]$GICS.Sector & 
                 categoricalData[i, ]$GICS.Sub.Industry != categoricalData[j, ]$GICS.Sub.Industry) {
        categoricalSimilarities[i, j] = 0
      } else {
        categoricalSimilarities[i, j] = 1
      }
    } else {
      goodallMeasures[i, j] <- 1
      overlapMeasures[i, j] <- 1
      categoricalSimilarities[i, j] <- 2
    }
  }
}

calculateNormalizedWeights <- function(lpDistancesMatrix) {
  # Calculate the inverse of the L2 distances matrix
  invLpDistancesMatrix <- solve(lpDistancesMatrix)
  
  # Sum each row of the inverse matrix
  rowSumsInvMatrix <- rowSums(invLpDistancesMatrix)
  
  # Normalize the sums to obtain the normalized weights
  normalizedWeights <- rowSumsInvMatrix / sum(rowSumsInvMatrix)
  
  return(normalizedWeights)
}

# Calculate normalized weights for Minkowski distances
normalizedWeights <- calculateNormalizedWeights(l2Distances)

# Calculate Minkowski distances
for (i in 1:numberOfRows) {
  for (j in 1:numberOfRows) {
    if (i != j) {
      minkowskiDistances[i, j] <- sum(normalizedWeights * (l2Distances[i, ]^2))^(1/2)
    }
  }
}

# Normalize the goodallMeasures matrix by dividing by max value
maxGoodall <- max(goodallMeasures)
goodallMeasures <- goodallMeasures / maxGoodall

overallSimilarities <- matrix(0, nrow = numberOfRows, ncol = numberOfRows)

# Calculate overall similarity using mixed type data using lambda = 0.5 to give equal weight to both
for (i in 1:numberOfRows) {
  for (j in 1:numberOfRows) {
    # Using l1norm for the overall similarity
    overallSimilarities[i, j] <- 0.5 * l1Similarities[i, j] + (1 - 0.5) * categoricalSimilarities[i, j]
  }
}

# Normalize quantitativeData for a more unbiased similarity between that and categoricalData
scaledData <- scale(quantitativeData)
quantitativeData[, sapply(quantitativeData, is.numeric)] <- scaledData

overallNormalizedSimilarities <- matrix(0, nrow = numberOfRows, ncol = numberOfRows)

# Normalize L1 Similarities for normalized overall similarities
normalizedL1Similarities <- scale(l1Similarities)

# Calculate overall similarities again
for (i in 1:numberOfRows) {
  for (j in 1:numberOfRows) {
    overallNormalizedSimilarities[i, j] <- 0.5 * normalizedL1Similarities[i, j] * (1 - 0.5) * categoricalSimilarities[i, j]
  }
}

# Function to find the top and bottom pairs with values and corresponding Ticker.Symbol pairs
getTopAndBottomValuesIndices <- function(matrix) {
  numElements <- nrow(matrix)
  
  # Create an index matrix to track the indices of the elements
  indexMatrix <- expand.grid(row = 1:numElements, col = 1:numElements)
  indexMatrix <- indexMatrix[indexMatrix$row != indexMatrix$col, ]
  
  # Get the values corresponding to the indices
  values <- matrix[indexMatrix$row + (indexMatrix$col - 1) * numElements]
  
  # Combine the indices and values
  data <- data.frame(index1 = indexMatrix$row, index2 = indexMatrix$col, value = values)
  
  # Get the top 10 pairs with values
  top10 <- data[order(data$value, decreasing = FALSE)[1:10], ]
  
  # Get the bottom 10 pairs with values
  bottom10 <- data[order(data$value, decreasing = TRUE)[1:10], ]
  
  return(list(top10, bottom10))
}



# Function to print the top and bottom 10 pairs and values
printTopAndBottomValues <- function(top10Values, bottom10Values, dataframe) {
  print("Printing the top 10 values")
  for (i in 1:nrow(top10Values)) {
    # print(dataframe[(top10Values[i, ]$index1), ]$Ticker.Symbol, " ", dataframe[(top10Values[i, ]$index2), ]$Ticker.Symbol)
    cat("The pair of Tickers", dataframe[(top10Values[i, ]$index1), ]$Ticker.Symbol, dataframe[top10Values[i, ]$index2, ]$Ticker.Symbol, "has value", top10Values[i, ]$value, "\n")
  }
  print("Printing the bottom 10 values")
  for (i in 1:nrow(bottom10Values)) {
    # print(dataframe[(top10Values[i, ]$index1), ]$Ticker.Symbol, " ", dataframe[(top10Values[i, ]$index2), ]$Ticker.Symbol)
    cat("The pair of Tickers", dataframe[(bottom10Values[i, ]$index1), ]$Ticker.Symbol, dataframe[bottom10Values[i, ]$index2, ]$Ticker.Symbol, "has value", bottom10Values[i, ]$value, "\n")
  }
}