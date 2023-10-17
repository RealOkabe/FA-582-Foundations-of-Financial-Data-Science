library(dplyr)
library(rvest)
library(cov)

wikiUrl <- "https://en.wikipedia.org/wiki/List_ofS%26P_500_companies"

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

# Filtering fundamentalsData to include Tickers that are present in securitiesData
fundamentalsData <- fundamentalsData[ fundamentalsData$Ticker.Symbol %in% securitiesData$Ticker.symbol, ]

# Changing Period.Ending from character to Date
fundamentalsData$Period.Ending <- as.Date(fundamentalsData$Period.Ending, format = "%Y-%m-%d")

# Filtering fundamentalsData to include data from year 2015
fundamentalsData <- subset(fundamentalsData, format(fundamentalsData$Period.Ending, "%Y") == 2015)

# Select 10 Quantitative columns
quantitativeColumns <- c(
  "After.Tax.ROE", "Cash.Ratio", "Cash.and.Cash.Equivalents", 
  "Current.Ratio", "Gross.Margin", "Gross.Profit", 
  "Operating.Income", "Pre.Tax.ROE", "Profit.Margin", "Total.Revenue"
)

quantitativeData <- fundamentalsData[, quantitativeColumns]
quantitativeData <- na.omit(quantitativeData)


# Function to calculate Lp Norm
calculateLpNorm <- function(x, y, p) {
  return (sum(abs(x - y) ^ p) ^ (1 / p))
}

# Function to calculate Lp Similarity
calculateLpSimilarity <- function(x, y, p) {
  return (1 / (1 + calculateLpNorm(x, y, p)))
}

# Function to calculate Match based similarity
calculateMatchSimilarity <- function(x, y) {
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

# Number of columns
numColumns <- length(quantitativeColumns)

# Initialize 10x10 matrices to store distances and similarities
l1Distances <- matrix(0, nrow = numColumns, ncol = numColumns)
l1Similarities <- matrix(0, nrow = numColumns, ncol = numColumns)

l2Distances <- matrix(0, nrow = numColumns, ncol = numColumns)
l2Similarities <- matrix(0, nrow = numColumns, ncol = numColumns)

l3Distances <- matrix(0, nrow = numColumns, ncol = numColumns)
l3Similarities <- matrix(0, nrow = numColumns, ncol = numColumns)

l10Distances <- matrix(0, nrow = numColumns, ncol = numColumns)
l10Similarities <- matrix(0, nrow = numColumns, ncol = numColumns)

minkowskiDistances <- matrix(0, nrow = numColumns, ncol = numColumns)

matchSimilarities <- matrix(0, nrow = numColumns, ncol = numColumns)

# Calculate L1 distances and L1 norm-based similarities
for (i in 1:numColumns) {
  for (j in 1:numColumns) {
    if (i != j) {
      # Calculate L1 Distance and Similarity
      l1Dist <- calculateLpNorm(quantitativeData[, quantitativeColumns[i]], quantitativeData[, quantitativeColumns[j]], 1)
      l1Sim <- calculateLpSimilarity(quantitativeData[, quantitativeColumns[i]], quantitativeData[, quantitativeColumns[j]], 1)
      
      # Calculate L2 Distance and Similarity
      l2Dist <- calculateLpNorm(quantitativeData[, quantitativeColumns[i]], quantitativeData[, quantitativeColumns[j]], 2)
      l2Sim <- calculateLpSimilarity(quantitativeData[, quantitativeColumns[i]], quantitativeData[, quantitativeColumns[j]], 2)
      
      #Calculate L3 Distance and Similarity
      l3Dist <- calculateLpNorm(quantitativeData[, quantitativeColumns[i]], quantitativeData[, quantitativeColumns[j]], 3)
      l3Sim <- calculateLpSimilarity(quantitativeData[, quantitativeColumns[i]], quantitativeData[, quantitativeColumns[j]], 3)
      
      # Calculate L10 Distance and Similarity
      l10Dist <- calculateLpNorm(quantitativeData[, quantitativeColumns[i]], quantitativeData[, quantitativeColumns[j]], 10)
      l10Sim <- calculateLpSimilarity(quantitativeData[, quantitativeColumns[i]], quantitativeData[, quantitativeColumns[j]], 10)
      
      l1Distances[i, j] <- as.numeric(l1Dist)
      l1Similarities[i, j] <- as.numeric(l1Sim)
      
      l2Distances[i, j] <- as.numeric(l2Dist)
      l2Similarities[i, j] <- as.numeric(l2Sim)
      
      l3Distances[i, j] <- as.numeric(l3Dist)
      l3Similarities[i, j] <- as.numeric(l3Sim)
      
      l10Distances[i, j] <- as.numeric(l10Dist)
      l10Similarities[i, j] <- as.numeric(l10Sim)
      
      matchSimilarities[i, j] <- calculateMatchSimilarity(quantitativeData[, quantitativeColumns[i]], quantitativeData[, quantitativeColumns[j]])
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
for (i in 1:numColumns) {
  for (j in 1:numColumns) {
    if (i != j) {
      minkowskiDistances[i, j] <- sum(normalizedWeights * (l2Distances[i, ]^2))^(1/2)
    }
  }
}

scaledQuantitativeData <- scale(quantitativeData)

# Calculate the mahalanobis distances
mahalanobisDistances <- mahalanobis(scaledQuantitativeData, colMeans(scaledQuantitativeData), cov(scaledQuantitativeData))

