# install statements in case libraries are not present
install.packages("dplyr", "ggplot2", "gridExtra", "readxl")

library(dplyr)
library(ggplot2)
library(gridExtra)
library(readxl)

# Problem 1
brooklyn_data = read_excel("rollingsales_brooklyn.xlsx", skip = 4)
bronx_data = read_excel("rollingsales_bronx.xlsx", skip = 4)
manhattan_data = read_excel("rollingsales_manhattan.xlsx", skip = 4)
statenisland_data = read_excel("rollingsales_statenisland.xlsx", skip = 4)
queens_data = read_excel("rollingsales_queens.xlsx", skip = 4)

# We only want houses, no commercial buildings
housesOfInterest = function(x) {
  return(grepl("one family|two family|three family|coop|condo", tolower(x$`BUILDING CLASS CATEGORY`)))
}

# Cleaning data
# Just to be safe, removing data about houses with 0 residential units and no houses have price < $100k
cleanHousingData <- function(x) {
  na.omit(x)
  colnames(x) <- sub("\r\n", " ", colnames(x))
  x <- x[ x$`RESIDENTIAL UNITS` != 0 & 
            x$`TOTAL UNITS` != 0 & 
            x$`ZIP CODE` != 0 & 
            x$`LAND SQUARE FEET` != 0 & 
            x$`GROSS SQUARE FEET` != 0 & 
            x$`YEAR BUILT` != 0 & 
            x$`SALE PRICE` > 100000,
          ]
  x <- x[ housesOfInterest(x), ]
}

brooklyn_data <- cleanHousingData(brooklyn_data)
bronx_data <- cleanHousingData(bronx_data)
manhattan_data <- cleanHousingData(manhattan_data)
statenisland_data <- cleanHousingData(statenisland_data)
queens_data <- cleanHousingData(queens_data)

# Adding a sale year column for analysis
getSaleYear <- function(x) {
  x %>% mutate(`SALE YEAR` = as.integer(format(x$`SALE DATE`, "%Y")))
}

brooklyn_data <- getSaleYear(brooklyn_data)
bronx_data <- getSaleYear(bronx_data)
manhattan_data <- getSaleYear(manhattan_data)
statenisland_data <- getSaleYear(statenisland_data)
queens_data <- getSaleYear(queens_data)

# Combine all the data frames into one
all_data <- rbind(
  data.frame(SALE_YEAR = brooklyn_data$`SALE YEAR`, SALE_PRICE = brooklyn_data$`SALE PRICE`, BOROUGH = "Brooklyn"),
  data.frame(SALE_YEAR = bronx_data$`SALE YEAR`, SALE_PRICE = bronx_data$`SALE PRICE`, BOROUGH = "Bronx"),
  data.frame(SALE_YEAR = manhattan_data$`SALE YEAR`, SALE_PRICE = manhattan_data$`SALE PRICE`, BOROUGH = "Manhattan"),
  data.frame(SALE_YEAR = statenisland_data$`SALE YEAR`, SALE_PRICE = statenisland_data$`SALE PRICE`, BOROUGH = "Staten Island"),
  data.frame(SALE_YEAR = queens_data$`SALE YEAR`, SALE_PRICE = queens_data$`SALE PRICE`, BOROUGH = "Queens")
)

# Plotting price changes over the years
ggplot(all_data, aes(x = SALE_YEAR, y = SALE_PRICE, color = BOROUGH)) +
  geom_boxplot() +
  labs(title = "SALE PRICE Across Boroughs Over SALE YEAR",
       x = "SALE YEAR",
       y = "SALE PRICE") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)


# Problem 2
# Read data for 3 days and store into variables
nyt1 <- read.csv("nyt1.csv")
nyt2 <- read.csv("nyt2.csv")
nyt3 <- read.csv("nyt3.csv")

# Function for creating the AgeGroup column
addAgeGroup <- function(x) {
  x %>% mutate(AgeGroup = case_when(
    Age < 20 ~ "<20",
    Age >= 20 & Age <= 29 ~ "20-29",
    Age >= 30 & Age <= 39 ~ "30-39",
    Age >= 40 & Age <= 49 ~ "40-49",
    Age >= 50 & Age <= 59 ~ "50-59",
    Age >= 60 & Age <= 69 ~ "60-69",
    Age >=70 ~ "70+"
  ))
}
# Remove columns in which Age is 0. Just cleaning the data
nyt1 <- nyt1[ nyt1$Age != 0, ]
nyt1 <- addAgeGroup(nyt1)
nyt2 <- nyt2[ nyt2$Age != 0, ]
nyt2 <- addAgeGroup(nyt2)
nyt3 <- nyt3[ nyt1$Age != 0, ]
nyt3 <- addAgeGroup(nyt3)

# Removing rows in which Impressions = 0
nyt1 <- nyt1[ nyt1$Impressions != 0, ]
nyt2 <- nyt2[ nyt2$Impressions != 0, ]
nyt3 <- nyt3[ nyt3$Impressions != 0, ]
# Calculating impressions by AgeGroups
impressions1 <- aggregate(nyt1$Impressions, by=list(AgeGroup = nyt1$AgeGroup), FUN=sum)
# Plotting Impressions by AgeGroups
ggplot(impressions1, aes(x = AgeGroup, y = x, fill = AgeGroup)) +
  geom_bar(stat = "identity") +
  labs(title = "Impressions by Age Groups on Day 1",
       x = "Age Group",
       y = "Impressions") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

# Day 2 Impressions by AgeGroups
impressions2 <- aggregate(nyt2$Impressions, by=list(AgeGroup = nyt2$AgeGroup), FUN=sum)
ggplot(impressions2, aes(x = AgeGroup, y = x, fill = AgeGroup)) +
  geom_bar(stat = "identity") +
  labs(title = "Impressions by Age Groups on Day 2",
       x = "Age Group",
       y = "Impressions") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

# Day 3 Impressions by AgeGroups
impressions3 <- aggregate(nyt3$Impressions, by=list(AgeGroup = nyt3$AgeGroup), FUN=sum)
ggplot(impressions3, aes(x = AgeGroup, y = x, fill = AgeGroup)) +
  geom_bar(stat = "identity") +
  labs(title = "Impressions by Age Groups on Day 3",
       x = "Age Group",
       y = "Impressions") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

# If the Impressions are 0, then let's just make the CTR also 0
getCTR <- function(x) {
  x %>% mutate(CTR = Clicks / Impressions)
}
nyt1 <- getCTR(nyt1)
nyt2 <- getCTR(nyt2)
nyt3 <- getCTR(nyt3)
# Calculating CTR for each day and plotting it
ctr1 <- aggregate(nyt1$CTR, by=list(AgeGroup = nyt1$AgeGroup), FUN=sum)
ggplot(ctr1, aes(x = AgeGroup, y = x, fill = AgeGroup)) +
  geom_bar(stat = "identity") +
  labs(title = "CTR by Age Groups on Day 1",
       x = "Age Group",
       y = "CTR") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

ctr2 <- aggregate(nyt2$CTR, by=list(AgeGroup = nyt2$AgeGroup), FUN=sum)
ggplot(ctr2, aes(x = AgeGroup, y = x, fill = AgeGroup)) +
  geom_bar(stat = "identity") +
  labs(title = "CTR by Age Groups on Day 2",
       x = "Age Group",
       y = "CTR") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

ctr3 <- aggregate(nyt3$CTR, by=list(AgeGroup = nyt3$AgeGroup), FUN=sum)
ggplot(ctr1, aes(x = AgeGroup, y = x, fill = AgeGroup)) +
  geom_bar(stat = "identity") +
  labs(title = "CTR by Age Groups on Day 3",
       x = "Age Group",
       y = "CTR") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

# Adding categories based on Clicking behavior
getClickCategory <- function(x) {
  x %>% mutate(ClickCategory = case_when(
    Clicks == 0 ~ "Non Clicker",
    Clicks < 3 ~ "Low Clicker",
    Clicks > 3 ~ "High Clicker"
  ))
}

nyt1 <- getClickCategory(nyt1)
nyt2 <- getClickCategory(nyt2)
nyt3 <- getClickCategory(nyt3)

# Filtering data of <20 year old males and females
nytMales1 <- nyt1[ nyt1$Gender == 1 & nyt1$Age < 20, ]
nytMales2 <- nyt2[ nyt2$Gender == 1 & nyt2$Age < 20, ]
nytMales3 <- nyt3[ nyt3$Gender == 1 & nyt3$Age < 20, ]
nytFemales1 <- nyt1[ nyt1$Gender == 0 & nyt1$Age < 20, ]
nytFemales2 <- nyt2[ nyt2$Gender == 0 & nyt2$Age < 20, ]
nytFemales3 <- nyt3[ nyt3$Gender == 0 & nyt3$Age < 20, ]


# Create histograms for CTR in nytMales and nytFemales
hist_males1 <- ggplot(nytMales1, aes(x = CTR)) +
  geom_histogram(binwidth = 0.05, fill = "blue", alpha = 0.7) +
  labs(title = "CTR Distribution for Males on Day 1",
       x = "CTR",
       y = "Frequency") +
  theme_minimal()

hist_females1 <- ggplot(nytFemales1, aes(x = CTR)) +
  geom_histogram(binwidth = 0.05, fill = "pink", alpha = 0.7) +
  labs(title = "CTR Distribution for Females on Day 1",
       x = "CTR",
       y = "Frequency") +
  theme_minimal()

hist_males2 <- ggplot(nytMales2, aes(x = CTR)) +
  geom_histogram(binwidth = 0.05, fill = "blue", alpha = 0.7) +
  labs(title = "CTR Distribution for Males on Day 2",
       x = "CTR",
       y = "Frequency") +
  theme_minimal()

hist_females2 <- ggplot(nytFemales2, aes(x = CTR)) +
  geom_histogram(binwidth = 0.05, fill = "pink", alpha = 0.7) +
  labs(title = "CTR Distribution for Females on Day 2",
       x = "CTR",
       y = "Frequency") +
  theme_minimal()

hist_males3 <- ggplot(nytMales3, aes(x = CTR)) +
  geom_histogram(binwidth = 0.05, fill = "blue", alpha = 0.7) +
  labs(title = "CTR Distribution for Males on Day 3",
       x = "CTR",
       y = "Frequency") +
  theme_minimal()

hist_females3 <- ggplot(nytFemales3, aes(x = CTR)) +
  geom_histogram(binwidth = 0.05, fill = "pink", alpha = 0.7) +
  labs(title = "CTR Distribution for Females on Day 3",
       x = "CTR",
       y = "Frequency") +
  theme_minimal()

# Display the histograms side by side
grid.arrange(hist_males1, hist_females1, hist_males2, hist_females2, hist_males3, hist_females3, nrow = 3, ncol = 2)
