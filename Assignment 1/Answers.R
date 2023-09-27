library(dplyr)
library(ggplot2)
library(gridExtra)

# Problem 2
# Read data for 3 days and store into variables
nyt1 <- read.csv("nyt1.csv")
nyt2 <- read.csv("nyt2.csv")
nyt3 <- read.csv("nyt3.csv")
# Remove columns in which Age is 0. Just cleaning the data
nyt1 <- nyt1[ nyt1$Age != 0, ]
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