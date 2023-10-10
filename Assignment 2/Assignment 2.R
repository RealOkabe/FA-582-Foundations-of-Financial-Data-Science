library(dplyr)
library(rvest)

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
