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

# Use aggregate to count the number of companies in each state
stateCounts <- table(companiesTable$`Headquarters State`)
#stateCounts <- aggregate(`Headquarters State` ~ `Headquarters State`, data = companiesTable, FUN = length)
print(stateCounts)
summary(stateCounts)

