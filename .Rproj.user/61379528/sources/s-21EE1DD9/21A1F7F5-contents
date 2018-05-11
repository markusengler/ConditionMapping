library(tidyverse)
data <- read_csv("exampledata.csv")
map_condition <- read_csv("mapcondition.csv")

# add IV columns
data[, "Edge orientation"] <- NA
data[, "Target location"] <- NA
data[, "Edge section"] <- NA
data[, "Side section"] <- NA

# map endIndex to condition
for(i in 1:nrow(data)){
  # starting index of table is one
  index_that_maps <- data$endIndex[i] + 1
  row_to_map <- map_condition[index_that_maps,]
  data[i,]$`Edge orientation` <- row_to_map$`Edge orientation`
  data[i,]$`Target location` <- row_to_map$`Target location`
  data[i,]$`Edge section` <- row_to_map$`Edge section`
  data[i,]$`Side section` <- row_to_map$`Side section`
}