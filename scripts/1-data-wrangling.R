data <- read.csv(file = "data/bike_buyers.csv")

library(skimr)
library(caret)


skimmed_data <-  skim(data)

## Report
# skimmed_data[, c(1:5, 9:11, 13, 15:16)]


preProcess_missingdata <-  preProcess(data, method = "knnImpute")

##Report
#preProcess_missingdata

data <-  predict(preProcess_missingdata, newdata = data)


##Report
#anyNA(data)

# Save the data-set

save(data, file = "rda/bike_buyers.rda")