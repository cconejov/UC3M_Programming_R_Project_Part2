#Libraries

library(dplyr)

# Load original data
data <- read.csv(file = "data/bike_buyers.csv")

# Structure
str(data)

# Delete first column: ID of customer. It's not a predictable variable

data <- data[, -1]

# Factors variables and levels

data <-  data %>% 
  mutate(Marital.Status = factor(Marital.Status, levels = c("Married", "Single")),
         Gender = factor(Gender, levels = c("Female", "Male")),
         Education = factor(Education),
         Occupation = factor(Occupation),
         Home.Owner = factor(Home.Owner, levels = c("No", "Yes")),
         Commute.Distance = factor(Commute.Distance),
         Region = factor(Region),
         Purchased.Bike = factor(Purchased.Bike, levels = c("No", "Yes"))
         )

# Convert Purchase to a binary 0-1 variable
##data$Purchased.Bike = factor((data$Purchased.Bike == "Yes") * 1)

# Summary

summary(data)

# Save the data-set

save(data, file = "rda/bike_buyers.rda")
