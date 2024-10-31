# Load required packages
library(dplyr)

# Read the dataset
data <- read.csv("data.csv")

# Add age group column
data <- data %>%
  mutate(age_group = case_when(
    age >= 20 & age <= 22 ~ "20-22",
    age >= 23 & age <= 25 ~ "23-25",
    TRUE ~ "Other"
  ))

# Save the transformed dataset
write.csv(data, "age_group_data.csv", row.names = FALSE)
