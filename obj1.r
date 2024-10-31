install.packages("dplyr")

# Load required packages
library(dplyr)

# Read the dataset
data <- read.csv("data.csv")

# Filter rows with score > 80
filtered_data <- data %>% filter(score > 80)

# Save the transformed dataset
write.csv(filtered_data, "filtered_data.csv", row.names = FALSE)
