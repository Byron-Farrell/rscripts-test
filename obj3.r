# Load required packages
library(dplyr)

# Read the dataset
data <- read.csv("data.csv")

# Calculate mean and standard deviation for score
mean_score <- mean(data$score)
sd_score <- sd(data$score)

# Add Z-score column
data <- data %>%
  mutate(score_z = (score - mean_score) / sd_score)

# Save the transformed dataset
write.csv(data, "obj3.csv", row.names = FALSE)
