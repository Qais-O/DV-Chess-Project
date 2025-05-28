# Load necessary libraries
library(tidyverse) # For data manipulation
library(tidyr)     # For handling missing values
library(ggplot2)   # For visualization
data <- read_csv("chess.csv")


# 1. Check the structure of the data
str(data)  # View the types of each column to detect inconsistencies

# 2. Summarize missing values
colSums(is.na(data))  # Show how many missing values exist in each column

# 3. Detect and remove duplicates
# Check for duplicate rows
sum(duplicated(data))  # Print the number of duplicated rows

# Remove duplicate rows (keep only the first occurrence)
data <- data %>% distinct()


# 4. Generate summary statistics for the cleaned data

summary(data)

# 5. (Optional) Basic exploratory plot: distribution of number of turns
ggplot(data, aes(x = turns)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Number of Turns in Games",
       x = "Number of Turns",
       y = "Frequency")

ggplot(data, aes(x = victory_status)) +
  geom_bar(fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of victory status in Games",
       x = "Victory Status",
       y = "Frequency")

write.csv(data, "cleaned_chess_data.csv", row.names = FALSE)
