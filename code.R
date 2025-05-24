install.packages("ggplot2")
install.packages("dplyr")
# Loading required libraries
library(ggplot2)
library(dplyr)

# Reading the CSV file
speed_data <- read.csv("m1_speeds.csv", check.names = TRUE)

# Checking actual column names
print(colnames(speed_data))

# Renaming 'Speed...(mph)' column to 'Speed' 
names(speed_data)[grepl("Speed", names(speed_data))] <- "Speed"

# Ensuring Direction is a factor
speed_data$Direction <- as.factor(speed_data$Direction)

# Structure of data
str(speed_data)

# Summary statistics overall
summary(speed_data$Speed)

# Summary by direction
summary_by_direction <- speed_data %>%
  group_by(Direction) %>%
  summarise(
    Mean_Speed = mean(Speed),
    Median_Speed = median(Speed),
    SD_Speed = sd(Speed),
    Count = n()
  )
print(summary_by_direction)

# Histogram of all speeds
ggplot(speed_data, aes(x = Speed)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Vehicle Speeds on M1", x = "Speed (mph)", y = "Frequency")

# Boxplot of speeds by direction
ggplot(speed_data, aes(x = Direction, y = Speed, fill = Direction)) +
  geom_boxplot() +
  labs(title = "Boxplot of Speeds by Direction", x = "Direction", y = "Speed (mph)") +
  theme_minimal()

# Performing t-test between directions
t_test_result <- t.test(Speed ~ Direction, data = speed_data)
print(t_test_result)

# Line plot for one sample location (e.g., M1 J5)
ggplot(filter(speed_data, Location == "M1 J5"), aes(x = Time, y = Speed, color = Direction)) +
  geom_line(aes(group = interaction(Date, Direction))) +
  labs(title = "Speed Over Time at M1 J5", x = "Time", y = "Speed (mph)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


