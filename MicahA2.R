# Load necessary libraries
library(dplyr)
library(readr)

# Load the data
setwd("C:\\Users\\Dell\\Desktop\\SCMA 24")
ipl_data <- read_csv("IPL_ball_by_ball_updated till 2024 (1).csv")

# Filter data for R. Ashwin's performance
ashwin_data <- ipl_data %>% filter(Bowler == "R Ashwin")

# Convert 'Date' to Date format and extract year
ashwin_data$Date <- as.Date(ashwin_data$Date, format="%d-%m-%Y")
ashwin_data$Year <- format(ashwin_data$Date, "%Y")

# Filter data for the last three seasons (2022, 2023, 2024)
last_three_years <- ashwin_data %>% filter(Season %in% c("2022", "2023", "2024"))

# Function to calculate performance metrics
calculate_metrics <- function(data) {
  wickets <- nrow(data %>% filter(wicket_confirmation == 1))
  runs_conceded <- sum(data$runs_scored, na.rm = TRUE)
  balls_bowled <- nrow(data)
  overs_bowled <- balls_bowled / 6
  economy_rate <- ifelse(overs_bowled != 0, runs_conceded / overs_bowled, 0)
  
  return(data.frame(Wickets = wickets, RunsConceded = runs_conceded, OversBowled = overs_bowled, EconomyRate = economy_rate))
}

# Calculate performance metrics for each season
metrics <- last_three_years %>% group_by(Season) %>% do(calculate_metrics(.))

# Add salary data for R. Ashwin (example values, replace with actual salary data)
salary_data <- data.frame(
  Season = c("2022", "2023", "2024"),
  Salary = c(76000000, 77000000, 78000000)  # in INR
)

# Merge performance metrics with salary data
analysis_data <- merge(metrics, salary_data, by = "Season")

# Perform regression analysis
model <- lm(Salary ~ Wickets + EconomyRate, data = analysis_data)

# Print summary of the regression model
summary(model)

# Plot the relationships
library(ggplot2)

# Wickets vs Salary
ggplot(analysis_data, aes(x = Wickets, y = Salary)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Relationship between Wickets and Salary", x = "Wickets", y = "Salary (INR)")

# Economy Rate vs Salary
ggplot(analysis_data, aes(x = EconomyRate, y = Salary)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Relationship between Economy Rate and Salary", x = "Economy Rate", y = "Salary (INR)")