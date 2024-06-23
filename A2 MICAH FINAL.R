install.packages("dplyr")
library(dplyr)
install.packages("tidyverse")
library(tidyverse)
install.packages("car")
library(car)
install.packages("lmtest")
library(lmtest)
setwd("C:\\Users\\HP\\Documents\\ns")
getwd()
data = read.csv("NSSO68 new.csv")
str(data)
westbengal_data <- data %>% 
  filter(state_1 == "WB")
relevant_columns <- c("foodtotal_q", "Meals_At_Home", "Possess_ration_card", "Age", "MPCE_URP", "MPCE_MRP")
westbengal_data <- westbengal_data %>% 
str(westbengal_data)
sum(is.na(westbengal_data$Meals_At_Home))
sum(is.na(westbengal_data$Possess_ration_card))
sum(is.na(westbengal_data$Age))
sum(is.na(westbengal_data$MPCE_URP))
sum(is.na(westbengal_data$MPCE_MRP))
complete_rows <- complete.cases(westbengal_data$foodtotal_q, westbengal_data$Possess_ration_card)
filtered_data <- westbengal_data[complete_rows, ]
model <- lm(foodtotal_q ~ Possess_ration_card, data = filtered_data)

imput_with_mean <- function(data,column) {
  data %>%
    mutate(across(all_of(columns), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
}
sum(is.na(data$foodtotal_q))
cleaned_data <- na.omit(data)
westbengal_data <- cleaned_data %>%
  filter(state_1 == "WB")
nrow(data) 
model <- lm(foodtotal_q ~ Meals_At_Home + Possess_ration_card + Age + MPCE_URP + MPCE_MRP, data = data)
summary(model)
