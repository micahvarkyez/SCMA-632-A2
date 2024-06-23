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
subset_data <- data %>% 
  filter(state_1 == "WD")
relevant_columns <- c("foodtotal_q", "Meals_At_Home", "Possess_ration_card", "Age", "MPCE_URP", "MPCE_MRP")
subset_data <- subset_data %>% 
  select(all_of(relevant_columns)) %>% 
  print(subset_data)
str(subseta_data)
sum(is.na(subset_data$Meals_At_Home))
sum(is.na(subset_data$Possess_ration_card))
sum(is.na(subset_data$Age))
sum(is.na(subset_data$MPCE_URP))
sum(is.na(subset_data$MPCE_MRP))
complete_rows <- complete.cases(subset_data$foodtotal_q, subset_data$Possess_ration_card)
filtered_data <- subset_data[complete_rows, ]
model <- lm(foodtotal_q ~ Possess_ration_card, data = filtered_data)

imput_with_mean <- function(data,column) {
  data %>%
    mutate(across(all_of(columns), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
}
sum(is.na(data$foodtotal_q))
cleaned_data <- na.omit(data)
westbengal_data <- cleaned_data %>%
  filter(state_1 == "WB")
nrow(westbengal_data) 
model <- lm(foodtotal_q ~ Meals_At_Home + Possess_ration_card + Age + MPCE_URP + MPCE_MRP, data = data)
summary(model)
