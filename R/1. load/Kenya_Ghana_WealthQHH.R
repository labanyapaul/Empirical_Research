# Reading the DHS Wealth Index Quantile Household Questionnaire

# loading packages
library(haven)
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)

# Reading and importing the csv file
wealthqhh <- read_csv("~/Documents/TUD/TUD 2024 S2/Empirical Research Task/Ghana/DHS/Ghana Kenya /Household WealthHQQ/idhs_00003.csv")
print(wealthqhh)

# Selecting only the columns WEALTHQHH and YEAR

wealthqhh <- data.frame(wealthqhh) %>% select(WEALTHQHH, YEAR, COUNTRY)
print(wealthqhh)

# Then, dropping the NA values

wealthqhh_clean <- na.omit(wealthqhh)
print(wealthqhh_clean)

# Export this file as CSV. for labanya 

write.csv(wealthqhh_clean, "wealthqhh_clean.csv", row.names = FALSE)

# filter the data so I can just focus on Ghana data. 

wealthqhh_ghana <- wealthqhh_clean %>% filter(COUNTRY == 288)
print(wealthqhh_ghana)

# Remove the country column frmo the dataframe 

wealthqhh_ghana <- wealthqhh_ghana %>% select(-COUNTRY)
print(wealthqhh_ghana)





