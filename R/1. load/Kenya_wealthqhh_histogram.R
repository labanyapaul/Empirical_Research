# Reading the DHS Wealth Index Quantile Household Questionnaire

# loading packages
library(haven)
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(here)
library(readr)

# Reading and importing the csv file
wealthindexqhh <- read.csv(here::here("output/idhs_00003.csv"))
print(wealthindexqhh)

# Selecting only the columns WEALTHQHH and YEAR, COUNTRY

wealthindexqhh <- data.frame(wealthindexqhh) %>% select(WEALTHQHH, YEAR, COUNTRY)
print(wealthindexqhh)

# Then, dropping the NA values

kewealthqhh_clean <- na.omit(wealthindexqhh)
print(kewealthqhh_clean)


# filter the data so I can just focus on Ghana data. 

kenya_wealthqhh <- kewealthqhh_clean  %>% filter(COUNTRY == 404)
print(kenya_wealthqhh)

# Remove the country column from the dataframe 

wealthqhh_kenya <- kenya_wealthqhh %>% select(-COUNTRY)
print(wealthqhh_kenya)

# Creating 1. histogram for the wealth index overall


ggplot(wealthqhh_kenya, aes(x = WEALTHQHH)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  scale_x_continuous(breaks = 1:5, labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")) +
  labs(title = "Wealth Index Quantile for Kenya", x = "Wealth Quintile", y = "Count") +
  theme_minimal()

ggsave("report/images/wealthqhh_overallkenya_hist.png")

# Creating 2. Histogram. Splitting the histogram into years (2008 and 2014)

ggplot(wealthqhh_kenya, aes(x = WEALTHQHH)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  scale_x_continuous(breaks = 1:5, labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")) +
  labs(title = "Wealth Index Quintiles for Kenya", x = "Wealth Quintile", y = "Count") +
  theme_minimal() +
  facet_wrap(~ YEAR)

# Saving the data
ggsave("report/images/wealthqhh_kenya_hist0814.png")


