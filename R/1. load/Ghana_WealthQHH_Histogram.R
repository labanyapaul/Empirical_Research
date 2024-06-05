# Reading the DHS Wealth Index Quantile Household Questionnaire

# loading packages
library(haven)
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(here)

# Reading and importing the csv file
wealthqhh <- read_csv("~/Documents/TUD/TUD 2024 S2/Empirical Research Task/Ghana/DHS/Ghana Kenya /Household WealthHQQ/idhs_00003.csv")
print(wealthqhh)

# Selecting only the columns WEALTHQHH and YEAR

wealthqhh <- data.frame(wealthqhh) %>% select(WEALTHQHH, YEAR, COUNTRY)
print(wealthqhh)

# Then, dropping the NA values

wealthqhh_clean <- na.omit(wealthqhh)
print(wealthqhh_clean)


# filter the data so I can just focus on Ghana data. 

wealthqhh_ghana <- wealthqhh_clean %>% filter(COUNTRY == 288)
print(wealthqhh_ghana)

# Remove the country column from the dataframe 

wealthqhh_ghana <- wealthqhh_ghana %>% select(-COUNTRY)
print(wealthqhh_ghana)
# save wealthqhh_ghana as a file to use in future scripts. 
write_csv(wealthqhh_ghana, "output/wealthqhh_ghana.csv")


# Creating 1. histogram for the wealth index overall


ggplot(wealthqhh_ghana, aes(x = WEALTHQHH)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  scale_x_continuous(breaks = 1:5, labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")) +
  labs(title = "Wealth Index Quantile for Ghana", x = "Wealth Quintile", y = "Count") +
  theme_minimal()
  
# Creating 2. Histogram. Splitting the histogram into years (2008 and 2014)

 ggplot(wealthqhh_ghana, aes(x = WEALTHQHH)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  scale_x_continuous(breaks = 1:5, labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")) +
  labs(title = "Wealth Index Quintiles for Ghana", x = "Wealth Quintile", y = "Count") +
  theme_minimal() +
  facet_wrap(~ YEAR)

if (!here("output") |> file.exists()) {
  here("output") |> dir.create()
}
saveRDS(wealthqhh_ghana, "output/wealthqhh_ghana.rds")

