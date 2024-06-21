# Reading the DHS Wealth Index Quantile Household Questionnaire

# loading packages
library(haven)
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(here)

# Reading and importing the csv file
wealthindexqhh <- read.csv(here::here("output/idhs_00003.csv"))
print(wealthindexqhh)

# Selecting only the columns WEALTHQHH and YEAR, COUNTRY

wealthindexqhh <- data.frame(wealthindexqhh) %>% select(WEALTHQHH, YEAR, COUNTRY)
print(wealthindexqhh)

# Then, dropping the NA values

ghwealthqhh_clean <- na.omit(wealthindexqhh)
print(ghwealthqhh_clean)


# filter the data so I can just focus on Ghana data. 

ghana_wealthqhh <- ghwealthqhh_clean %>% filter(COUNTRY == 288)
print(ghana_wealthqhh)

# Remove the country column from the dataframe 

wealthqhh_ghana <- ghana_wealthqhh %>% select(-COUNTRY)
print(wealthqhh_ghana)

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
 
# Saving the data
 ggsave("report/images/wealthqhh_ghana_hist.png")

 ############################################
if (!here("output") |> file.exists()) {
  here("output") |> dir.create()
}
saveRDS(wealthqhh_ghana, "output/wealthqhh_ghana.rds")

