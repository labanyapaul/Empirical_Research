# Reading the DHS Wealth Index Quantile Household Questionnaire

library(haven)
wealthqhh <- read_csv("~/Documents/TUD/TUD 2024 S2/Empirical Research Task/Ghana/DHS/Ghana Kenya /Household WealthHQQ/idhs_00003.csv")
print(wealthqhh)

# Selecting only the columns WEALTHQHH and YEAR

library(dplyr)
wealthqhh <- data.frame(wealthqhh) %>% select(WEALTHQHH, YEAR)
print(wealthqhh)
