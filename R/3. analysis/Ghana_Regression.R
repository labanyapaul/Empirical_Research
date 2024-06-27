

# Load the data
library(fixest)
library(readr)
library(modelsummary)

combined_dataGhana <- read_csv("output//combined_dataGhana.csv")
View(combined_dataGhana)

combined_dataGhana$Year <- as.factor(combined_dataGhana$DHSYEAR)
combined_dataGhana$Group <- as.factor(combined_dataGhana$Group)

# Create dummy variable for Year
combined_dataGhana$Year_2014 <- ifelse(combined_dataGhana$DHSYEAR == 2014, 1, 0)

# Create dummy variable for Group
combined_dataGhana$Treatment <- ifelse(combined_dataGhana$Group == "Treatment", 1, 0)

#Change the column name ADM1NAME to LANDFILLS

combined_dataGhana <- combined_dataGhana %>%
  rename(LANDFILLS = ADM1NAME)

# Run the regression

# Run the fixed effects model. TWFE- Landfills and Year

model_GhanaTimeD <- feols(WEALTHQHH ~  Treatment |LANDFILLS+Year, data = combined_dataGhana)

# Summarize the results
summary(model_GhanaTimeD)

modelsummary::modelsummary(model_GhanaTimeD)
Reg_tableGhanaTWFE <- modelsummary(model_GhanaTimeD, output = "modelsummary_list")

#Save rds file
saveRDS(Reg_tableGhanaTWFE, "output/Reg_tableGhanaTWFE.rds")



#Filter for just 2014 for the combined_treatmentcontrol data

combined_dataGhana_2014 <- combined_dataGhana[combined_dataGhana$Year_2014 == 1,]


#Run the regression for just 2014(to make it comparable with Kenya)


# Run the fixed effects model(FE-Landfills)

model_Ghana <- feols(WEALTHQHH ~ Treatment|LANDFILLS, data = combined_dataGhana_2014)

summary(model_Ghana)

modelsummary::modelsummary(model_Ghana)
Reg_tableGhana <- modelsummary(model_Ghana, output = "modelsummary_list")

#Save rds file
saveRDS(Reg_tableGhana, "output/Reg_tableGhana.rds")

