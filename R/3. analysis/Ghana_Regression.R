

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


# Run the regression

# Run the fixed effects model. TWFE- Landfills and Year

model_GhanaTimeD <- feols(WEALTHQHH ~  Treatment |ADM1NAME+Year, data = combined_dataGhana)

# Summarize the results
summary(model_GhanaTimeD)

modelsummary::modelsummary(model_GhanaTimeD)

#Filter for just 2014 for the combined_treatmentcontrol data

combined_dataGhana_2014 <- combined_dataGhana[combined_dataGhana$Year_2014 == 1,]


#Run the regression for just 2014(to make it comparable with Kenya)


# Run the fixed effects model(FE-Landfills)

model_Ghana <- feols(WEALTHQHH ~ Treatment|ADM1NAME, data = combined_dataGhana_2014)

summary(model_Ghana)

modelsummary::modelsummary(model_Ghana)

#Regression table for model_Ghana and model_GhanaTimeD

library(etable)

Reg_tableGhana <- etable(model_Ghana)

Reg_tableGhanaTWFE<- etable(model_GhanaTimeD)

#save the regression table
write.table(Reg_tableGhana, "output/Reg_tableGhana.txt", row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")


write.table(Reg_tableGhanaTWFE, "output/Reg_tableGhanaTWFE.txt", row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")

