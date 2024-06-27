library(readr)
combined_dataKibera <- here::here("output//combined_dataKibera.csv")
combined_dataKibera <- read_csv("output/combined_dataDandora.csv")
View(combined_dataKibera)


combined_dataDandora <- here::here("output//combined_dataDandora.csv")
read_csv(combined_dataDandora)
combined_dataDandora <- read_csv("output//combined_dataDandora.csv")
View(combined_dataDandora)

library(dplyr)
# Update the ADM1NAME column in combined_dataDandora
combined_dataDandora <- combined_dataDandora %>%
  mutate(ADM1NAME = ifelse(ADM1NAME == "Nairobi", "NairobiD", ADM1NAME))



combined_dataMombasa <- here::here("output//combined_dataMombasa.csv")
read_csv(combined_dataMombasa)
combined_dataMombasa <- read_csv("output/combined_dataMombasa.csv")
View(combined_dataMombasa)

combined_dataKenya <- rbind(combined_dataKibera, combined_dataDandora, combined_dataMombasa)

#Change the column name ADM1NAME to LANDFILLS

combined_dataKenya <- combined_dataKenya %>%
  rename(LANDFILLS = ADM1NAME)
#Save combined_dataKenya as csv file

write_csv(combined_dataKenya, "output/combined_dataKenya.csv")
# View the combined dataframe
View(combined_dataKenya)


# Create dummy variable for Group
combined_dataKenya$Treatment <- ifelse(combined_dataKenya$Group == "Treatment", 1, 0)



# Run the fixed effects model
library(fixest)
model_kenya <- fixest::feols(WEALTHQHH ~ Treatment| LANDFILLS, data = combined_dataKenya)
# Summarize the results
summary(model_kenya)



# Load necessary libraries
library(fixest)
library(here)
library(modelsummary)

# Fit the model
model_kenya <- fixest::feols(WEALTHQHH ~ Treatment | LANDFILLS, data = combined_dataKenya)


# Generate the regression table


modelsummary(model_kenya)
Reg_tableKenya <- modelsummary(model_kenya, output = "modelsummary_list")

#Save rds file
saveRDS(Reg_tableKenya, "output/Reg_tableKenya.rds")


