library(readr)
combined_dataKibera <- here::here("output//combined_dataKibera.csv")
combined_dataKibera <- read_csv("output/combined_dataDandora.csv")
View(combined_dataKibera)


combined_dataDandora <- here::here("output//combined_dataDandora.csv")
read_csv(combined_dataDandora)
combined_dataDandora <- read_csv("output//combined_dataDandora.csv")
View(combined_dataDandora)

# Update the ADM1NAME column in combined_dataDandora
combined_dataDandora <- combined_dataDandora %>%
  mutate(ADM1NAME = ifelse(ADM1NAME == "Nairobi", "NairobiD", ADM1NAME))



combined_dataMombasa <- here::here("output//combined_dataMombasa.csv")
read_csv(combined_dataMombasa)
combined_dataMombasa <- read_csv("output/combined_dataMombasa.csv")
View(combined_dataMombasa)

combined_dataKenya <- rbind(combined_dataKibera, combined_dataDandora, combined_dataMombasa)

#Save combined_dataKenya as csv file

write_csv(combined_dataKenya, "output/combined_dataKenya.csv")
# View the combined dataframe
View(combined_dataKenya)

library(plm)

# Create dummy variable for Group
combined_dataKenya$Treatment <- ifelse(combined_dataKenya$Group == "Treatment", 1, 0)

# Ensure the data is in the right format for plm
combined_dataKenya <- pdata.frame(combined_dataKenya, index = c("ADM1NAME"))

# Run the fixed effects model
model_kenya <- plm(WEALTHQHH~ Treatment, data = combined_dataKenya, model = "within")

# Summarize the results
summary(model_kenya)

#Regression table for model_kenya

library(stargazer)
stargazer(model_kenya, type = "text")

# Save the regression table
stargazer(model_kenya, type = "html", out = "output/model_kenya.html")
