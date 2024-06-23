# Intrepretation of Ghana Wealth index in Control and Treatment group 

# Load the data
library(plm)
library(readr)


combined_dataGhana <- read_csv("output//combined_dataGhana.csv")
View(combined_dataGhana)

combined_dataGhana$Year <- as.factor(combined_dataGhana$DHSYEAR)
combined_dataGhana$Group <- as.factor(combined_dataGhana$Group)

# Create dummy variable for Year
combined_dataGhana$Year_2014 <- ifelse(combined_dataGhana$DHSYEAR == 2014, 1, 0)

# Create dummy variable for Group
combined_dataGhana$Treatment <- ifelse(combined_dataGhana$Group == "Treatment", 1, 0)

#Interaction term
combined_dataGhana$Year_Treatment <- combined_dataGhana$Year_2014 * combined_dataGhana$Treatment

# Run the regression
# Ensure the data is in the right format for plm
combined_dataGhana <- pdata.frame(combined_dataGhana, index = c("ADM1NAME", "DHSYEAR"))

# Run the fixed effects model
model_GhanaTimeD <- plm(WEALTHQHH ~ Year_2014 + Treatment + Year_Treatment, data = combined_dataGhana, model = "within")

# Summarize the results
summary(model_GhanaTimeD)


#Filter for just 2014 for the combined_treatmentcontrol data

combined_dataGhana_2014 <- combined_dataGhana[combined_dataGhana$Year_2014 == 1,]


#Run the regression for just 2014(to make it comparable with Kenya)

# Ensure the data is in the right format for plm
combined_dataGhana_2014<- pdata.frame(combined_dataGhana_2014, index = c("ADM1NAME"))

# Run the fixed effects model
model_Ghana <- plm(WEALTHQHH ~ Treatment, data = combined_dataGhana_2014, model = "within")

summary(model_Ghana)

#Regression table for model_Ghana and model_GhanaTimeD

library(stargazer)

stargazer(model_Ghana, model_GhanaTimeD, type = "text")

# Save the regression table

stargazer(model_Ghana, model_GhanaTimeD, type = "html", out = "output/model_Ghana.html")

