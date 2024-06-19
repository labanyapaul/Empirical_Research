# Intrepretation of Ghana Wealth index in Control and Treatment group 

# Load the data
library(plm)
library(readr)


combined_treatmentcontrol <- read_csv("output/combined_treatmentcontrol.csv")
View(combined_treatmentcontrol)

combined_treatmentcontrol$Year <- as.factor(combined_treatmentcontrol$DHSYEAR)
combined_treatmentcontrol$Group <- as.factor(combined_treatmentcontrol$Group)

# Create dummy variable for Year
combined_treatmentcontrol$Year_2014 <- ifelse(combined_treatmentcontrol$DHSYEAR == 2014, 1, 0)

# Create dummy variable for Group
combined_treatmentcontrol$Treatment <- ifelse(combined_treatmentcontrol$Group == "Treatment", 1, 0)

#Interaction term
combined_treatmentcontrol$Year_Treatment <- combined_treatmentcontrol$Year_2014 * combined_treatmentcontrol$Treatment

# Run the regression
# Ensure the data is in the right format for plm
combined_treatmentcontrol <- pdata.frame(combined_treatmentcontrol, index = c("ADM1NAME", "DHSYEAR"))

# Run the fixed effects model
model <- plm(WEALTHQHH ~ Year_2014 + Treatment + Year_Treatment, data = combined_treatmentcontrol, model = "within")

# Summarize the results
summary(model)