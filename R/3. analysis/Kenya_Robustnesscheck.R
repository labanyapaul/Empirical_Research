#Part 1: Running fixed effects with control variable if the Head of the household is a Male

#loading the data from DHS Ipums (HHEADSEXHH)
library(dplyr)
library(readr)
library(modelsummary)
idhs_00007 <- read_csv("output/idhs_00007.csv", show_col_types = FALSE)
View(idhs_00007)

#Filter for Kenya only, COUNTRY 404, and also filter for WEALTHQHH, DHSYEAR, DHSID, Group, HHEADSEXHH, SEX. 

idhs_00007_kenya <- idhs_00007 %>%
  filter(COUNTRY == 404) %>%
  select(YEAR, DHSID, HHEADSEXHH, SEX)

View(idhs_00007_kenya)

# Loading the combined data for Kenya (output/combined_dataKenya.csv)

library(readr)
combined_dataGhana <- read_csv("output/combined_dataKenya.csv", show_col_types = FALSE)
View(combined_dataKenya)

#merging bot datasets into 1 dataframe. 

Kenya_datacontrol <- merge(idhs_00007_kenya, combined_dataGhana, by = "DHSID")
View(Kenya_datacontrol) 

#Convert to factors
Kenya_datacontrol$Year <- as.factor(Kenya_datacontrol$DHSYEAR)
Kenya_datacontrol$Group <- as.factor(Kenya_datacontrol$Group)

# Create dummy variables for Group
Kenya_datacontrol$Treatment <- ifelse(Kenya_datacontrol$Group == "Treatment", 1, 0)

# Create dummy variables for HHEADSEXHH is male
Kenya_datacontrol$HHEADSEXHH_male <- ifelse(Kenya_datacontrol$HHEADSEXHH == 1, 1, 0)
Kenya_datacontrol$HHEADSEXHH_female <- ifelse(Kenya_datacontrol$HHEADSEXHH == 2, 1, 0)


# Run the fixed effects model

library(fixest)
model_Kenyacontrol <-fixest::feols(WEALTHQHH ~ Treatment+ HHEADSEXHH_male|ADM1NAME,data = Kenya_datacontrol)

summary(model_Kenyacontrol)

modelsummary::modelsummary(model_Kenyacontrol)
#Part 2:Running fixed effects with control variables but here we are adding,
#The dummy if the household head is a female, we are using HHEADSEXHH_male as the reference category.
#To see the effect relative to when household head is a male.


#Dummy for household head female
Kenya_datacontrolFemale <- Kenya_datacontrol %>% 
  mutate(Kenya_datacontrol$HHEADSEXHH_female <- ifelse(Kenya_datacontrol$HHEADSEXHH == 2, 1, 0)
)
  
# Run the fixed effects model

model_KenyacontrolFemale <-fixest::feols(WEALTHQHH ~ Treatment+ HHEADSEXHH_female|ADM1NAME,data = Kenya_datacontrolFemale, )

summary(model_KenyacontrolFemale)

modelsummary::modelsummary(model_KenyacontrolFemale)

#Save the Regression table for model_Kenyacontrol and model_KenyacontrolFemale using etable and save the table in output folder

library(etable)

# Generate the regression table

Reg_tableKenyacontrol <- etable(model_Kenyacontrol)


Reg_tableKenyacontrolFemale <- etable(model_KenyacontrolFemale)

#save the regression table

write.table(Reg_tableKenyacontrol, "output/Reg_tableKenyacontrol.txt",  row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
write.table(Reg_tableKenyacontrolFemale, "output/Reg_tableKenyacontrolFemale.txt", , row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")