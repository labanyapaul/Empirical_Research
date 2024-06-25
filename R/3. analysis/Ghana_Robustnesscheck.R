#Part 1: Running fixed effects with control variable if the Head of the household is a Male


#loading the data from DHS Ipums (HHEADSEXHH)
library(dplyr)
library(readr)
library(fixest)
library(etable)
idhs_00007 <- read_csv("output/idhs_00007.csv", show_col_types = FALSE)
View(idhs_00007)

#Filter for ghana only, COUNTRY 288, and also filter for WEALTHQHH, DHSYEAR, DHSID, Group, HHEADSEXHH, SEX. 

idhs_00007_ghana <- idhs_00007 %>%
  filter(COUNTRY == 288) %>%
  select(YEAR, DHSID, HHEADSEXHH, SEX)

View(idhs_00007_ghana)

# Loading the combibed treatment control data for Ghana

library(readr)
combined_dataGhana <- read_csv("output/combined_dataGhana.csv", show_col_types = FALSE)
View(combined_dataGhana)

#merging bot datasets into 1 dataframe. 

Ghana_datacontrol <- merge(idhs_00007_ghana, combined_dataGhana, by = "DHSID")
View(Ghana_datacontrol) 

#stored in output
#write.table(HHead_ghana, "./HHead_ghana.csv", sep = ",", row.names = FALSE, col.names = !file.exists("./HHead_ghana.csv"), append = T)

# Convert to factors
Ghana_datacontrol$Year <- as.factor(Ghana_datacontrol$DHSYEAR)
Ghana_datacontrol$Group <- as.factor(Ghana_datacontrol$Group)

# Create dummy variables for Year and Group
Ghana_datacontrol$Year_2014 <- ifelse(Ghana_datacontrol$DHSYEAR == 2014, 1, 0)
Ghana_datacontrol$Treatment <- ifelse(Ghana_datacontrol$Group == "Treatment", 1, 0)


# Create dummy variables for HHEADSEXHH
Ghana_datacontrol$HHEADSEXHH_male <- ifelse(Ghana_datacontrol$HHEADSEXHH == 1, 1, 0)
Ghana_datacontrol$HHEADSEXHH_female <- ifelse(Ghana_datacontrol$HHEADSEXHH == 2, 1, 0)
Ghana_datacontrol$HHEADSEXHH_transgender <- ifelse(Ghana_datacontrol$HHEADSEXHH == 3, 1, 0)

# Run the Two way fixed effects model with control

model_GhanaTimeDcontrol <- feols(WEALTHQHH ~ Treatment + HHEADSEXHH_male|Year+ADM1NAME, data = Ghana_datacontrol)

summary(model_GhanaTimeDcontrol)

#Part 2: Running fixed effect(Fixed effect-Landfills) with control variable.
#We do the Part 2 to make it comparable with Kenya.(As in Kenya we do not have 2008 data)
#

#Filter the Ghana_datacontrol  for 2014 only

Ghana_datacontrol_2014 <- Ghana_datacontrol[Ghana_datacontrol$Year_2014 == 1,]



# Run the fixed effects model with control HHEADSEXHH_male

model_Ghanacontrol <- fixest::feols(WEALTHQHH ~ Treatment + HHEADSEXHH_male|ADM1NAME, data = Ghana_datacontrol_2014)

summary(model_Ghanacontrol)

#Regression tables
library(etable)

Reg_tableGhanacontrol <- etable(model_Ghanacontrol)
Reg_tableGhanaTWFEcontrol<- etable(model_GhanaTimeDcontrol)

#save the regression table

write.table(Reg_tableGhanacontrol, "output/Reg_tableGhanacontrol.txt", sep = ",")

write.table(Reg_tableGhanaTWFEcontrol, "output/Reg_tableGhanaTWFEcontrol.txt", sep = ",")