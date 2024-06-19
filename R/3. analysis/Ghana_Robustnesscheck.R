# Robustness check - adding further controls 

#loading the data from DHS Ipums (HHEADSEXHH)
library(dplyr)
library(readr)
idhs_00007 <- read_csv("output/idhs_00007.csv", show_col_types = FALSE)
View(idhs_00007)

#Filter for ghana only, COUNTRY 288, and also filter for WEALTHQHH, DHSYEAR, DHSID, Group, HHEADSEXHH, SEX. 

idhs_00007_ghana <- idhs_00007 %>%
  filter(COUNTRY == 288) %>%
  select(YEAR, DHSID, HHEADSEXHH, SEX)

View(idhs_00007_ghana)

# Loading the combibed treatment control data for Ghana

library(readr)
combined_treatmentcontrol <- read_csv("output/combined_treatmentcontrol.csv", show_col_types = FALSE)
View(combined_treatmentcontrol)

#merging bot datasets into 1 dataframe. 

HHead_ghana <- merge(idhs_00007_ghana, combined_treatmentcontrol, by = "DHSID")
View(HHead_ghana) 

#stored in output
#write.table(HHead_ghana, "./HHead_ghana.csv", sep = ",", row.names = FALSE, col.names = !file.exists("./HHead_ghana.csv"), append = T)

################################################################################
#
################################################################################
