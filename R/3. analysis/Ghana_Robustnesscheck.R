# Robustness check - adding further control (Patriach)

#loading the data from DHS Ipums (HHEADSEXHH)
library(readr)
idhs_00007 <- read_csv("output/idhs_00007.csv")
View(idhs_00007)

#Filter for ghana only, COUNTRY 288, and also filter for WEALTHQHH, DHSYEAR, DHSID, Group, HHEADSEXHH, SEX. 

idhs_00007_ghana <- idhs_00007 %>% filter(COUNTRY == 288) %>% select(YEAR, DHSID, HHEADSEXHH, SEX)
View(idhs_00007_ghana)

# Loading the combibed treatment control data for Ghana

library(readr)
combined_treatmentcontrol <- read_csv("output/combined_treatmentcontrol.csv")
View(combined_treatmentcontrol)

#merging bot datasets into 1 dataframe. 

robust_ghana <- merge(idhs_00007_ghana, combined_treatmentcontrol, by = "DHSID")
View(robust_ghana)

#stored in output
write.table(robust_ghana, "./robust_ghana.csv", sep = ",", row.names = FALSE, col.names = !file.exists("./robust_ghana.csv"), append = T)
