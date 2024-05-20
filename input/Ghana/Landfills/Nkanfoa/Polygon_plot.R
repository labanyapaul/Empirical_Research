# Reading the DHS Wealth Index Quantile Household Questionnaire

library(haven)
wealthqhh <- read_csv("~/Documents/TUD/TUD 2024 S2/Empirical Research Task/Ghana/DHS/Ghana Kenya /Household WealthHQQ/idhs_00003.csv")
print(wealthqhh)

# Selecting only the columns WEALTHQHH and YEAR

library(dplyr)
wealthqhh <- data.frame(wealthqhh) %>% select(WEALTHQHH, YEAR)
print(wealthqhh)



library(sf)
#> Linking to GEOS 3.6.1, GDAL 2.1.3, PROJ 4.9.3

Nkanfoa_07 <- st_read("/Users/nt/Documents/TUD/TUD 2024 S2/Empirical Research Task/Ghana/Polygon Google Earth/Landfills 2007 - 2015/Nkanfoa 07-11-15/Nkanfoa 10.28.2007.kml")
#> Reading layer `Nkanfoa 10.28.2007' from data source `/Users/nt/Documents/TUD/TUD 2024 S2/Empirical Research Task/Ghana/Polygon Google Earth/Nkanfoa 10.28.2007.kml' using driver `LIBKML'
#> Simple feature collection with 1 feature and 1 field
#> geometry type:  POLYGON
#> dimension:      XY
#> bbox:           xmin: -1.625 ymin: 5.625 xmax: -1.625 ymax: 5.625
#> CRS:            NA
plot(Nkanfoa_07)