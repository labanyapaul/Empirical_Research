library(haven)
data <- read_dta("~/Documents/TUD/TUD 2024 S2/Empirical Research Task/Ghana/DHS/2008/GH_2008_DHS_05162024_1843_212215/GHKR5ADT/GHKR5AFL.DTA")
print(data)

library(sf)
#> Linking to GEOS 3.6.1, GDAL 2.1.3, PROJ 4.9.3

Nkanfoa_07 <- st_read("/Users/nt/Documents/TUD/TUD 2024 S2/Empirical Research Task/Ghana/Polygon Google Earth/Nkanfoa 10.28.2007.kml")
#> Reading layer `Nkanfoa 10.28.2007' from data source `/Users/nt/Documents/TUD/TUD 2024 S2/Empirical Research Task/Ghana/Polygon Google Earth/Nkanfoa 10.28.2007.kml' using driver `LIBKML'
#> Simple feature collection with 1 feature and 1 field
#> geometry type:  POLYGON
#> dimension:      XY
#> bbox:           xmin: -1.625 ymin: 5.625 xmax: -1.625 ymax: 5.625
#> CRS:            NA
plot(Nkanfoa_07)