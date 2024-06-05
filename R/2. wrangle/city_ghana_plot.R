#Plotting map for cities. 
library(sf)
library(tidyverse)
library(maptiles)
library(archive)
library(dplyr)
library(unglue)
library(units)
library(readr)
library(ggplot2)
library(ggspatial)

library(knitr)
library(kableExtra)


library(readr)
Ghana_landfill_city_intersects <- read_csv("R/2. wrangle/Ghana_landfill_city_intersects.csv")
View(Ghana_landfill_city_intersects)

desired_crs <- 2136

# Filter cities: 1. Accra 
accra_data <- intersecting_cities %>%
  filter(cty_name == "Accra")
print(accra_data)


# Calculate centroid of Accra polygon
accra_center <- accra_data %>%
  st_centroid() 

# Plot Accra data with city center point
ggplot() +
  geom_sf(data = accra_data) + 
  geom_sf(data = accra_center, color = "black", size = 3) + 
  theme_void() +
  ggspatial::annotation_scale() +
  ggtitle("Spatial Data for Accra with City Center Point")

