---
title: "kenya scatter"
output: html_document
date: "2024-06-03"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
library(tidyverse)
library(ggplot2)
library(dplyr)
library(haven)

library(sf)
library(tidyverse)
library(maptiles)
library(archive)
library(dplyr)
library(unglue)

# Define the directory where your KMZ files are stored
kmz_dir <- "C://Users//laban//Downloads//Empirical_Research//input//Kenya//Landfills"

# Ensure the directory exists
if (!dir.exists(kmz_dir)) {
  stop("The directory does not exist. Please check the path.")
}

# List all KMZ files in the directory
kmz_files <- list.files(path = kmz_dir, pattern = "\\.kmz$", full.names = TRUE)
if (length(kmz_files) == 0) {
  stop("No KMZ files found in the specified directory. Please check the file extensions and path.")
}

# Print KMZ files to verify
print("KMZ files found:")
print(kmz_files)

# Function to unzip KMZ to KML and read as sf object
read_kmz_to_sf <- function(kmz_file) {
  kml_path <- tempfile()
  archive_extract(archive = kmz_file, dir = kml_path)
  kml_files <- list.files(kml_path, pattern = "\\.kml$", full.names = TRUE)
  sf_object <- st_read(kml_files[1], quiet = FALSE) # Assuming the first KML file is the one you need
  sf_object$filename <- basename(kmz_file)  # Add filename as a new column
  return(sf_object)
}

# Read all KMZ files into sf objects
sf_list <- lapply(kmz_files, read_kmz_to_sf)

# Combine all sf objects into a single sf data frame
combined_sf <- bind_rows(sf_list)

# Extract year, month, and landfill name from filenames
combined_sf <- combined_sf %>%
  mutate(
    year = str_extract(filename, "\\d{4}"),
    month = str_extract(filename, "(?<=\\d{4})_(\\w+)"),
    landfill_name = str_remove(filename, "_\\d{4}.*")
  )

# Clean up landfill names
combined_sf$landfill_name <- case_match(
  combined_sf$landfill_name,
  "Dandora" ~ "Dandora",
  "Kibera" ~ "Kibera",
  # Add more landfill name corrections as needed
  .default = combined_sf$landfill_name
)

# Convert year to numeric
combined_sf$year <- as.numeric(combined_sf$year)

print("Combined sf data:")


# Inspect the geometry types
geometry_types <- st_geometry_type(combined_sf)
print("Geometry types in combined_sf:")
print(geometry_types)

# Split into points and polygons based on actual geometry types
unique_geometry_types <- unique(geometry_types)
print("Unique geometry types:")
print(unique_geometry_types)

points_sf <- combined_sf %>% filter(st_geometry_type(geometry) %in% c("POINT", "MULTIPOINT"))
polygons_sf <- combined_sf %>% filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON"))

# Check if points and polygons data frames have observations
print("Points sf:")
print(points_sf)

print("Polygons sf:")
print(polygons_sf)

# If you have a 'Name' column in polygons_sf, unglue and clean polygon data
if ("Name" %in% colnames(polygons_sf)) {
  polygons_sf <- polygons_sf %>%
    unglue_unnest(Name,
                  patterns = c("{landfill_name}_{month}_{year}_{polygon_no}",
                               "{landfill_name}_{month}_{year}",
                               "{landfill_name} {month}_{year}"),
                  remove = FALSE)
}

polygons_sf <- polygons_sf %>%
  st_make_valid() %>%
  st_transform(crs = 4326)  # Using WGS 84 for visualization

# Filter data for a specific landfill (e.g., Dandora)
dandora_sf <- polygons_sf %>% filter(landfill_name == "dandora")

# Plot the polygons for Dandora landfill over the years


ggplot() +
geom_sf(data = dandora_sf) +
  facet_wrap(~year) +
    ggtitle("Dandora Landfill Polygons Over Years") +
  theme(legend.position = "bottom") +
  theme_void() 

```

```{r}
# Summarize polygon data
Summarized_kenya <- polygons_sf %>%
  st_zm() %>%
  st_transform(crs = "ESRI:54009") %>%
  st_make_valid() %>%
  group_by(landfill_name, year, month) %>%
  summarize(area = sum(st_area(geometry)), .groups = 'drop')

# Display summarized data
print("Summarized_kenya")

```

```{r}
# Load necessary libraries
library(sf)
library(dplyr)

gpkg_file <- "C:\\Users\\laban\\Downloads\\Empirical_Research\\input\\world_2015_20000.gpkg"

# Read the GPKG file
cities_data <- st_read(gpkg_file)
 

# Check the CRS of the city data
cities_crs <- st_crs(cities_data)
print(cities_crs)





# Filter for the cities of interest
cities_of_interest <- c("Dandora", "Kisumu", "Nakaru", "Mombasa", "Nairobi")
Cities_kenya <- cities_data %>%
  filter(cty_name %in% cities_of_interest)

# Check the CRS of your summarized_kenya data
landfills_crs <- st_crs(Summarized_kenya)
print(landfills_crs)

# Set the CRS of cities_data to match summarized_kenya
st_crs(cities_data) <- landfills_crs

cities_crs <- st_crs(cities_data)
# Ensure the city data and landfill data have the same CRS
if (!st_crs(Cities_kenya) == st_crs(Summarized_kenya)) {
  Cities_kenya <- st_transform(Cities_kenya, st_crs(Summarized_kenya))
}

# Ensure the geometries are valid
Cities_kenya <- st_make_valid(Cities_kenya)
Summarized_kenya <- st_make_valid(Summarized_kenya)

# Use st_intersects to find intersections
intersections <- st_intersects(Cities_kenya, Summarized_kenya)

# Print the results
print(intersections)

# Extract the intersecting cities and landfills
intersecting_data <- Cities_kenya[unlist(intersections), ]

# Print the intersecting data
print(intersecting_data)



```


```{r}
# Filter data for a specific landfill (e.g., Kisumu)
Kisumu_sf <- polygons_sf %>% filter(landfill_name == "kisumu")

# Plot the polygons for Kisumu landfill over the years


ggplot() +
geom_sf(data = dandora_sf) +
  facet_wrap(~year) +
    ggtitle("Kisumu Landfill Polygons Over Years") +
  theme(legend.position = "bottom") +
  theme_void() 

```
## R Markdown

```{r}


library(sf)
library(dplyr)
library(ggplot2)

# Load GPS data shapefiles for 2008 and 2014
gps_data_2008 <- st_read("C:\\Users\\laban\\Downloads\\Empirical_Research\\input\\Kenya\\GPS 2008")
gps_data_2014 <- st_read("C:\\Users\\laban\\Downloads\\Empirical_Research\\input\\Kenya\\GPS 2014")

Kisumu_sf <- Kisumu_sf %>%
  ms_simplify() %>%
  st_make_valid()

# Ensure GPS data is valid
gps_data_2008 <- st_make_valid(gps_data_2008)
gps_data_2014 <- st_make_valid(gps_data_2014)

# Perform spatial join with Dandora polygons for each year
Kisumu_2008 <- st_join(gps_data_2008, Kisumu_sf, join = st_intersects)
Kisumu_2014 <- st_join(gps_data_2014, Kisumu_sf, join = st_intersects)




