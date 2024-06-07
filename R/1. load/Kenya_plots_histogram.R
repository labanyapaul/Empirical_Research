
library(tidyverse)
library(ggplot2)
library(dplyr)
library(haven)
library(archive)
library(sf)
library(sp)
library(dplyr)
library(ggplot2)
library(units)
library(unglue)
library(here)

library(readr)
wealth_kenya <- read_csv("C:/Users/laban/Desktop/idhs_00003.csv")
View(wealth_kenya)

wealth_kenya  %>% select(WEALTHQHH,YEAR,COUNTRY)

wealth_kenyaclean <- wealth_kenya %>% filter(COUNTRY == 404)
head(wealth_kenyaclean)
print(wealth_kenyaclean)
ggplot(wealth_kenyaclean, aes(x = WEALTHQHH)) +
 
geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  scale_x_continuous(breaks = 1:5, labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")) +
  labs(title = "Wealth Index Quintiles for KENYA", x = "Wealth Quintile", y = "Count") +
  theme_minimal() +
  facet_wrap(~YEAR)
```

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
                  patterns = c("{landfill_name}_{year}_{polygon_no}",
                               "{landfill_name}_{year}",
                               "{landfill_name}_{year}"),
                  remove = FALSE)
}

polygons_sf <- polygons_sf %>%
  st_make_valid() %>%
  st_transform(crs = 4326)  # Using WGS 84 for visualization


# Summarize polygon data
Summarized_kenya <- polygons_sf %>%
  st_zm() %>%
  st_transform(crs = "ESRI:54009") %>%
  st_make_valid() %>%
  group_by(landfill_name, year) %>%
  summarize(area = sum(st_area(geometry)), .groups = 'drop')

#plotting the data

ggplot(Summarized_kenya, aes(x = year, y = area, fill = landfill_name)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Landfill Areas in Kenya by Year", x = "Year", y = "Area (m^2)", fill = "Landfill Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("landfill_areas_by_year.png")

#Scatter plot for Dandora Landfill

dandora_data <- Summarized_kenya %>% filter(landfill_name == "dandora")

ggplot(dandora_data, aes(x = year, y = area)) +
  geom_point() +
  
  labs(title = "Dandora Landfill Area Over Time", x = "Year", y = "Area (m^2)") +
  theme_minimal()

#save the plot

ggsave("dandora_landfill_area.png")
