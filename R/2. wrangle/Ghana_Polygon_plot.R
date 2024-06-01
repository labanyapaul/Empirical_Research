# Ghana scatter plot of polygons landfills. 

library(sf)
library(tidyverse)
library(maptiles)
library(archive)
library(dplyr)
library(unglue)
library(units)
library(readr)

library(knitr)
library(kableExtra)


# Define the directory where your KMZ files are stored
kmz_dir <- "~/Documents/TUD/TUD 2024 S2/Empirical Research Task/Empirical_Research/input/Ghana/Landfills"

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
  "Agbogbloshie" ~ "Agbogbloshie",
  "Pantang Borla" ~ "Pantang Borla",
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
                               "{landfill_name} {year}"),
                  remove = FALSE)
}


# Summarize polygon data
summarized_data <- polygons_sf %>%
  st_zm() %>%
  st_transform(crs = "ESRI:54009") %>%
  st_make_valid() %>%
  group_by(landfill_name, year) %>%
  summarize(area = sum(st_area(geometry)), .groups = 'drop')

# Display summarized data
print("Summarized data:")
print(summarized_data)

# Summarize polygons into one multipolygon and calculate the area for all landfills
all_landfills_polygon <- summarized_data %>%
  st_zm() %>%
  st_transform(crs = "ESRI:54009") %>%
  st_make_valid() %>%
  group_by(landfill_name, year) %>%
  summarize(geometry = st_union(geometry)) %>%
  ungroup() %>%
  st_transform(crs = "epsg:2136") %>%
  mutate(area = st_area(geometry))

# Ensure the area is in numeric format for plotting
all_landfills_polygon <- all_landfills_polygon %>%
  mutate(area_ha = as.numeric(area) / 10000) # Convert area to hectares

# Display the summarized polygon data with area for all landfills
print(all_landfills_polygon)

# save into csv file and (single data frame)
write.csv(all_landfills_polygon, "all_landfills_polygon.csv")


## Filter data for Agbogbloshie landfill
agbogbloshie_sf <- polygons_sf %>% filter(landfill_name == "Agbogbloshie")

# Plot the polygons for Agbogbloshie landfill over the years

ggplot() +
  geom_sf(data = agbogbloshie_sf) +
  facet_wrap(~year) +
  ggtitle("Agbogbloshie Landfill Polygons Over Years") +
  theme(legend.position = "bottom") +
  theme_void()

# Filter data for the 'Agbogbloshie' landfill
agbogbloshie_sf <- polygons_sf %>% 
  filter(landfill_name == "Agbogbloshie")

# Summarize polygons into one multipolygon
agbogbloshie_polygon <- agbogbloshie_sf %>%
  st_zm() %>%
  st_transform(crs = "ESRI:54009") %>%
  st_make_valid() %>%
  group_by(landfill_name, year) %>%
  summarize(geometry = st_union(geometry)) %>%
  ungroup() %>%
  st_transform(crs = "epsg:2136") %>%
  mutate(area = st_area(geometry))

print(agbogbloshie_polygon)

# Convert area to hectares for plotting
agbogbloshie_polygon <- agbogbloshie_polygon %>%
  mutate(area_ha = as.numeric(area) / 10000)

# Display the summarized polygon data with area
print(agbogbloshie_polygon)

# Plot the area of Agbogbloshie landfill over the years using a bar plot (hectares)
ggplot(data = agbogbloshie_polygon) +
  geom_col(aes(x = as.character(year), y = area_ha)) +
  coord_flip() +
  labs(y = "Area (ha)", x = "Year", title = "Agbogbloshie Landfill Area Over Time") + 
  theme_minimal()

## 


# Filter data for 2008
landfills_2008 <- all_landfills_polygon %>%
  filter(year == 2008)

# Calculate summary statistics for 2008
summary_stats_2008 <- landfills_2008 %>%
  summarize(
    Number_of_Observations = n(),
    Min = min(area_ha),
    Max = max(area_ha),
    Mean = mean(area_ha, na.rm = TRUE),
    Number_of_NAs = sum(is.na(area_ha))
  )

# Convert to a table
summary_table_2008 <- kable(summary_stats_2008, 
                            caption = "Summary Statistics of Landfills in 2008", 
                            align = "c") %>%
  kable_styling()

# Print the table for 2008
print(summary_table_2008)

# Filter data for 2014
landfills_2014 <- all_landfills_polygon %>%
  filter(year == 2014)

# Calculate summary statistics for 2014
summary_stats_2014 <- landfills_2014 %>%
  summarize(
    Number_of_Observations = n(),
    Min = min(area_ha),
    Max = max(area_ha),
    Mean = mean(area_ha, na.rm = TRUE),
    Number_of_NAs = sum(is.na(area_ha))
  )

# Convert to a table
summary_table_2014 <- kable(summary_stats_2014, 
                            caption = "Summary Statistics of Landfills in 2014", 
                            align = "c") %>%
  kable_styling()

# Print the table for 2014
print(summary_table_2014)


