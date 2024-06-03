# Ghana cities intersect with landfill, then intersect with dhs gps data. 

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




# Define the directory where your KMZ files <are stored
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

st_crs(all_landfills_polygon)


#landfills_df <- read.csv("~/Documents/TUD/TUD 2024 S2/Empirical Research Task/Empirical_Research/R/3. analysis/all_landfills_polygon.csv")

# Load the city data from the gpkg file
city_data <- st_read("~/Documents/TUD/TUD 2024 S2/Empirical Research Task/Empirical_Research/input/world_2015_20000.gpkg")

#st_crs(landfills_df)

#all_landfills_polygon <- all_landfills_polygon |> 
#  st_transform(crs = "epsg:2136") 

city_data <- city_data |> 
  st_transform(crs = "epsg:2136") 

st_crs(city_data)
st_crs(all_landfills_polygon)

# intersections
intersections <- st_intersects(city_data, all_landfills_polygon)

# Filter cities intersecting with the landfills
intersecting_cities <- city_data[which(lengths(intersections) > 0), ]

# Print
print(intersecting_cities)
write.csv(intersecting_cities, "~/Documents/TUD/TUD 2024 S2/Empirical Research Task/Empirical_Research/R/2. wrangle/Ghana_landfill_city_intersects.csv")

### GPS data
dhs_gps_2008 <- st_read("~/Documents/TUD/TUD 2024 S2/Empirical Research Task/Empirical_Research/input/GHGE5AFL_2008/GHGE5AFL.shp")
print(dhs_gps_2008)

dhs_gps_2008 <- dhs_gps_2008 |> 
  st_transform(crs = "epsg:2136") |>
  mutate(area = st_area(geometry)) 

  st_crs(dhs_gps_2008)

dhs_gps_2014 <- st_read("~/Documents/TUD/TUD 2024 S2/Empirical Research Task/Empirical_Research/input/GHGE71FL_2014/GHGE71FL.shp")
print(dhs_gps_2014)

dhs_gps_2014 <- dhs_gps_2014 |> 
  st_transform(crs = "epsg:2136") |>
  mutate(area = st_area(geometry)) 

st_crs(dhs_gps_2014)

# Check CRS 

crs_intersecting_cities <- st_crs(intersecting_cities)
print(crs_intersecting_cities)

crs_accra_data <- st_crs(intersecting_cities)
print(crs_accra_data)

crs_gps08 <- st_crs(dhs_gps_2008)
print(crs_gps08)

crs_gps14 <- st_crs(dhs_gps_2014)
print(crs_gps14)
# All have the same CRS of epsg: 2136

# intersections of dhs gps data with the cities intersecting with the landfills for year 2008/ 2014 (which have been combined with city data and gps data). 
city_dhs_gps_2008<- st_intersects(intersecting_cities, dhs_gps_2008)
print(city_dhs_gps_2008)

city_dhs_gps_2014<- st_intersects(intersecting_cities, dhs_gps_2014)
print(city_dhs_gps_2014)
print(intersecting_cities["cty_name"])
# only 3 cities intersect with landfill data. 
#save city_dhs_gps_2014 as csv, for plotting city landfill (2014 extent)
write.csv(city_dhs_gps_2014, "~/Documents/TUD/TUD 2024 S2/Empirical Research Task/Empirical_Research/R/2. wrangle/city_dhs_gps_2014.csv")

# Filter cities: 1. Accra 
accra_data <- intersecting_cities %>%
  filter(cty_name == "Accra")
print(accra_data)

#Plotting for Accra#
ggplot() +
  geom_sf(data = accra_center) +
  theme_void() +
  ggspatial::annotation_scale() +
  ggtitle("Spatial Data for Accra")


accra_data <- intersecting_cities %>%
  filter(cty_name == "Accra")
print(accra_data)

# Extract the city center coordinates and convert to sf
accra_center <- accra_data %>%
  select(center_lon, center_lat) %>%
  st_as_sf(coords = c("center_lon", "center_lat"), crs = st_crs(accra_data))

# Plotting for Accra
ggplot() +
  geom_sf(data = accra_data) + 
  geom_sf(data = accra_center, color = "red", size = 3) +
  theme_void() +
  ggspatial::annotation_scale() +
  ggtitle("Spatial Data for Accra with City Center")


# Extract the city center coordinates and convert to sf
accra_center <- accra_data %>%
  select(center_lon, center_lat) %>%
  st_as_sf(coords = c("center_lon", "center_lat"), crs = 2136)

# Plotting for Accra
ggplot() +
  geom_sf(data = accra_data) + 
  geom_point(data = accra_center, aes(x = center_lon, y = center_lat), color = "red", size = 3) +
  theme_void() +
  ggspatial::annotation_scale() +
  ggtitle("Spatial Data for Accra with City Center")



  

