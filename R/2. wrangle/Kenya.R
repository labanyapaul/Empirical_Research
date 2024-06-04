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
# Summarize polygon data
Summarized_kenya <- polygons_sf %>%
  st_zm() %>%
  st_transform(crs = "ESRI:54009") %>%
  st_make_valid() %>%
  group_by(landfill_name, year, month) %>%
  summarize(area = sum(st_area(geometry)), .groups = 'drop')

# Display summarized data
print("Summarized_kenya")

# Summarize polygons into one multipolygon and calculate the area for all landfills
all_landfills_polygon <- Summarized_kenya %>%
  st_zm() %>%
  st_transform(crs = "ESRI:54009") %>%
  st_make_valid() %>%
  group_by(landfill_name, year) %>%
  summarize(geometry = st_union(geometry)) %>%
  ungroup() %>%
  st_transform(crs = "epsg:2136") %>%
  mutate(area = st_area(geometry))


# Display the area of all landfills over the years
print(all_landfills_polygon)


#Loading the city data


gpkg_file <- "C:\\Users\\laban\\Downloads\\Empirical_Research\\input\\world_2015_20000.gpkg"

# Read the GPKG file
cities_data <- st_read(gpkg_file)

#
cities_data <- cities_data |>
  st_transform(crs = "epsg:2136") 

st_crs(cities_data) |>
st_crs(all_landfills_polygon)

#intersection
intersections <- st_intersects(cities_data, all_landfills_polygon)

#filter cities intersecting with the landfills
intersecting_cities_kenya <- cities_data[which(lengths(intersections) > 0), ]

#print
print(intersecting_cities_kenya)

### GPS data
gps_Kenya2008 <- st_read("C:\\Users\\laban\\Downloads\\Empirical_Research\\input\\Kenya\\GPS 2008")
print(gps_Kenya2008)

gps_Kenya2008 <- gps_Kenya2008 |> 
  st_transform(crs = "epsg:2136") |>
  mutate(area = st_area(geometry)) 

st_crs(gps_Kenya2008)

gps_Kenya2014 <- st_read("C:\\Users\\laban\\Downloads\\Empirical_Research\\input\\Kenya\\GPS 2014")
print(gps_Kenya2014)

gps_Kenya2014 <-gps_Kenya2014 |> 
  st_transform(crs = "epsg:2136") |>
  mutate(area = st_area(geometry)) 

st_crs(gps_Kenya2014)

#Check CRS

crs_intersecting_cities <- st_crs(intersecting_cities_kenya)
print(crs_intersecting_cities)



crs_gpsK08 <- st_crs(gps_Kenya2008)
print(crs_gpsK08)

crs_gpsK14 <- st_crs(gps_Kenya2014)
print(crs_gpsK14)

# intersections of dhs gps data with the cities intersecting with the landfills for year 2008/ 2014 (which have been combined with city data and gps data). 
Kenya_dhs_gps_2008<- st_intersects(intersecting_cities_kenya, gps_Kenya2008)
print(Kenya_dhs_gps_2008)

Kenya_dhs_gps_2014<- st_intersects(intersecting_cities_kenya, gps_Kenya2014)
print(Kenya_dhs_gps_2014)
print(intersecting_cities_kenya["cty_name"])

# Filter cities: 1. Nairobi 
Nairobi_data <- intersecting_cities_kenya %>%
filter(cty_name == "Nairobi")
print(Nairobi_data)


# Calculate centroid of Nairobi polygon
Nairobi_center <- Nairobi_data %>%
  st_centroid() 

# Plot Nairobi data with city center point
ggplot() +
  geom_sf(data = Nairobi_data) + 
  geom_sf(data = Nairobi_center, color = "black", size = 3) + 
  theme_void() +
  ggspatial::annotation_scale() +
  ggtitle("Spatial Data for Nairobi with City Center Point")

# Filter cities: 2. Mombasa
Mombasa_data <- intersecting_cities_kenya %>%
filter(cty_name == "Mombasa")
print(Mombasa_data)

# Calculate centroid of Mombasa polygon
Mombasa_center <- Mombasa_data %>%
  st_centroid()

# Plot Mombasa data with city center point
ggplot() +
  geom_sf(data = Mombasa_data) + 
  geom_sf(data = Mombasa_center, color = "black", size = 3) + 
  theme_void() +
  ggspatial::annotation_scale() +
  ggtitle("Spatial Data for Mombasa with City Center Point")



# Filter cities: 3. Kisumu
Kisumu_data <- intersecting_cities_kenya %>%
filter(cty_name == "Kisumu")
print(Kisumu_data)

# Calculate centroid of Kisumu polygon

Kisumu_center <- Kisumu_data %>%
  st_centroid()

# Plot Kisumu data with city center point

ggplot() +
  geom_sf(data = Kisumu_data) + 
  geom_sf(data = Kisumu_center, color = "black", size = 3) + 
  theme_void() +
  ggspatial::annotation_scale() +
  ggtitle("Spatial Data for Kisumu with City Center Point")

# Add dandora polygon into the map (2014 extent)

dandora_2014 <- dandora_sf %>%
  filter(year == 2014)

ggplot() +
  geom_sf(data = Nairobi_data) + 
  geom_sf(data = dandora_2014, fill = "blue", alpha = 0.5) + 
  geom_sf(data = Nairobi_center, color = "black", size = 3) + 
  theme_void() +
  ggspatial::annotation_scale() +
  ggtitle("Spatial Data for Nairobi with City Center Point and Dandora Polygon (2014)")

# Add dandora polygon into the map (2008 extent)

dandora_2008 <- dandora_sf %>%
  filter(year == 2008)
ggplot() +
  geom_sf(data = Nairobi_data) + 
  geom_sf(data = dandora_2008, fill = "blue", alpha = 0.5) + 
  geom_sf(data = Nairobi_center, color = "black", size = 3) + 
  theme_void() +
  ggspatial::annotation_scale() +
  ggtitle("Spatial Data for Nairobi with City Center Point and Dandora Polygon (2008)")

# Adding the dhs_gps_2014 data to the map

gps_Kenya_Nairobi<- gps_Kenya2014%>%
  filter(ADM1NAME == "Nairobi")

# dropping R (outliers too far)
gps_Kenya_Nairobi <- gps_Kenya_Nairobi%>%
  filter(URBAN_RURA == "U")

# Create the plot
ggplot() +
  geom_sf(data = Nairobi_data) + 
  geom_sf(data = dandora_2014, fill = "blue", alpha = 0.5) + 
  geom_sf(data = Nairobi_center, color = "black", size = 0.5) + 
 # geom_sf(data = gps_Kenya_Nairobi, aes(x = "LONGNUM", y = "LATNUM"), color = "red", size = 2, alpha = 0.7) +
  geom_sf(data = gps_Kenya_Nairobi, color = "red", size = 0.5, shape = 21, fill = "red", alpha = 0.7) +
  theme_void() +
  ggspatial::annotation_scale() +
  ggtitle("Spatial Data for Nairobi with Clusters")

#Bringing it together for Nairobi


# Calculate the distance from the city center to the landfill
dandora_2014 <- st_transform(dandora_2014, st_crs(Nairobi_center))
cityNairobi_tolandfill = st_distance(Nairobi_center, dandora_2014)
print(cityNairobi_tolandfill)

outBig <- st_buffer(Nairobi_center, cityNairobi_tolandfill +2000)
outSmall <- st_buffer(Nairobi_center, 5000)

buffer = st_difference(outBig,outSmall)


# intersections
intersections_Nairobi <- st_intersects(gps_Kenya_Nairobi, buffer)
# Filter cities intersecting with the landfills
intersecting_Nairobi<- gps_Kenya_Nairobi[which(lengths(intersections) > 0), ]

print(intersecting_Nairobi)

ggplot() +
  geom_sf(data = Nairobi_data) + 
  geom_sf(data = dandora_2014, fill = "blue", alpha = 0.5) + 
  geom_sf(data = outBig, color = "black", size = 0.5) + 
  geom_sf(data = outSmall, color = "black", size = 0.5) + 
  geom_sf(data = buffer, fill = "blue", alpha = 0.5) + 
  geom_sf(data = gps_Kenya_Nairobi, color = "red", size = 0.5, shape = 21, fill = "red", alpha = 0.7) +
  geom_sf(data = Nairobi_center, color = "black", size = 0.5) + 
  geom_sf(data = intersecting_Nairobi, color = "yellow", size = 0.5, shape = 21, fill = "yellow", alpha = 0.7)
theme_void() +
  ggspatial::annotation_scale() +
  ggtitle("Spatial Data for Nairobi with Clusters")

# For Mombasa
#Filter data for Zigira
zigira_sf <- polygons_sf %>% filter(landfill_name == "zigira")

# Plot the polygons for Zigira landfill over the years


ggplot() +
  geom_sf(data = zigira_sf) +
  facet_wrap(~year) +
  ggtitle("Zigira Landfill Polygons Over Years") +
  theme(legend.position = "bottom") +
  theme_void() 

# Add zigira polygon into the map (2014 extent)

zigira_2014 <- zigira_sf %>%
  filter(year == 2014)

ggplot() +
  geom_sf(data = Mombasa_data) + 
  geom_sf(data = zigira_2014, fill = "blue", alpha = 0.5) + 
  geom_sf(data = Mombasa_center, color = "black", size = 3) + 
  theme_void() +
  ggspatial::annotation_scale() +
  ggtitle("Spatial Data for Mombasa with City Center Point and Zigira Polygon (2014)")




