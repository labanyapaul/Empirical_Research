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

# Filter data for a specific landfill (e.g., Kisumu)
Kisumu_sf <- polygons_sf %>% filter(landfill_name == "kisumu")

# Plot the polygons for Kisumu landfill over the years


ggplot() +
  geom_sf(data = dandora_sf) +
  facet_wrap(~year) +
  ggtitle("Kisumu Landfill Polygons Over Years") +
  theme(legend.position = "bottom") +
  theme_void() 


# Summarize polygon data
Summarized_kenya <- polygons_sf %>%
  st_zm() %>%
  st_transform(crs = "ESRI:54009") %>%
  st_make_valid() %>%
  group_by(landfill_name, year, month) %>%
  summarize(area = sum(st_area(geometry)), .groups = 'drop')

#Reading the city data

gpkg_file <-"C:\\Users\\laban\\Downloads\\world_2015_20000.gpkg"


cities_data <- st_read(gpkg_file)
Summarized_kenya <- Summarized_kenya |> 
  st_transform(crs = "epsg:2136") 

cities_data <- cities_data |> 
  st_transform(crs = "epsg:2136") 

st_crs(cities_data)
st_crs(Summarized_kenya)

# intersections
intersections <- st_intersects(cities_data, Summarized_kenya)

# Filter cities intersecting with the landfills
intersectingK_cities <- cities_data[which(lengths(intersections) > 0), ]

# Print
print(intersectingK_cities)

### GPS data
dhs_gps_2008 <- st_read("C:\\Users\\laban\\Downloads\\Empirical_Research\\input\\Kenya\\GPS 2008")
print(dhs_gps_2008)

dhs_gps_2008 <- dhs_gps_2008 |> 
  st_transform(crs = "epsg:2136") |>
  mutate(area = st_area(geometry)) 

st_crs(dhs_gps_2008)

dhs_gps_2014 <- st_read("C:\\Users\\laban\\Downloads\\Empirical_Research\\input\\Kenya\\GPS 2014")
print(dhs_gps_2014)

dhs_gps_2014 <- dhs_gps_2014 |> 
  st_transform(crs = "epsg:2136") |>
  mutate(area = st_area(geometry)) 

st_crs(dhs_gps_2014)

# Check CRS 

crs_intersectingK_cities <- st_crs(intersectingK_cities)
print(crs_intersectingK_cities)

crs_Nairobi_data <- st_crs(intersectingK_cities)
print(crs_Nairobi_data)

crs_gps08 <- st_crs(dhs_gps_2008)
print(crs_gps08)

crs_gps14 <- st_crs(dhs_gps_2014)
print(crs_gps14)



# intersections of dhs gps data with the cities intersecting with the landfills for year 2008/ 2014 (which have been combined with city data and gps data). 
cityK_dhs_gps_2008<- st_intersects(intersectingK_cities, dhs_gps_2008)
print(cityK_dhs_gps_2008)

cityK_dhs_gps_2014<- st_intersects(intersectingK_cities, dhs_gps_2014)
print(cityK_dhs_gps_2014)
print(intersectingK_cities["cty_name"])
## only 3 cities intersect with landfill data. 

#Filter for cities that intersect 
# Filter cities: 1. Nairobi 
Nairobi_data <- intersectingK_cities %>%
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

# Add Dandora polygon into the map (2014 extent)

dandora_2014 <- dandora_sf %>%
  filter(year == 2014)

ggplot() +
  geom_sf(data = Nairobi_data) + 
  geom_sf(data =dandora_2014, fill = "blue", alpha = 0.5) + 
  geom_sf(data = Nairobi_center, color = "black", size = 3) + 
  theme_void() +
  ggspatial::annotation_scale() +
  ggtitle("Spatial Data forNairobi with City Center Point and Dandora Polygon (2014)")

# adding the dhs_gps_2014 data to the map
print(dhs_gps_2014)
dhs_gps_2014 <- dhs_gps_2014 %>%
  filter(ADM1NAME == "Nairobi")

# dropping R (outliers too far)
dhs_gps_2014 <- dhs_gps_2014 %>%
  filter(URBAN_RURA == "U")

# Create the plot
ggplot() +
  geom_sf(data = Nairobi_data) + 
  geom_sf(data = dandora_2014, fill = "blue", alpha = 0.5) + 
  geom_sf(data = Nairobi_center, color = "black", size = 0.5) + 
  #geom_sf(data = dhs_gps_2014, aes(x = "LONGNUM", y = "LATNUM"), color = "red", size = 2, alpha = 0.7) +
  geom_sf(data = dhs_gps_2014, color = "red", size = 0.5, shape = 21, fill = "red", alpha = 0.7) +
  theme_void() +
  ggspatial::annotation_scale() +
  ggtitle("Spatial Data for Nairobi with Clusters")




crs_dandora <- st_crs(dandora_2014)

# Check CRS of both objects
crs_nairobi <- st_crs(Nairobi_center)
crs_dandora <- st_crs(dandora_2014)

# If the CRS are different, transform dandora_2014 to the CRS of Nairobi_center
if (crs_nairobi != crs_dandora) {
  dandora_2014 <- st_transform(dandora_2014, crs_nairobi)
}

# Calculate the distance
cityKN_to_landfill <- st_distance(Nairobi_center, dandora_2014)



print(cityKN_to_landfill)

outBig <- st_buffer(Nairobi_center, 32000)

outSmall <- st_buffer(Nairobi_center, 10000)

dhs_gps_2014_cityK <- st_intersects(dhs_gps_2014, Nairobi_data)
dhs_gps_2014_city_points <- dhs_gps_2014[which(lengths(dhs_gps_2014_cityK) > 0), ]

buffer = st_difference(outBig,outSmall)
print(buffer)
buffer_city<- st_intersects(buffer, Nairobi_data)
intersecting_buffer_city <- buffer[which(lengths(buffer_city) > 0), ]

intersections <- st_intersects(dhs_gps_2014_city_points, intersecting_buffer_city)
intersecting_buffer_dhs <- dhs_gps_2014_city_points[which(lengths(intersections) > 0), ]

print(intersecting_buffer_dhs)


ggplot() +
  geom_sf(data = Nairobi_data) +
  geom_sf(data = dandora_2014, fill = "blue", alpha = 0.5) + 
  #geom_sf(data = outBig, color = "black", size = 0.5) + 
  #geom_sf(data = outSmall, color = "black", size = 0.5) + 
  geom_sf(data = intersecting_buffer_city, fill = "blue", alpha = 0.1) + 
  geom_sf(data = dhs_gps_2014_city_points, color = "red", size = 0.5, shape = 21, fill = "red", alpha = 0.7) +
  geom_sf(data = Nairobi_center, color = "black", size = 0.5) + 
  geom_sf(data = intersecting_buffer_dhs, color = "yellow", size = 0.5, shape = 21, fill = "yellow", alpha = 0.7) +
  
  theme_void() +
  ggspatial::annotation_scale() +
  ggtitle("Spatial Data for Nairobi with Clusters")

## Treatment/ Control group

#Buffer around landfill agbogbloshie
buffer <- st_buffer(dandora_2014, 10000)

#determine clusters left in the buffer, and intersect once more. 

outBig_dif <- st_difference(outBig, buffer)

# intersections
intersections_buffer <- intersecting_buffer_city[st_intersects(intersecting_buffer_city, outBig_dif, sparse = FALSE), ]

ggplot() +
  geom_sf(data = Nairobi_data) + 
  
  geom_sf(data = outBig, color = "black", size = 0.5) + 
  geom_sf(data = outSmall, color = "black", size = 0.5) + 
  geom_sf(data = buffer, fill = "blue", alpha = 0.5) + 
  geom_sf(data = dhs_gps_2014_city_points, color = "red", size = 0.5, shape = 21, fill = "red", alpha = 0.7) +
  geom_sf(data = Nairobi_center, color = "black", size = 0.5) + 
  geom_sf(data = dandora_2014, fill = "blue", alpha = 0.5) + 
  #geom_sf(data = intersections_buffer, color = "yellow", size = 0.5, shape = 21, fill = "yellow", alpha = 0.7) +
  #geom_sf(data = outBig_dif, fill = "green", alpha = 0.1) +
  theme_void() +
  ggspatial::annotation_scale() +
  ggtitle("Spatial Data for Nairobi with Clusters")


ggplot() +
  geom_sf(data = Nairobi_data) + 
  geom_sf(data = dandora_2014, fill = "blue", alpha = 0.5) + 
  geom_sf(data = outBig, color = "black", size = 0.5,alpha = 0) + 
  geom_sf(data = outSmall, color = "black", size = 0.5,alpha = 0) + 
  geom_sf(data = buffer, fill = "blue", alpha = 0.5) + 
  geom_sf(data = dhs_gps_2014_city_points, color = "red", size = 0.5, shape = 21, fill = "red", alpha = 0.7) +
  geom_sf(data = Nairobi_center, color = "black", size = 0.5) + 
  geom_sf(data = intersecting_buffer_dhs, color = "yellow", size = 0.5, shape = 21, fill = "yellow", alpha = 0.7)
theme_void() +
  ggspatial::annotation_scale() +
  ggtitle("Spatial Data for Nairobi with Clusters")

#what we have 
intersecting_buffer_dhs  
#  
landfill_dhs_outBig <- st_intersects(intersecting_buffer_dhs, buffer)
landfill_dhs_outBig_points <- intersecting_buffer_dhs[which(lengths(landfill_dhs_outBig) > 0), ]

print(landfill_dhs_outBig_points)

buffer_city<- st_intersects(buffer,Nairobi_data)
intersecting_buffer_city <- buffer[which(lengths(buffer_city) > 0), ]


control_group_poly <- st_difference(outBig,buffer)
control_group_poly_int<- st_intersects(intersecting_buffer_dhs, control_group_poly)
control_group <- intersecting_buffer_dhs[which(lengths(control_group_poly_int) > 0), ]


ggplot() +
  geom_sf(data = Nairobi_data) + 
  geom_sf(data = dandora_2014, fill = "blue", alpha = 0.5) + 
  geom_sf(data = outBig, color = "black", size = 0.5,alpha = 0) + 
  geom_sf(data = outSmall, color = "black", size = 0.5,alpha = 0) + 
  geom_sf(data = buffer, fill = "yellow", alpha = 0.1) + 
  geom_sf(data = control_group, color = "blue", size = 0.5, fill = "blue", alpha = 0.5) +
  #geom_sf(data = dhs_gps_2014_city_points, color = "red", size = 0.5, shape = 21, fill = "red", alpha = 0.7) +
  geom_sf(data = Nairobi_center, color = "black", size = 0.5) + 
  geom_sf(data = landfill_dhs_outBig_points, color = "green", size = 0.5, fill = "green", alpha = 0.5)
theme_void() +
  ggspatial::annotation_scale() +
  ggtitle("Spatial Data for Nairobi with Clusters")

##Now doing the same for Kisumu

Kisumu_data <- intersectingK_cities %>%
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

# Add Kisumu polygon into the map (2014 extent)

Kisumu_2014 <- Kisumu_sf %>%
  filter(year == 2014)

ggplot() +
  geom_sf(data = Kisumu_data) + 
  geom_sf(data =Kisumu_2014, fill = "blue", alpha = 0.5) + 
  geom_sf(data = Kisumu_center, color = "black", size = 3) + 
  theme_void() +
  ggspatial::annotation_scale() +
  ggtitle("Spatial Data for Kisumu with City Center Point and Kisumu Polygon (2014)")


# adding the dhs_gps_2014 data to the map

dhs_gps_2014 <- dhs_gps_2014 %>%
  filter(ADM1NAME == "Kisumu")


# Create the plot
ggplot() +
  geom_sf(data = Kisumu_data) + 
  geom_sf(data = Kisumu_2014, fill = "blue", alpha = 0.5) + 
  geom_sf(data = Kisumu_center, color = "black", size = 0.5) + 
  geom_sf(data = dhs_gps_2014, color = "red", size = 0.5, shape = 21, fill = "red", alpha = 0.7) +
  theme_void() +
  ggspatial::annotation_scale() +
  ggtitle("Spatial Data for Kisumu with Clusters")


crs_Kisumu <- st_crs(Kisumu_2014)

# Check CRS of both objects

crs_Kisumu_centre <- st_crs(Kisumu_center)

# If the CRS are different, transform Kisumu_2014 to the CRS of Kisumu_center

if (crs_Kisumu != crs_Kisumu) {
  Kisumu_2014 <- st_transform(Kisumu_2014, crs_Kisumu)
}

# Calculate the distance

cityKM_to_landfill <- st_distance(Kisumu_center, Kisumu_2014)

print(cityKM_to_landfill)

outBigM <- st_buffer(Kisumu_center, 32000)

outSmallM <- st_buffer(Kisumu_center, 10000)

dhs_gps_2014_cityM <- st_intersects(dhs_gps_2014, Kisumu_data)
print(dhs_gps_2014_cityM)

bufferM = st_difference(outBigM,outSmallM)
print(bufferM)
buffer_cityM<- st_intersects(bufferM, Kisumu_data)
intersecting_buffer_cityM <- bufferM[which(lengths(buffer_cityM) > 0), ]

intersections <- st_intersects(dhs_gps_2014_cityM, intersecting_buffer_cityM)

intersecting_buffer_dhsM <- dhs_gps_2014_cityM[which(lengths(intersections) > 0), ]

print(intersecting_buffer_dhsM)

ggplot() +
  geom_sf(data = Kisumu_data) +
  geom_sf(data = Kisumu_2014, fill = "blue", alpha = 0.5) + 
  geom_sf(data = outBigM, color = "black", size = 0.5) + 
  geom_sf(data = outSmallM, color = "black", size = 0.5) + 
  geom_sf(data = intersecting_buffer_cityM, fill = "blue", alpha = 0.1) + 
  geom_sf(data = dhs_gps_2014_cityM, color = "red", size = 0.5, shape = 21, fill = "red", alpha = 0.7) +
  geom_sf(data = Kisumu_center, color = "black", size = 0.5) + 
  geom_sf(data = intersecting_buffer_dhsM, color = "yellow", size = 0.5, shape = 21, fill = "yellow", alpha = 0.7) +
  
  theme_void() +
  ggspatial::annotation_scale() +
  ggtitle("Spatial Data for Kisumu with Clusters")




