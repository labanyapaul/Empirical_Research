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
library(geos)
library(here)

# Define the directory where your KMZ files <are stored
kmz_dir <- here::here("input/Kenya/Landfills")

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
                  patterns = c("{landfill_name}{year}{polygon_no}",
                               "{landfill_name}_{year}",
                               "{landfill_name} {year}"),
                  remove = FALSE)
}


# Summarize polygon data
summarized_data <- polygons_sf %>%
  st_zm() %>%
  st_transform(crs = "epsg:2136") %>%
  st_make_valid() %>%
  group_by(landfill_name, year) %>%
  summarize(area = sum(st_area(geometry)), .groups = 'drop')

# Display summarized data
print("Summarized data:")
print(summarized_data)

# Summarize polygons into one multipolygon and calculate the area for all landfills
all_landfills_polygon <- summarized_data %>%
  st_zm() %>%
  st_transform(crs = "epsg:2136") %>%
  st_make_valid() %>%
  group_by(landfill_name, year) %>%
  summarize(geometry = st_union(geometry)) %>%
  ungroup() %>%
  st_transform(crs = "epsg:2136") %>%
  mutate(area = st_area(geometry))

# Ensure the area is in numeric format for plotting
all_landfills_polygon <- all_landfills_polygon %>%
  mutate(area_m2 = drop_units(set_units(area, "m^2")))

# Display the summarized polygon data with area for all landfills
print(all_landfills_polygon)
st_crs(all_landfills_polygon)

#save as csv file
#write.csv(all_landfills_polygon, "output/all_landfills_polygon.csv", row.names = FALSE)

# Load the city data from the gpkg file
city_data <- st_read(here::here("input/world_2015_20000.gpkg"))

city_data <- city_data |> 
  st_transform(crs = "epsg:2136") 

st_crs(city_data)
st_crs(all_landfills_polygon)

# intersections
intersections <- st_intersects(city_data, all_landfills_polygon)

# Filter cities intersecting with the landfills
intersecting_cities <- city_data[which(lengths(intersections) > 0), ]
print(intersecting_cities)

### GPS data
dhs_gps_2008 <- st_read(here::here("input/Kenya/GPS 2008"))
print(dhs_gps_2008)

dhs_gps_2008 <- dhs_gps_2008 |> 
  st_transform(crs = "epsg:2136") |>
  mutate(area = st_area(geometry)) 

st_crs(dhs_gps_2008)

dhs_gps_2014 <- st_read(here::here("input/Kenya/GPS 2014"))

print(dhs_gps_2014)

dhs_gps_2014 <- dhs_gps_2014 |> 
  st_transform(crs = "epsg:2136") |>
  mutate(area = st_area(geometry)) 

st_crs(dhs_gps_2014)

# Check CRS 

crs_intersecting_cities <- st_crs(intersecting_cities)
print(crs_intersecting_cities)

crs_ <- st_crs(intersecting_cities)
print(crs_)

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



city <- "Mombasa" 
# Year options are: 2014, 2008
year <-2014

wealthindexqhh <- read.csv(here::here("output/idhs_00003.csv"))

if (year ==2014){
  dhs_year_data <- dhs_gps_2014
} else if (year ==2008){
  dhs_year_data <- dhs_gps_2008
} else {
  print("Year not found")
  break
}

city_selected <- "Mombasa"
landfill_selected <- "maweni"
year_selected <- year
dhs_gps_year_filter_name_city <- "Mombasa"
big_buffer_additional <- 2000
small_buffer <- 10000
landfill_buffer <- 10000
output_file_name <- "output//treatment_maweni_14_survey.rds"
COUNTRY_value <- 404

city_data <- intersecting_cities %>%
  filter(cty_name == city_selected)
print(city_data)

# Calculate centroid of polygon
city_center <- city_data %>%
  st_centroid() 

#calculate centroid using st_centroid() of landfill polygon
landfill_center <- all_landfills_polygon %>%
  filter(landfill_name == landfill_selected) %>%
  st_centroid()

# Plot city center point
plot_title <- paste("Spatial Data for",city_selected,"with City Center Point")
ggplot() +
  geom_sf(data = city_data) + 
  geom_sf(data = city_center, color = "black", size = 3) + 
  theme_void() +
  ggspatial::annotation_scale() +
  
  ggtitle(plot_title)

## Filter data for landfill

landfill_sf <- all_landfills_polygon %>% filter(landfill_name == landfill_selected)

# Plot the polygons for landfill landfill over the years
plot_title <- paste(landfill_selected,"Landfill Polygons Over Years")
print(landfill_sf)
ggplot() +
  geom_sf(data = landfill_sf) +
  geom_sf(data = landfill_center, color = "black", size = 1) + 
  facet_wrap(~year) +
  ggtitle(plot_title) +
  theme(legend.position = "bottom") +
  theme_void()


# Plot barplot of landfill area over the years

plot_title <- paste(landfill_selected,"Landfill Area Over Time")
ggplot(data = landfill_sf) +
  geom_col(aes(x = as.character(year), y = area_m2)) +
  coord_flip() +
  labs(y = "Area (m2)", x = "Year") + 
  ggtitle(plot_title) +
  theme_minimal()


# Filter data for the landfill
landfill_sf <- polygons_sf %>% 
  filter(landfill_name == landfill_selected)

# Summarize polygons into one multipolygon
landfill_polygon <- landfill_sf %>%
  st_zm() %>%
  st_transform(crs = "epsg:2136") %>%
  st_make_valid() %>%
  group_by(landfill_name, year) %>%
  summarize(geometry = st_union(geometry)) %>%
  ungroup() %>%
  st_transform(crs = "epsg:2136") %>%
  mutate(area = st_area(geometry))

print(landfill_polygon)

# Add landfill polygon into the map 

landfill_year <- landfill_polygon %>%
  filter(year == year_selected)

plot_title <- paste("Spatial Data for",city_selected,landfill_selected, year_selected)

ggplot() +
  geom_sf(data = city_data) + 
  geom_sf(data = landfill_year, fill = "blue", alpha = 0.5) + 
  geom_sf(data = city_center, color = "black", size = 3) + 
  theme_void() +
  ggspatial::annotation_scale() +
  ggtitle(plot_title)


# adding the dhs_gps_2014 data to the map
print(dhs_gps_2014)

dhs_gps_year <- dhs_year_data %>%
  filter(ADM1NAME == dhs_gps_year_filter_name_city)

# dropping R (outliers too far)
dhs_gps_year <- dhs_gps_year %>%
  filter(URBAN_RURA == "U")

# Create the plot
plot_title <- paste("Spatial Data for",city_selected,"with Clusters", year_selected)

ggplot() +
  geom_sf(data = city_data) + 
  geom_sf(data = landfill_year, fill = "blue", alpha = 0.5) + 
  geom_sf(data = city_center, color = "black", size = 0.5) + 
  #geom_sf(data = dhs_gps_year, aes(x = "LONGNUM", y = "LATNUM"), color = "red", size = 2, alpha = 0.7) +
  geom_sf(data = dhs_gps_year, color = "red", size = 0.5, shape = 21, fill = "red", alpha = 0.7) +
  theme_void() +
  ggspatial::annotation_scale() +
  ggtitle(plot_title)

city_to_landfill = st_distance(city_center, landfill_year)
print(city_to_landfill)

outBig <- st_buffer(city_center, city_to_landfill+big_buffer_additional)

outSmall <- st_buffer(city_center,small_buffer)

dhs_gps_year_city <- st_intersects(dhs_gps_year, city_data)
dhs_gps_year_city_points <- dhs_gps_year[which(lengths(dhs_gps_year_city) > 0), ]

buffer = st_difference(outBig,outSmall)
print(buffer)
buffer_city<- st_intersects(buffer, city_data)
intersecting_buffer_city <- buffer[which(lengths(buffer_city) > 0), ]

intersections <- st_intersects(dhs_gps_year_city_points, intersecting_buffer_city)
intersecting_buffer_dhs <- dhs_gps_year_city_points[which(lengths(intersections) > 0), ]

print(intersecting_buffer_dhs)


plot_title <- paste("Spatial Data for",city_selected,"with Clusters",year)

ggplot() +
  geom_sf(data = city_data) +
  geom_sf(data = landfill_year, fill = "blue", alpha = 0.5) + 
  #geom_sf(data = outBig, color = "black", size = 0.5) + 
  #geom_sf(data = outSmall, color = "black", size = 0.5) + 
  geom_sf(data = intersecting_buffer_city, fill = "blue", alpha = 0.1) + 
  geom_sf(data = dhs_gps_year_city_points, color = "red", size = 0.5, shape = 21, fill = "red", alpha = 0.7) +
  geom_sf(data = city_center, color = "black", size = 0.5) + 
  geom_sf(data = intersecting_buffer_dhs, color = "yellow", size = 0.5, shape = 21, fill = "yellow", alpha = 0.7) +
  theme_void() +
  ggspatial::annotation_scale() +
  ggtitle(plot_title)
## Treatment/ Control group

#Buffer around landfill Maweni


buffer <- st_buffer(landfill_year, landfill_buffer)

#determine clusters left in the buffer, and intersect once more. 

outBig_dif <- st_difference(outBig, buffer)

# intersections
intersections_buffer <- intersecting_buffer_city[st_intersects(intersecting_buffer_city, outBig_dif, sparse = FALSE), ]

plot_title <- paste("Spatial Data for",city_selected,"with Clusters",year)
ggplot() +
  geom_sf(data = city_data) + 
  
  geom_sf(data = outBig, color = "black", size = 0.5) + 
  geom_sf(data = outSmall, color = "black", size = 0.5) + 
  geom_sf(data = buffer, fill = "blue", alpha = 0.5) + 
  geom_sf(data = dhs_gps_year_city_points, color = "red", size = 0.5, shape = 21, fill = "red", alpha = 0.7) +
  geom_sf(data = city_center, color = "black", size = 0.5) + 
  geom_sf(data = landfill_year, fill = "blue", alpha = 0.5) + 
  #geom_sf(data = intersections_buffer, color = "yellow", size = 0.5, shape = 21, fill = "yellow", alpha = 0.7) +
  #geom_sf(data = outBig_dif, fill = "green", alpha = 0.1) +
  theme_void() +
  ggspatial::annotation_scale() +
  ggtitle(plot_title)

plot_title <- paste("Spatial Data for",city_selected,"with Clusters",year)
ggplot() +
  geom_sf(data = city_data) + 
  geom_sf(data = landfill_year, fill = "blue", alpha = 0.5) + 
  geom_sf(data = outBig, color = "black", size = 0.5,alpha = 0) + 
  geom_sf(data = outSmall, color = "black", size = 0.5,alpha = 0) + 
  geom_sf(data = buffer, fill = "blue", alpha = 0.5) + 
  geom_sf(data = dhs_gps_year_city_points, color = "red", size = 0.5, shape = 21, fill = "red", alpha = 0.7) +
  geom_sf(data = city_center, color = "black", size = 0.5) + 
  geom_sf(data = intersecting_buffer_dhs, color = "yellow", size = 0.5, shape = 21, fill = "yellow", alpha = 0.7)+
  theme_void() +
  ggspatial::annotation_scale() +
  ggtitle(plot_title)

#what we have 
intersecting_buffer_dhs  
#  
landfill_dhs_outBig <- st_intersects(intersecting_buffer_dhs, buffer)
landfill_dhs_outBig_points <- intersecting_buffer_dhs[which(lengths(landfill_dhs_outBig) > 0), ]

print(landfill_dhs_outBig_points)

buffer_city<- st_intersects(buffer, city_data)
intersecting_buffer_city <- buffer[which(lengths(buffer_city) > 0), ]


control_group_poly <- st_difference(outBig,buffer)
control_group_poly_int<- st_intersects(intersecting_buffer_dhs, control_group_poly)
control_group <- intersecting_buffer_dhs[which(lengths(control_group_poly_int) > 0), ]

# CORRECT PLOT showing Treatment and Control group for  Kibera
plot_title <- paste("Spatial Data for",city_selected,"with Clusters",year)
ggplot() +
  geom_sf(data = city_data) +
  geom_sf(data = landfill_year, fill = "blue", alpha = 0.5) +
  geom_sf(data = outBig, color = "black", size = 0.5,alpha = 0) +
  geom_sf(data = outSmall, color = "black", size = 0.5,alpha = 0) +
  geom_sf(data = buffer, fill = "yellow", alpha = 0.1) +
  geom_sf(data = control_group, color = "blue", size = 0.5, fill = "blue", alpha = 0.5) +
  #geom_sf(data = dhs_gps_year_city_points, color = "red", size = 0.5, shape = 21, fill = "red", alpha = 0.7) +
  geom_sf(data = city_center, color = "black", size = 0.5) +
  geom_sf(data = landfill_dhs_outBig_points, color = "green", size = 0.5, fill = "green", alpha = 0.5)+
  theme_void() +
  ggspatial::annotation_scale() +
  ggtitle(plot_title)

#save to respective names

treatment_landfill <- landfill_dhs_outBig_points
print(treatment_landfill)

control_city <-control_group
print(control_city)




# Combine dhs gps data together with the dhs wealth survey data
################################################################################

# Filter for the year 2014 or 2008 and select columns (common variables) for merging

#drop NAs 
kenya_wealthqhh <- na.omit(wealthindexqhh)
print(kenya_wealthqhh)

kenya_wealthqhh <- wealthindexqhh %>%
  filter(YEAR == year_selected) %>%
  filter(COUNTRY == 404) %>%
  select(YEAR, DHSID, COUNTRY,WEALTHQHH)

print(kenya_wealthqhh)

kenya_wealthqhh <- kenya_wealthqhh %>%
  select(-COUNTRY)

print(kenya_wealthqhh)


#check column names
print(colnames(treatment_landfill))
print(colnames(kenya_wealthqhh))

# Merge the two data sets: the treatment with the Wealthqhh data
treatment_landfill_variable <- treatment_landfill %>%
  left_join(kenya_wealthqhh, by = c("DHSID" = "DHSID", "DHSYEAR" = "YEAR")) 

print(treatment_landfill_variable)



# Combine control group with Wealthqhh data 
control_city_variable <- control_city %>%
  left_join(kenya_wealthqhh, by = c("DHSID" = "DHSID", "DHSYEAR" = "YEAR"))


print(control_city_variable)
print(treatment_landfill_variable)

#write.table(control_city_variable, "./control_landfill_data.csv", sep = ",", row.names = FALSE, col.names = !file.exists("./control_landfill_data.csv"), append = T)
#write.table(treatment_landfill_variable, "./treatment_landfill_data.csv", sep = ",", row.names = FALSE, col.names = !file.exists("./treatment_landfill_data.csv"), append = T)



#################################################################################


plot_title <- paste("Wealth Index Quantile for Kenya", landfill_selected, year_selected)

# Combine the datasets and add a 'Group' column to differentiate between Treatment and Control
treatment_landfill_variable$Group <- "Treatment"
control_city_variable$Group <- "Control"
combined_dataMombasa <- rbind(treatment_landfill_variable, control_city_variable)

#Save the combined_dataMombasa as a csv file

write.csv(combined_dataMombasa, "output//combined_dataMombasa.csv", row.names = FALSE)

p <- ggplot(combined_dataMombasa, aes(x = factor(WEALTHQHH), y = after_stat(count), fill = Group)) +
  geom_bar(stat = "count", position = position_dodge(width = 0.9), color = "black") +
  
  #   scale_x_discrete(labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")) +
  labs(title = plot_title,
       x = "Wealth Quintile",
       y = "Count",
       caption = "1: Poorest 2: Poor 3: Middle 4: Rich 5: Richest") +
  scale_fill_manual(values = c("Treatment" = "green", "Control" = "blue")) +
  theme_minimal()

print(p)