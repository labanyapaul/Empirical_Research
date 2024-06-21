This is to show where some of the data has been saved and to follow along on important steps. Some data which is sensitive and is on gitignore. will be commented on here. 

* Landfill Polygons
- As we did not have enough information in some landfills due to missing satellite imagery/ missing years. We have taken the year before and replicated it for the year after (missing imagery), and for the other years which are also missing. 

* Unit measurements are in foot -> The CRS used EPSG: 2136 measurement unit is 'Gold Coast foot'. 


# Intersecting cities 

  GHANA: 
- Out of the 10 landfills collected, we have only 3 that intersected with the DHS data. However, out of those 3 only 2 had full information. For 1 specific landfill there was no control group for 2008, hence it had to be dropped. Therefore the 2 cities we analyse are Accra and Kumasi, landfills Agbogbloshie and Oti.

  KENYA: 
- Out of the 7 landfills collected, we have only 3 that intersected with the DHS data. However, there was 2 cities. 2 landfills resided in Nairobi (Kibera and Dandora), and 1 landfill in Mombasa (Maweni).
- However, there was only relevant data for 2014 (dhs clusters). 

# Input 
- City data is saved under /input/world_2015_20000.gpkg
- Ghana: Landfill data (.kmz files) are saved under /input/Ghana/Landfills. 
- Ghana: GPS files are saved under /input/Ghana/GHGE71FL_2014 and GHGE5AFL_2008. Within these files we have used the corresponding .shp files and intersected with the landfill polygon data. 


# Output 
Some sensitive data has been saved here such as:
- Ghana: dhs ipums data under /idhs_00003.csv. 
- Ghana: /Ghana_all_landfills_polygon_nogeometry.csv (This is derived from milestone 2, it shows the growth of the landfills). 
- Ghana: /combined_dataghana.csv (This is the end result in milestone 2, to identify the wealth variable of our control and treatment group)
- Ghana: dhs ipums new data for robustness check (Sex of Head of Household and Sex of Household Member) is saved under /idhs_00007.csv


