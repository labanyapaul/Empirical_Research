This is to show where some of the data has been saved and to follow along on important steps. Some data which is sensitive and is on gitignore. will be commented on here. 

* Landfill Polygons
- As we did not have enough information in some landfills due to missing satellite imagery/ missing years. We have taken the year before and replicated it for the year after (missing imagery), and for the other years which are also missing. 
- For Ghana, out of the 10 landfills collected, we have only 3 that intersected with the DHS data. However, out of those 3 only 2 had full information. For 1 specific landfill there was no control group for 2008, hence it had to be dropped. 

# Input 
- Landfill data (.kmz files) are saved under /Ghana or Kenya/Landfills. 
- City data is saved under /input/world_2015_20000.gpkg
- GPS files are saved under /input/Ghana/GHGE71FL_2014 and GHGE5AFL_2008. Within these files we have used the corresponding .shp files and intersected with the landfill polygon data. (Ghana can be found under all_landfills_polygon.csv in the 2. wrangle folder.)

# Output 
Some sensitive data has been saved here for Ghana, such as:
- Ghana dhs ipums data under idhs_00003.csv. 
- combined_dataGhana.csv (This is the end result in milestone 2, to identify the wealth variable of our control and treatment group)
- Ghana dhs ipums new data for robustness check (Sex of household head) is saved under output/idhs_00007.csv
- Merged dataframe of combined_dataGhana.csv and idhs_00007.csv is saved under output/HHead_ghana.csv

Other data which is uploaded to the repository: 
- all landfills_polygon.csv (This is derived from milestone 2, it shows the growth of the landfills)

#  