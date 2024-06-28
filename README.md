This is to show where some of the data has been saved and to follow along on important steps. Some data which is sensitive and is on gitignore. will be commented on here. 

* Landfill Polygons
- As we did not have enough information in some landfills due to missing satellite imagery/ missing years. We have taken the year before and replicated it for the year after (missing imagery), and for the other years which are also missing. 

* The CRS used EPSG: 2136 measurement unit is 'Gold Coast foot'. The buffer distances are in this unit, equivalent to 0.3048 meters. 

* Landfill areas are in their original measurement of m2. 

* Data is shown with 2 decimal points. 


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
- Ghana: dhs ipums new data which now includes the new variables needed as control variables for robustnestness check (Sex of Head of Household(HHEADSEXHH) and Sex of Household Member) is saved under /idhs_00007.csv.

-Kenya: dhs ipums data under /idhs_00003.csv.

-Kenya: (i)combined_dataMombasa.csv- This is the combined dataframe which has the wealth variable, landfill data(control and treatment group) and the dhs data for Mombasa.
(ii)combined_dataKibera.csv- This is the combined dataframe which has the wealth variable, landfill data(control and treatment group) and the dhs data for Kibera.
(iii)combined_dataDandora.csv- This is the combined dataframe which has the wealth variable, landfill data(control and treatment group) and the dhs data for Dandora.

(iv)combined_dataKenya.csv
- Combining (i),(ii) and (iii) we get the combined dataframe for Kenya 

- Kenya: dhs ipums new data which now includes the new variables needed as control variables for robustnestness check (Sex of Head of Household(HHEADSEXHH) and Sex of Household Member) is saved under /idhs_00007.csv.

#Analysis

- Ghana:Ghana_Regression.R : This is the regression analysis for Ghana. It has 2 parts-

1st part- It is the  OLS regression with time dummy(we have 2 years 2008 and 2014), treatment dummy and the interaction term of time and treatment dummy. 
Named- model_GhanaTimeD

2nd part- To make the regression comparable with Kenya(for Kenya we do not have 2008 cluster data),we drop the time dummy and run the regression with only treatment dummy.
Named- model_Ghana

- Kenya:Kenya_Regression.R : This is the regression analysis for Kenya. It has one part.

For Kenya we do not have 2008 cluster data, so we run the regression with only treatment dummy.
Named- model_Kenya


-Ghana_Robustnesscheck.R : We include the the control variable , the sex of the head of household (HHEADSEXHH). We create Dummy if the head is a male. We run the regression with the new control variable.

Similar to the Ghana_Regression.R, we have 2 parts.

1st part- It is the  OLS regression with time dummy(we have 2 years 2008 and 2014), treatment dummy , interaction term of time and treatment dummy and the new control variable(HHEADSEXHH_male)
Named- model_GhanaTimeDcontrol

2nd part- To make the regression comparable with Kenya(for Kenya we do not have 2008 cluster data),we drop the time dummy and run the regression with only treatment dummy and the new control variable(HHEADSEXHH_male).
Named- model_Ghanacontrol


-Kenya_Robustnesscheck.R : We include the the control variable

1st part- Regression with only treatment dummy and the new control variable(HHEADSEXHH_male)
Named- model_Kenyacontrol

2nd part- added another dummy for Household Head Female(HHEADSEXHH_female) .We used HHEADSEXHH_male as the reference category.
To see the effect on Wealth relative to when household head is a male.
Named-model_KenyacontrolFemale
