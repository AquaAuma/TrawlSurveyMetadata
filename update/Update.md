# How to keep the inventory of bottom trawl surveys updated?

 

All issues should be raised as a 'new issue‘ in the GitHub project. Everyone (data providers, data users and external people) can raise a new issue. 

**Responsible to maintain/solve issues**: Aurore Maureaud, Romain Frelat



***

**Issue I: a survey is completely missing from the database**

1. **Get the metadata** for all the stations realised with otter trawl (make sure it only contains otter trawl stations). The data must contain two columns for the geographic coordinates (latitude and longitude in decimal degrees), one column with the year, and ideally one column for depth (in m). If there are multiple raws per station, we  need an extra column to identify the station/haul ID.
2. **Customize add_dataset_X.R** to fit the new metadata. Among other things, make sure the data is loaded properly, select the proper column names, check the units, etc. When completed, save the script replacing X by the abbreviation of the new dataset.
3. **Update the shapefile** by running the customized script, then don't forget to commit and push the changes in GitHub.
4. **Update the ShinyApp**
   Ideally, the Shiny App should be able to use the latest shapefile automatically. *But not sure how to make it? So far, the ShinyApp is built with the latest shapefile and upladed in shinyapps.io*.



*Estimated time: 30 minutes*



***

**Issue II: there is an error in the metadata table**

1. **Correct the error** in the file metadata_last.csv

2. **Update the shapefile** by running the script update_Shapefile.R

3. **Update the ShinyApp**

   

*Estimated time: 10 minutes*