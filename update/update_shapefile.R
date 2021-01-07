# Script to update the shapefile with corrected metadata
# Make changes in the file Metadata_last.csv
# The shapefile Metadata_last.shp will be updated automatically


# 1. Load file ------------------------
source("update/Tools.R")

# 1a. csv file Metadata_last.csv
# test if ',' is separating
meta <- read.csv("update/Metadata_last.csv")
if(ncol(meta)==1){ # or ';'
  meta <- read.csv("update/Metadata_last.csv", sep=";")
}

#Replace if any ',' or ';' by '-'
meta <- as.data.frame(apply(meta, 2, rmcoma))

# 1b. shapefile Metadata_last.shp
library(rgdal)
shp <- readOGR("update/Metadata_last.shp", "Metadata_last")

# 1c. Get the date
today <- format(Sys.Date(), format="%d%m%Y")

# 2. Check differences between shapefile and csv -----
# match format shapefile
shpmeta <- shp@data
# dim(shpmeta)
meta2 <- meta[match(shpmeta$Survey, meta$Survey),1:ncol(shpmeta)]
# dim(meta2)

# compare values
difftxt <- which(shpmeta!=meta2, arr.ind = TRUE)

if (nrow(difftxt)>1){
  print(paste(nrow(difftxt), "differences found."))
  shp@data <- meta2
  
  # Save the updated version with today's suffix
  filetoday <- paste0("Metadata_", today)
  writeOGR(shp, dsn = paste0("update/", filetoday, ".shp"),
           layer = filetoday, driver="ESRI Shapefile",
           overwrite_layer = TRUE)
  
  # Save the updated version as last version
  filelast <- "Metadata_last"
  writeOGR(shp, dsn = paste0("update/", filelast, ".shp"),
           layer = filelast, driver="ESRI Shapefile",
           overwrite_layer = TRUE)
  
  print("Shapefile updated !")
} else {
  print("No difference found between csv and shapefile information.")
  print("If you want to update information, make changes in the file Metadata_last.csv")
}

