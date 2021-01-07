# Script to add a new dataset in the the metadata
# compute the convex hull and merge with existing information

# Gulf of Saint Laurent North

library(raster); library(rgdal)
library(alphahull) #for ahull
library(maps); library(mapdata)
library(rgeos) # for gDifference
library(sf) # for st_geometry
source("update/Tools.R")
resch <- 1 #resolution
rndch <- 8 #rounded
period <- 2001:2018

# Load last version of the shapefile
shp <- readOGR("update/Metadata_last.shp", "Metadata_last")

pro <- proj4string(shp)

# Load last version of the shapefile
meta <- read.csv("update/Metadata_last.csv")
if(ncol(meta)==1){ # or ';'
  meta <- read.csv("update/Metadata_last.csv", sep=";")
}

# 1. Load dataset with haul coordinates ------------
# downloaded directly from url
# https://open.canada.ca/data/en/dataset/4eaac443-24a8-4b37-9178-d7cce4eb7c7b
url1 <- "https://pacgis01.dfo-mpo.gc.ca/FGPPublic/needler/PGF_Needler.csv"
tab1 <- read.csv(url1, sep=";") #25387 23

# https://open.canada.ca/data/en/dataset/40381c35-4849-4f17-a8f3-707aa6a53a9d
url2 <- "https://pacgis01.dfo-mpo.gc.ca/FGPPublic/teleost/PGF_Teleost.csv"
tab2 <- read.csv(url2, sep=";") #49789    23

# merge the two tables together
tab <- rbind(tab1, tab2)
names(tab)
# create a 'Year' variable from date
tab$Year <- substr(tab$Date_Deb_Trait,1,4)

# customize variable names
varname <- list(
  Year = "Year",
  Lat = "Latit_Deb", 
  Lon = "Longit_Deb",
  Depth = "Prof_Min"
)

# provide information about the survey
# Please do not include any ',' or ';'
# as the information will be saved in a coma delimited format.
info <- list(
  Contact ="Department of Fisheries and Oceans (DFO)",
  email = NA,
  Contact2 ="",
  email2 ="",
  link ="https://open.canada.ca/data/dataset/",
  link2 ="",
  Providr	="Department of Fisheries and Oceans (DFO)",
  Opn_ccs	="Publicly available",
  Div_sps	="Publicly available",
  Length ="Available upon request",
  Continent	="North America",
  First_year = "1978",
  Area = "Gulf of St. Lawrence North"
)

#if only one survey
tab$Survey <- as.factor("GSL-N")

# Year of the survey
table(tab[,varname$Year])
#Select only since 2001
tab <- tab[tab[varname$Year]>2000,]

tab$Trait
# If dataset has multiple rows per haul (e.g. catch composition)
# Use or create an unique ID per haul
tab$ID <- paste(tab$Nom_Navire, tab$No_Releve, tab$Trait, tab$Date_Deb_Trait, 
                tab$Hre_Deb, tab$Latit_Deb, tab$Longit_Deb, sep="_")
length(unique(tab$ID))
# Keep only one row per haul
tab <- tab[!duplicated(tab$ID),]

# Verify depth information
# If available in dataset, check that all depth >0
boxplot(tab[,varname$Depth])

# Else, depth estimated from GEBCO
# coo<-cbind(tab[,varname$Lon], tab[,varname$Lat])
# coosp <- SpatialPoints(coo, proj4string = CRS(pro))
# depth<-extract(gebco, coosp)
# tab$depth<-ifelse(depth>0, NA, -depth)
# varname$Depth <- "depth"
# boxplot(tab[,varname$Depth])

# 2. Compute convex hull ------------------------
#Create a list of polygons with the convex hull of stations
plist <- list()
for (i in levels(tab$Survey)){
  subsa <- subset(tab, Survey==i)
  coo <- cbind(subsa[,varname$Lon], subsa[,varname$Lat])
  coo <- coo[!duplicated(coo),]
  
  #test resolution
  z <- ahull(coo, alpha=resch)
  y <- ah2sp(z, name=i, rnd = rndch)
  plist[[i]] <- y
}
sps <-  SpatialPolygons(plist, proj4string = CRS(pro))

#Check if everything ok
pal <- rainbow(nlevels(tab$Survey))
par(mar=c(3,3,1,1))
plot(sps, col=pal, border=pal)
map("world",col = "grey70", fill=TRUE, border=NA, add = TRUE)
points(coo, pch=".")

#Remove country borders
m <- map("world",plot = FALSE, fill = TRUE)
IDs <- sapply(strsplit(m$names, ":"), function(x) x[1])
worldN <- maptools::map2SpatialPolygons(m, IDs=IDs, proj4string = CRS(pro))
worldS <- gSimplify(worldN, tol = 0.0001)
slot(sps, "polygons") <- lapply(slot(sps, "polygons"), checkPolygonsHoles)
updatedS <- gSimplify(sps, tol = 0.0001)
sps <- updatedS - worldS

#Calculate area
areakm2 <- round(area(sps)/1000000, 1)

#Stations per year
stpyr <- tapply(tab$ID, list(tab$Survey, tab[,varname$Year]), lunique)
stpyr <- stpyr[,match(period, colnames(stpyr))]
stpyr[is.na(stpyr)] <- 0
names(stpyr) <- period

#Compute the metadata
newsurvey <- data.frame(
  "Survey"=levels(tab$Survey),
  "nbHauls"=as.numeric(tapply(tab$ID, tab$Survey, lunique)),
  "minYear"=as.numeric(tapply(tab[,varname$Year], tab$Survey, min)),
  "maxYear"=as.numeric(tapply(tab[,varname$Year], tab$Survey, max)),
  "minLat"=round(as.numeric(tapply(tab[,varname$Lat], tab$Survey, min, na.rm=TRUE)),2),
  "maxLat"=round(as.numeric(tapply(tab[,varname$Lat], tab$Survey, max, na.rm=TRUE)),2),
  "minLong"=round(as.numeric(tapply(tab[,varname$Lon], tab$Survey, min, na.rm=TRUE)),2),
  "maxLong"=round(as.numeric(tapply(tab[,varname$Lon], tab$Survey, max, na.rm=TRUE)),2),
  "minDpth"=round(as.numeric(tapply(tab[,varname$Depth], tab$Survey, min03, na.rm=TRUE))),
  "maxDpth"=round(as.numeric(tapply(tab[,varname$Depth], tab$Survey, max97, na.rm=TRUE))),
  "areakm2"=areakm2,
  "nbY"= sum(stpyr>0),       
  "Contact"= info$Contact,
  "email"= info$email,
  "Contact2"= info$Contact2,
  "email2"= info$email2,  
  "link"=info$link,
  "link2"=info$link2,
  "Providr"=info$Providr,
  "Opn_ccs"=info$Opn_ccs,
  "Div_sps"=info$Div_sps,
  "Length"=info$Length,
  "Continent"=info$Continent,
  "First_year"=info$First_year,
  "Area"=info$Area      
)
row.names(newsurvey) <- newsurvey$Survey
newshp <- SpatialPolygonsDataFrame(sps, data = newsurvey)


# 3. Merge information --------------------------
#if survey already existing
if(any(levels(tab$Survey) %in% shp$Survey)){
  #remove existing information about the survey
  shp <- shp[-which(shp$Survey%in%levels(tab$Survey)),]
  meta <- meta[!meta$Survey%in%levels(tab$Survey),]
}

# bind the new shapefile
updatedshp <- bind(shp, newshp)
  
# bind the new metadata
newmeta <- as.data.frame(c(newsurvey, stpyr))
updatedmeta <- rbind(meta, newmeta)

#make sure the metadata is in the same order than the shapefile
ordmeta <- match(updatedshp$Survey, updatedmeta$Survey)

#4. Save the shapefile and metadata -------------

# Save the updated version with today's suffix
filetoday <- paste0("Metadata_", today)
writeOGR(updatedshp, dsn = paste0("update/", filetoday, ".shp"),
         layer = filetoday, driver="ESRI Shapefile",
         overwrite_layer = TRUE)

write.csv(updatedmeta[ord,], paste0("update/", filetoday, ".csv"),
          row.names = FALSE)


# Save the updated version as last version
filelast <- "Metadata_last"
writeOGR(updatedshp, dsn = paste0("update/", filelast, ".shp"),
         layer = filelast, driver="ESRI Shapefile",
         overwrite_layer = TRUE)
ord<-order(updatedmeta$Continent, updatedmeta$Survey)

write.csv(updatedmeta[ord,], paste0("update/", filelast, ".csv"),
          row.names = FALSE)
