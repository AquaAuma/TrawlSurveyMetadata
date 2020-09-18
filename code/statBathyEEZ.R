# Script to compute statistics of the coverage of surveys
# Last update 07.01.2019, by R. Frelat

#0. Load package and data -----------------------
# Load needed package
Sys.setenv(LANG = "en")
require(rgdal)
require(raster)
require(mapdata)
require(rworldmap)
require(sf)
options('stringsAsFactors'=FALSE)

# Load shapefile
# change the value corresponding to the last update 
lastupdate <- "18062020"

#make sure to select the latest shapefile
shape<-readOGR(dsn=paste0("data/metadata/Metadata_", lastupdate, ".shp"), 
               layer=paste0("Metadata_", lastupdate))
meta <- read.csv(paste0("data/metadata/Metadata_", lastupdate, ".csv"), 
                   check.names = FALSE)

sum(meta$nbHauls, na.rm=TRUE)
tapply(meta$nbHauls, meta$Opn_ccs, sum, na.rm=TRUE)/sum(meta$nbHauls, na.rm=TRUE)
table(meta$Opn_ccs)/nrow(meta)*100
#Need to merge polygons to avoid multiple polygons in same area
#shape1 <- aggregate(shape) #aggregate doesn't work
shape2 <- st_combine(st_as_sf(shape))
shape2 <- as(shape2, "Spatial")

#1. Compare with EEZ ----------------------------
eezR <- raster("data/eez_v11_R04.nc")
#284 unique values - unique EEZ
#intersection between shape and eez
sheez <- extract(eezR, shape)

#Look at unique id
sheezU <- unique(unlist(sheez))
sheezU <- sheezU[!is.na(sheezU)]
#77 unique EEZ
length(sheezU)

#Check the correspondance to country
require(sf)
eezP <- st_read(paste0("data/eez_v11.shp"))
eezP$GEONAME[match(sheezU, eezP$MRGID)]
sheezC <- c(eezP$TERRITORY1[match(sheezU, eezP$MRGID)], 
            eezP$TERRITORY2[match(sheezU, eezP$MRGID)],
            eezP$TERRITORY3[match(sheezU, eezP$MRGID)])
sheezC <- sheezC[!is.na(sheezC)]
length(unique(sheezC)) # 65 countries


#2. Check bathymetry thresholds -----------------
gebco <- raster("data/GEBCO_2014_Agg25.nc")
minlim <- c(-30, -30, -20, 0)
maxlim <- c(-300, -500, -250, -500)

png("figures/BathyThesholds.png", width=1600, height = 1200, res=200)
par(mfrow=c(2,2), mar=c(3,3,2,1))
for (i in 1:4){
  thraster <- gebco < minlim[i] & gebco > maxlim[i]
  numraster <- freq(thraster, value=1) # 900624 cells with 1
  #Extract the number of pixel '1' in the polygons
  numpoly <- extract(thraster, shape2, fun=sum)
  lab <- paste0("survey cover = ", round(sum(numpoly)/numraster*100), "%")
  
  image(gebco, breaks=c(-10000, maxlim[i], minlim[i], 0, 10000),
        col=c("grey", "green", "grey", "black"), 
        main=paste0(abs(minlim[i]), " - ", abs(maxlim[i]), "m : ", lab))
  plot(shape, add=TRUE, lwd=0.5, border="red")
}
dev.off()


#3. Check primary production thresholds -----------------
globco <- raster("data/Mean_L3m_20052015_GSMCHL1.nc")
#plot(globco)

minlim <- c(0.25, 0.5, 0.75, 1)

png("figures/PProdThesholds.png", width=1600, height = 1200, res=200)
par(mfrow=c(2,2), mar=c(3,3,1,1))
for (i in 1:4){
  thraster <- globco > minlim[i]
  #Total number of 1
  numraster <- freq(thraster, value=1)
  #Extract the number of pixel '1' in the polygons
  numpoly <- extract(thraster, shape2, fun=sum)
  lab <- paste0("survey cover = ", round(sum(numpoly)/numraster*100), "%")
  
  image(globco, breaks=c(0, minlim[i], 1000),
        col=c("black", "green"), 
        main=paste0(minlim[i], "mg/m3 : ", lab))
  plot(shape2, add=TRUE, lwd=0.5, border="red")
}
dev.off()

#4. Cross primary production and globacolour ----
ncell(globco) #37324800
ncell(gebco) #37324800

#Check thresholds of primary production and depth
thraster <- globco > 0.5 & gebco < (-30) & gebco > (-500)
numraster <- freq(thraster, value=1) # 900624 cells with 1

#Extract the number of pixel '1' in the polygons
numpoly <- extract(thraster, shape2, fun=sum)

lab <- paste0("Survey cover = ", round(sum(numpoly)/numraster*100), "%") # 55%

png("figures/PPDetphCover.png", width=1600, height = 900, res=200)
par(mar=c(3,3,3,1), xaxs = "i", yaxs = "i")
image(thraster, breaks=c(-0.1, 0.5, 1.2),
      col=c("white", "green"), 
      main=lab, asp=1)
map("world", col="grey", lwd=0.2,fill=TRUE, border=NA, 
    add=TRUE)
plot(shape2, add=TRUE, lwd=0.5)
dev.off()

#Raster with different colors
#-1: depth
# 0 : none
# 1 : primary production
# 2 : combined depth + primary production
thdepth <- gebco < (-30) & gebco > (-500)
thpp <- globco > 0.5
thcomb <- globco > 0.5 & gebco < (-30) & gebco > (-500)

thraster <- -thdepth + thpp + (2*thcomb)

png("figures/PPDetphDisagregated.png", width=1600, height = 900, res=200)
par(mar=c(3,3,1,1), xaxs="i", yaxs="i")
image(thraster, breaks=c(-1.1, -0.1, 0.5, 1.2, 2.1),
      col=c("red","white", "green", "blue"), 
      main="", asp=1)
map("world", col="grey", lwd=0.2,fill=TRUE, border=NA, 
    add=TRUE)
plot(shape, add=TRUE, lwd=0.5)
legend("left", fill=c("red", "green", "blue"), 
       bty = "n",cex = 0.8,
       legend = c("depth, SC=33%",
                  "PProd, SC=27%", 
                  "Both, SC=62%"))
dev.off()

#5. Cover per continent -------------------------
Conti <- raster("data/Conti_R04.nc")
ContiS<-readOGR(dsn="data/ContinentShelf.shp", 
               layer="ContinentShelf")
res(globco)
res <- c()
for (i in sort(unique(ContiS$id))){
  thraster <- globco > 0.5 & gebco < (-30) & gebco > (-500) & Conti==as.numeric(i)
  numraster <- freq(thraster, value=1)
  numpoly <- extract(thraster, shape2, fun=sum, na.rm=TRUE)
  sc <- round(sum(numpoly)/numraster*100, 2)
  newline <- c(i, numraster, numpoly, sc)
  res <- rbind(res, newline)
}
colnames(res) <- c("id", "numpix", "numcover", "coverPerc")

tabSX <- cbind(ContiS@data, res[,-1])
write.csv(tabSX, file = "figures/TabSX_CoverPerContinent.csv", 
          row.names = FALSE)


#6. Cover of fishing ground -------------------------
Fishing <- stack("data/TrawlFishing20132016_R04.tif")
names(Fishing) <- c(2013:2016, "sum")
#plot(Fishing[[5]])

#Set fishing thresholds
#plot(Fishing[[5]], breaks=c(0,1,4,10, 15000), col=c("white","green", "blue", "red"))
thraster <- Fishing[[5]] >= 4

numraster <- freq(thraster, value=1) # 900624 cells with 1

#Extract the number of pixel '1' in the polygons
numpoly <- extract(thraster, shape2, fun=sum, na.rm=TRUE)

lab <- paste0("Trawl Fishing 2013-2016 - Survey cover = ", round(sum(numpoly)/numraster*100), "%") # 51%

png("figures/Fishing201316Cover.png", width=1600, height = 900, res=200)
par(mar=c(3,3,3,1), xaxs = "i", yaxs = "i")
image(thraster, breaks=c(-0.1, 0.5, 1.2),
      col=c("white", "green"), xlim=c(-180, 180),
      main=lab, asp=1, ylim=c(-80, 90))
map("world", col="grey", lwd=0.2,fill=TRUE, border=NA, 
    add=TRUE)
plot(shape2, add=TRUE, lwd=0.5)
dev.off()



yr <- 2013:2016
png("figures/FishingCover.png", width=1600, height = 900, res=200)

par(mfrow=c(2,2), mar=c(3,3,3,1), xaxs = "i", yaxs = "i")
for (i in 1:4){
  thraster <- Fishing[[i]] > 1 
  numraster <- freq(thraster, value=1) # 900624 cells with 1
  numpoly <- extract(thraster, shape2, fun=sum, na.rm=TRUE)
  sc <- round(sum(numpoly)/numraster*100, 2)
  lab <- paste0(yr[i], ", survey cover = ", sc, "%")
  
  image(thraster, asp=1,
        col=c("white", "red"), 
        main=lab)
  plot(shape2, add=TRUE, lwd=0.5, border="black")
}
dev.off()


#7. Cover of fisheries production with Reg Watson's data --------
library(dplyr)
library(ggplot2)
library(RColorBrewer)
load('data/catch/Data_for_Aurore.Rdata')

catch <- tr_avg %>%
  group_by(Cell, LonCentre, LatCentre) %>% 
  summarize(Total=sum(Total))

catch_dem <- tr_avg %>% 
  filter(Group == 'Dem')

pal.map <- colorRampPalette(rainbow(15))
RdBu_r <- colorRampPalette(c("#053061","#4694C4","#F6F6F6","#E7886C","#67001F"),interpolate = "spline")
world <- map_data("world")

ggplot() + geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  geom_tile(data=catch, aes(x=LonCentre, y=LatCentre, fill=log(Total)))+
  scale_fill_gradientn(colours = RdBu_r(10),guide = "colourbar") + theme_bw()

ggplot() + geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  geom_tile(data=catch_dem, aes(x=LonCentre, y=LatCentre, fill=log(Total)))+
  scale_fill_gradientn(colours = RdBu_r(10),guide = "colourbar") + theme_bw()
