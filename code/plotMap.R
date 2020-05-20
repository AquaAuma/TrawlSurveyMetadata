# Script to create figure 1, 2 and 4 - map of the surveys
# Last update 04.05.2020, by R. Frelat

#0. Load package and data -----------------------
# Load needed package
library(rgdal)
library(raster)
library(rworldmap)
library(sf)
library(maps)
library(mapdata)
library(VAST)
options('stringsAsFactors'=FALSE)

#Additional function for transparent color
#https://stackoverflow.com/questions/8047668/transparent-equivalent-of-given-color
makeTransparent <- function(..., alpha=0.5) {
  if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
  alpha = floor(255*alpha)  
  newColor = col2rgb(col=unlist(list(...)), alpha=FALSE)
  .makeTransparent <- function(col, alpha) {
    rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
  }
  newColor = apply(newColor, 2, .makeTransparent, alpha=alpha)
  return(newColor)
}

# Load shapefile
# change the value corresponding to the last update 
lastupdate <- "20052020"

#make sure to select the latest shapefile
shape<-readOGR(dsn=paste0("data/metadata/Metadata_", lastupdate, ".shp"), 
               layer=paste0("Metadata_", lastupdate))
#Not sure why, but need st_read for dateline
shape2<-st_read(paste0("data/metadata/Metadata_", lastupdate, ".shp"))
meta <- read.csv(paste0("data/metadata/Metadata_", lastupdate, ".csv"), 
                   check.names = FALSE)

savepdf <- TRUE #else png format
ppi <- 300 #only if savepdf = FALSE

print(length(meta$Survey)) #91 surveys
print(sum(meta$nbHauls)) #271188 hauls
# tapply(meta$nbHauls, meta$Continent, sum)
print(tapply(meta$nbHauls, meta$Opn_ccs, sum)/sum(meta$nbHauls)*100)
#Figure 1 : Global coverage ---------------------
# Set colors for Open Access status
colOA <- list("Publicly available" = "blue",
              "Partly publicly available" = "orange",
              "Available upon request" = "purple",
              "Not publicly available"= "red",
              "Incomplete metadata"="black")

# Change plot order (best OA on top)
oafac <- factor(meta$Opn_ccs, levels = rev(names(colOA)), 
                ordered = TRUE)
shapeOr <- order(oafac)
shape <- shape[shapeOr, ]
meta <- meta[shapeOr,]
#shape@plotOrder <- shapeOr

# Get color for each polygon
oa <- unlist(colOA[shape$Opn_ccs])
transoa <- makeTransparent(oa)

#Hauls area and density
colC <- rainbow(7)[as.factor(meta$Continent)]

# Wintri projection
#remove polygons with longitude <180
# shapeWT <- spTransform(subset(shape, !shape$Survey %in% c("AI")), 
#                        CRS("+proj=wintri"))
shapeWT <- spTransform(shape, CRS("+proj=wintri"))

worldWT <- spTransform(getMap(), CRS("+proj=wintri"))

#Set special color or shading for country with missing data
missCountry <- c('Angola','Japan','India','Argentina','Malaysia')
colWT <- ifelse(worldWT$SOVEREIGNT%in%missCountry,"grey40", "grey90")
denWT<-ifelse(worldWT$SOVEREIGNT%in%missCountry, 40, -1)

if (savepdf){
  pdf(paste0("figures/figure1A_", lastupdate, ".pdf"), 
      width = 8, height = 6)
} else {
  png(paste0("figures/figure1A_", lastupdate, ".png"), 
      width = 8*ppi, height = 6*ppi, res=ppi)
}

par(mar=c(0,0,0,0))
plot(worldWT, col=colWT, border="grey40", lwd=0.2)
plot(shapeWT, col=oa, border=NA, add=TRUE)
plot(worldWT, col=colWT, add=TRUE, border="grey40", lwd=0.2)
#legend(0, bbox(shapeWT)[2,1]*0.9, legend = names(colOA), cex=0.5,
#       fill = unlist(colOA), bg="white")
dev.off()


if (savepdf){
  pdf(paste0("figures/figure1Abis_", lastupdate, ".pdf"), 
      width = 8, height = 6)
} else {
  png(paste0("figures/figure1Abis_", lastupdate, ".png"), 
      width = 8*ppi, height = 6*ppi, res=ppi)
}

par(mar=c(0,0,0,0))
plot(worldWT, col=colWT, border="grey40", lwd=0.2, density=denWT)
plot(shapeWT, col=oa, border=NA, add=TRUE)
plot(worldWT, col=colWT, add=TRUE, border="grey40", lwd=0.2, density=denWT)
dev.off()


#without open access status
if (savepdf){
  pdf(paste0("figures/WorldBTS_", lastupdate, ".pdf"), 
      width = 8, height = 6)
} else {
  png(paste0("figures/WorldBTS_", lastupdate, ".png"), 
      width = 8*ppi, height = 6*ppi, res=ppi)
}
par(mar=c(0,0,0,0))
plot(worldWT, col="grey80", border="grey40", lwd=0.2)
plot(shapeWT, col=rainbow(length(shapeWT)), border=NA, add=TRUE)
plot(worldWT, col="grey80", add=TRUE, border="grey40", lwd=0.2)
dev.off()

# Figure 3: Time series of surveys --------------
period <- 2001:2018
tsm <- meta[,as.character(period)]
oa <- factor(meta$Opn_ccs, levels = rev(names(colOA)), ordered = TRUE)
tsmoa <- apply(tsm, 2, function(x) tapply(x, oa, sum))

#per region
cct <- as.factor(meta$Continent)
Occt <- order(tapply(meta$nbHauls, meta$Continent, sum), decreasing = TRUE)

#No time series
moaT <- tapply(meta$nbHauls, meta$Opn_ccs, sum)
moaC <- tapply(meta$nbHauls, list(meta$Opn_ccs, meta$Continent), sum, na.rm=TRUE)
moaC[is.na(moaC)] <- 0
moaF <- cbind("Total"=moaT, moaC)
moaF <- t(t(moaF)/apply(moaF,2,sum))
lab <- c("Eur", "N.Am", "Asia", "Afr", "Ocea", "S.Am")
#change order of OA status
Ooa<-rev(match(names(colOA), row.names(moaC)))

if (savepdf){
  pdf(paste0("figures/figure1BC_", lastupdate, ".pdf"), 
      width = 6, height = 6)
} else {
  png(paste0("figures/figure1BC_", lastupdate, ".png"), 
      width = 6*ppi, height = 6*ppi, res=ppi)
}
par(mfrow=c(2,1), las=1, mar=c(4,8,2,1))
barplot(moaC[Ooa,Occt], col=rev(unlist(colOA)), 
        names.arg=lab, main="Number of stations", 
        yaxt="n")
axis(2, seq(0, 100000, length.out = 6),
     labels = c(0, "20k", "40k", "60k", "80k", "100k"), xpd=NA)
legend("topright", legend = names(colOA),
       fill = unlist(colOA), bty="n", cex=0.8)
par(mar=c(4,4,2,1))
barplot(moaF[Ooa,c(1, Occt+1)], col=rev(unlist(colOA)), 
        main="Relative number of stations", 
        names.arg=c("Total", lab), ylab="%")
dev.off()

#Figure 2 : Regional zoom -----------------------
#A. Europe
if (savepdf){
  pdf(paste0("figures/appendix4.figure4.4_", lastupdate, ".pdf"), 
      width = 4.5, height = 6)
} else {
  png(paste0("figures/appendix4.figure4.4_", lastupdate, ".png"), 
      width = 4.5*ppi, height = 6*ppi, res=ppi)
}
par(mar=c(0,0,0,0))
map("world", col="grey80", lwd=0.2,fill=TRUE, border="grey40", 
      xlim=c(-30, 40), ylim=c(34, 81), lforce = 1, mar = c(4,4,0,0), 
      asp=1/cos(55*pi/180))
plot(shape, col=transoa, border=NA, add=TRUE)
map("world", col="grey80", lwd=0.2,fill=TRUE, border="grey40", 
      xlim=c(-30, 40), ylim=c(34, 81), lforce = 1, mar = c(4,4,0,0), add=TRUE)
map.axes()
dev.off()

#B. Mediterranean Sea
if (savepdf){
  pdf(paste0("figures/appendix4.figure4.2_", lastupdate, ".pdf"), 
      width = 6, height = 4.5)
} else {
  png(paste0("figures/appendix4.figure4.2_", lastupdate, ".png"), 
      width = 6*ppi, height = 4.5*ppi, res=ppi)
}
par(mar=c(0,0,0,0))
map("world", col="grey80", lwd=0.2,fill=TRUE, border="grey40", 
    xlim=c(-10, 40), ylim=c(25, 50), lforce = 1, mar = c(4,4,0,0), 
    asp=1/cos(38*pi/180))
plot(shape, col=transoa, border=NA, add=TRUE)
map("world", col="grey80", lwd=0.2,fill=TRUE, border="grey40", 
    xlim=c(-10, 40), ylim=c(25, 50), lforce = 1, mar = c(4,4,0,0), 
    asp=1/cos(38*pi/180), add=TRUE)
map.axes()
dev.off()

# Across Dateline
inc <- ! (shape2$minLong< 0 & shape2$maxLong> 0) | shape2$Survey%in%c("NZ-CHAT")
shapeDL <- subset(shape2, inc)
# shape2$Survey[!shape2$Survey%in%shapeDL$Survey]
# Removed 7 surveys that are across longitude 0
# "NS-IBTS", "CGFS", "NOR-BTS", "AI", "ALG", "MEDITS-ESP", "GHA"    
oaDL <- unlist(colOA[shapeDL$Opn_ccs])
transoaDL <- makeTransparent(oaDL)
shape360 <- (st_geometry(st_as_sf(shapeDL)) + c(360,90)) %% c(360) - c(0,90)
world360 <- st_geometry(st_as_sf(map("world", plot = FALSE, fill = TRUE, wrap=c(0,360))))

#C. Bering Sea
if (savepdf){
  pdf(paste0("figures/appendix4.figure4.5_", lastupdate, ".pdf"), 
      width = 5, height = 6)
} else {
  png(paste0("figures/appendix4.figure4.5_", lastupdate, ".png"), 
      width = 5*ppi, height = 6*ppi, res=ppi)
}
par(mar=c(3,3,1,1))
plot(world360, col="grey80", lwd=0.2, border="grey40", 
    xlim=c(160, 200), ylim=c(45, 75), asp=1/cos(60*pi/180))
plot(shape360, col=transoaDL, border=NA, add=TRUE)
plot(world360, col="grey80", lwd=0.2, border="grey40", 
     xlim=c(160, 200), ylim=c(45, 75), add=TRUE)
box()
axis(1, at = seq(160, 200, by = 10), labels = c(-160, -170, 0, 170, 160))
axis(2)
dev.off()

#D. New Zealand
if (savepdf){
  pdf(paste0("figures/appendix4.figure4.3_", lastupdate, ".pdf"), 
      width = 6, height = 6)
} else {
  png(paste0("figures/appendix4.figure4.3_", lastupdate, ".png"), 
      width = 6*ppi, height = 6*ppi, res=ppi)
}
par(mar=c(3.5,3.5,1,1))
plot(world360, col="grey80", lwd=0.2, border="grey40", 
     xlim=c(140, 190), ylim=c(-60, -30), asp=1/cos(45*pi/180))
plot(shape360, col=transoaDL, border=NA, xlim=c(140, 190), add=TRUE,
     ylim=c(-60, -30), mar=c(4,4,1,1))
box()
axis(1, at = seq(140, 200, by = 10), labels = c(seq(140,170, by=10), 0, -170, -160))
axis(2)
plot(world360, col="grey80", lwd=0.2, border="grey40", 
     add=TRUE, exact=TRUE, lforce = 1)
dev.off()


#World across dateline
if (savepdf){
  pdf(paste0("figures/appendix4.figure4.1_", lastupdate, ".pdf"), 
      width = 9, height = 4.6)
} else {
  png(paste0("figures/appendix4.figure4.1_", lastupdate, ".png"), 
      width = 9*ppi, height = 4.6*ppi, res=ppi)
}
par(mar=c(3,3,0.5,0.5), yaxs="i", xaxs="i")
plot(world360, col="grey80", border="grey40", lwd=0.2, 
     xlim=c(0, 360), ylim=c(-80, 80))
plot(shape360, col=oaDL, border=NA, add=TRUE)
plot(world360, col="grey80", add=TRUE, border="grey40", lwd=0.2)
legend(0, bbox(shapeWT)[2,1]*0.9, legend = names(colOA), cex=0.5,
       fill = unlist(colOA))
abline(v=180, col="grey", lty=2, lwd=0.5)
box()
lab <- ifelse(seq(0, 360, by = 50)<180, seq(0, 360, by = 50), seq(0, 360, by = 50)-360)
axis(1, at = seq(0, 360, by = 50), labels = lab)
axis(2)
legend("bottomleft", legend = names(colOA), cex=0.5,
       fill = unlist(colOA), bg="white")
dev.off()


# Figure 5 : Density plot -----------------------
load("data/vast/Arrowtooth_2020-03-18.RData")

projargs_plot <- "+proj=utm +datum=WGS84 +units=km +zone=3"
n_cells <-  125^2
surveyWAm <-shape$Survey[shape$Continent=="North America"]
 # c("Aleutian Islands", "Eastern Bering Sea", 
 #               "Gulf of Alaska", , "DFO-WCHG", "DFO-QCS", 
 #               "DFO-HS","DFO-NF",
 #               "West Coast Annual", "DFO-WCVI")
shapeWAm <- subset(shape, shape$Survey %in% surveyWAm)
#UTM projection
shapeUTM <-spTransform(shapeWAm, CRS(projargs_plot))
shapeUTM$Survey <- as.factor(as.character(shapeUTM$Survey))

colo <- rep("grey40", length(shapeUTM))

if (savepdf){
  pdf(paste0("figures/figure3_", lastupdate, ".pdf"), 
      width = 6, height = 8)
} else {
  png(paste0("figures/figure3_", lastupdate, ".png"), 
      width = 6*ppi, height = 8*ppi, res=ppi)
}
year_index = c(1,9,18)
par( mfrow=c(3,1), mar=c(1,2,1,0), oma=c(2,2,1,0), mgp=c(2,0.5,0), tck=-0.02 )
Y_gs = log(Report$D_gcy[,1,year_index])
Y_gs = ifelse( Y_gs<(max(Y_gs)/1e3), NA, Y_gs )
for(tI in seq_along(year_index) ){
  if(tI==length(year_index)){
    #legend_x = c(0, 0.05); legend_y = c(0.05, 0.45)
    legend_x = c(0, 0.03); legend_y = c(0.05, 0.45)
  }else{
    legend_x = c(NA,NA); legend_y = c(NA,NA)
  }
  plot_variable( Y_gt=Y_gs[,tI], zlim=range(Y_gs,na.rm=TRUE), map_list=MapDetails_List, land_color="grey",
                 add=TRUE, Format="", projargs=projargs_plot, country=c("united states of america","canada","mexico","russia"),
                 legend_x=legend_x, legend_y=legend_y, n_cells=n_cells, xaxt="n" )
  
  plot(shapeUTM, add=TRUE, #lwd=0.5,
       border=colo[as.numeric(shapeUTM$Survey)])
  
  mtext( side=3, text=Year_Set[year_index[tI]] )
  axis(2)
}
axis(1)
mtext( side=c(1,2), text=c("Kilometers east of UTM zone 3", "Kilometers north of equator"), outer=TRUE, line=c(1,0) )
dev.off()
