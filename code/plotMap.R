# Script to create figure 1, 2 and 4 - map of the surveys
# Last update 15.06.2020, by R. Frelat

#0. Load package and data -----------------------
# Load needed package
library(rgdal)
library(raster)
library(rworldmap)
library(sf)
library(maps)
library(mapdata)
library(VAST)
options('stringsAsFactors'=FALSE, 'na.rm'=TRUE)

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
lastupdate <- "18062020"

#make sure to select the latest shapefile
shape<-readOGR(dsn=paste0("data/metadata/Metadata_", lastupdate, ".shp"), 
               layer=paste0("Metadata_", lastupdate))
meta <- read.csv(paste0("data/metadata/Metadata_", lastupdate, ".csv"), 
                   check.names = FALSE)

savepdf <- FALSE #else png format
ppi <- 300 #only if savepdf = FALSE

print(length(meta$Survey)) #94 surveys
print(sum(meta$nbHauls, na.rm=TRUE)) #280281 hauls
# tapply(meta$nbHauls, meta$Continent, sum)
print(tapply(meta$nbHauls, meta$Opn_ccs, sum, na.rm=TRUE)/sum(meta$nbHauls, na.rm=TRUE)*100)

#Figure 1 : Global coverage ---------------------
# Set colors for Open Access status
colOA <- list("Publicly available" = "blue",
              "Partly publicly available" = "orange",
              "Available upon request" = "purple",
              "Not publicly available"= "red",
              "Incomplete metadata"="black")

# Change plot order (best OA on top)
oafac <- factor(shape$Opn_ccs, levels = rev(names(colOA)), 
                ordered = TRUE)
shapeOr <- order(oafac)
shape <- shape[shapeOr, ]
shape$Survey
meta <- meta[shapeOr,]
shape@plotOrder <- 1:length(shape)

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
missCountry <- c('Angola','Japan','India','Malaysia')
colWT <- ifelse(worldWT$SOVEREIGNT%in%missCountry,"grey40", "grey90")

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

# Figure 1BC: Time series of surveys --------------
period <- 2001:2018
tsm <- meta[,as.character(period)]
oa <- factor(meta$Opn_ccs, levels = rev(names(colOA)), ordered = TRUE)
tsmoa <- apply(tsm, 2, function(x) tapply(x, oa, sum))

#per region
cct <- as.factor(meta$Continent)
Occt <- order(tapply(meta$nbHauls, meta$Continent, sum, na.rm=TRUE), decreasing = TRUE)

#No time series
moaT <- tapply(meta$nbHauls, meta$Opn_ccs, sum, na.rm=TRUE)
moaC <- tapply(meta$nbHauls, list(meta$Opn_ccs, meta$Continent), sum, na.rm=TRUE)
moaC[is.na(moaC)] <- 0
moaF <- cbind("Total"=moaT, moaC)
moaF <- t(t(moaF)/apply(moaF,2,sum, na.rm=TRUE))
lab <- c("Europe", "N. America", "Asia", "Africa", "Oceania",  "S. America", "Antartica")
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
barp <- barplot(moaC[Ooa,Occt], col=rev(unlist(colOA)), 
        names.arg=rep("", ncol(moaC)), ylab="Number of samples", 
        yaxt="n")
axis(2, seq(0, 100000, length.out = 6),
     labels = c(0, "20k", "40k", "60k", "80k", "100k"), xpd=NA)
text(barp, par("usr")[3]-10000, 
      srt = 45, adj = 1, xpd = TRUE,
      labels = lab)
legend("topright", legend = names(colOA),
       fill = unlist(colOA), bty="n", cex=0.8)
par(mar=c(4,4,2,1))
barp <- barplot(moaF[Ooa,c(1, Occt+1)], col=rev(unlist(colOA)), 
        main="", names.arg=rep("", ncol(moaF)), 
        ylab="Relative number (%)")
text(barp, par("usr")[3]-0.1, 
     srt = 45, adj = 1, xpd = TRUE,
     labels = c("Total", lab))
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
# Not sure why, but need st_read for dateline
# shape2<-st_read(paste0("data/metadata/Metadata_", lastupdate, ".shp"))
# shape2 <- shape[shapeOr, ]
# inc <- ! (shape2$minLong< 0 & shape2$maxLong> 0) | shape2$Survey%in%c("NZ-CHAT")
# shapeDL <- subset(shape2, inc)

inc <- ! (shape$minLong< 0 & shape$maxLong> 0) | shape$Survey%in%c("NZ-CHAT")
shapeDL <- subset(shape, inc)
# shape$Survey[!shape$Survey%in%shapeDL$Survey]
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

# North Pole view
inc <- (shape$minLat>0 & shape$maxLat> 30 & !shape$Survey%in%c("Gulf of Mexico"))
shape$Survey[inc]
shape@data[shape$Survey=="KOR",]
shapeN <- subset(shape, inc)
xlim <-  c(-180,200)
ylim <-  c(30,90)
m <- map("world", xlim=xlim , ylim=ylim, plot = FALSE, 
         fill = TRUE, lforce ="e")
IDs <- sapply(strsplit(m$names, ":"), function(x) x[1])
worldN <- maptools::map2SpatialPolygons(m, IDs=IDs, proj4string = CRS(proj4string(shape)))

p <-  rbind(c(xlim[1], ylim[1]),
            c(xlim[1], ylim[2]),
            c(xlim[2], ylim[2]),
            c(xlim[2], ylim[1]),
            c(xlim[1], ylim[1]))
bb = SpatialPolygons(list(Polygons(list(Polygon(list(p))),"S")), proj4string = CRS(proj4string(shape)))
shapeNclip <- intersect(shapeN, bb)
shapeNclip@plotOrder <- 1:length(shapeNclip)

proj <- "+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs"
worldNview <- spTransform(worldN, CRSobj = CRS(proj))
shapeNview <- spTransform(shapeNclip, CRSobj = CRS(proj))

oaN <- unlist(colOA[shapeNview$Opn_ccs])
transoaN <- makeTransparent(oaN)

# Ploting details from https://khufkens.com/2017/01/18/r-polar-plots/
pts <- SpatialPoints(rbind(c(-180,30),c(0,30),c(180,85),c(180,85)), 
                     CRS(proj4string(shape)))#CRS("+init=epsg:4326"))
gl <-  gridlines(pts, easts = seq(-180,180,30), 
                 norths = seq(30,85,10), ndiscr = 100)
gl.polar <-  spTransform(gl, proj)
ll = SpatialLines(list(Lines(Line(cbind(seq(-180,180,0.5),rep(30,721))), ID="outer")), CRS("+init=epsg:4326"))
ll.polar <- spTransform(ll, proj)
# l1 = labels(gl.polar, proj4string(shape), side = 1)
# l1$pos = NULL
# l2 = labels(gl.polar, proj4string(shape), side = 2)
# l2$srt = 0
# l2$pos = NULL

if (savepdf){
  pdf(paste0("figures/appendix4.figure4.6_", lastupdate, ".pdf"), 
      width = 5, height = 4.6)
} else {
  png(paste0("figures/appendix4.figure4.6_", lastupdate, ".png"), 
      width = 9*ppi, height = 4.6*ppi, res=ppi)
}
par(mar=c(1,1,1,1))
plot(worldNview, col="grey80", border="grey40", lwd=0.2)
lines(gl.polar, add = TRUE, lwd=0.2, lty=2, xpd=NA)
lines(ll.polar, lwd = 3, lty = 1, xpd=NA)
plot(shapeNview, col=oaN, border=NA, add=TRUE, lforce=TRUE)
dev.off()

# South Pole view
inc <- (shape$minLat<= -30 & shape$maxLat<= -20)
shape$Survey[inc]

shapeS <- subset(shape, inc)
xlim <-  c(-180,200)
ylim <-  c(-90,-30)
m <- map("world", xlim=xlim , ylim=ylim, plot = FALSE, 
         fill = TRUE, lforce = "e")
IDs <- sapply(strsplit(m$names, ":"), function(x) x[1])
worldS <- maptools::map2SpatialPolygons(m, IDs=IDs, proj4string = CRS(proj4string(shape)))

p <-  rbind(c(xlim[1], ylim[1]),
            c(xlim[1], ylim[2]),
            c(xlim[2], ylim[2]),
            c(xlim[2], ylim[1]),
            c(xlim[1], ylim[1]))
bb = SpatialPolygons(list(Polygons(list(Polygon(list(p))),"S")), 
                     proj4string = CRS(proj4string(shape)))
shapeSclip <- intersect(shapeS, bb)
shapeSclip@plotOrder <- 1:length(shapeSclip)

proj <- "+proj=stere +lat_0=-90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs"
worldSview <- spTransform(worldS, CRSobj = CRS(proj))
shapeSview <- spTransform(shapeSclip, CRSobj = CRS(proj))

oaS <- unlist(colOA[shapeSview$Opn_ccs])

# Ploting details from https://khufkens.com/2017/01/18/r-polar-plots/
pts <- SpatialPoints(rbind(c(-180,-85),c(0,-85),c(180,-30),c(180,-30)), 
                     CRS(proj4string(shape)))
gl <-  gridlines(pts, easts = seq(-180,180,30), 
                 norths = seq(-90,35,10), ndiscr = 100)
gl.polar <-  spTransform(gl, proj)
ll = SpatialLines(list(Lines(Line(cbind(seq(-180,180,0.5),rep(-30,721))), ID="outer")), CRS("+init=epsg:4326"))
ll.polar <- spTransform(ll, proj)

# l1 = labels(gl.polar, proj4string(shape), side = 1)
# l1$pos = NULL
# l2 = labels(gl.polar, proj4string(shape), side = 2)
# l2$srt = 0
# l2$pos = NULL

#Polar
if (savepdf){
  pdf(paste0("figures/appendix4.figure4.7_", lastupdate, ".pdf"), 
      width = 5, height = 4.6)
} else {
  png(paste0("figures/appendix4.figure4.7_", lastupdate, ".png"), 
      width = 9*ppi, height = 4.6*ppi, res=ppi)
}
par(mar=c(3,3,0.5,0.5), yaxs="i", xaxs="i")
plot(worldSview, col="grey80", border="grey40", lwd=0.2)
lines(gl.polar, add = TRUE, lwd=0.2, lty=2, xpd=NA)
lines(ll.polar, lwd = 3, lty = 1, xpd=NA)
plot(shapeSview, col=oaS, border=NA, add=TRUE)

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

colo <- rep("grey60", length(shapeUTM))

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
  plot_variable( Y_gt=Y_gs[,tI], zlim=range(Y_gs,na.rm=TRUE), map_list=MapDetails_List, land_color="black",
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







# modified function from Jim to change border colour of countries
plot_variable <-
  function( Y_gt, map_list, panel_labels, projargs='+proj=longlat', map_resolution="medium",
            file_name="density", working_dir=paste0(getwd(),"/"), Format="png", Res=200, add=FALSE,
            outermargintext=c("Eastings","Northings"), zlim=NULL, col, mar=c(0,0,2,0), oma=c(4,4,0,0),
            legend_x=c(0,0.05), legend_y=c(0.05,0.45), cex.legend=1, mfrow, land_color="grey",
            n_cells, xlim, ylim, country=NULL, contour_nlevels=0, fun=mean, ...){
    
    ###################
    # Settings and inputs
    ###################
    
    # Check for problems and fill in missing stuff
    if( is.vector(Y_gt)){
      Y_gt = matrix(Y_gt, ncol=1)
    }
    if( is.null(zlim)){
      zlim = range(Y_gt, na.rm=TRUE)
    }
    if( missing(map_list) || is.null(map_list$MapSizeRatio) ){
      MapSizeRatio = c(3, 3)
    }else{
      MapSizeRatio = map_list$MapSizeRatio
    }
    if( !("PlotDF" %in% names(map_list)) ) stop("Check input `map_list`")
    Y_gt = Y_gt[ map_list$PlotDF[which(map_list$PlotDF[,'Include']>0),'x2i'], , drop=FALSE]
    if(missing(n_cells) || is.null(n_cells)) n_cells = nrow(Y_gt)
    if( missing(mfrow) ){
      mfrow = ceiling(sqrt(ncol(Y_gt)))
      mfrow = c( mfrow, ceiling(ncol(Y_gt)/mfrow) )
    }
    if( missing(panel_labels) ){
      panel_labels = rep("", ncol(Y_gt))
    }
    if( length(panel_labels) != ncol(Y_gt) ){
      warning( "panel_labels and `ncol(Y_gt)` don't match: Changing panel_labels'")
      panel_labels = 1:ncol(Y_gt)
    }
    if( missing(col)){
      col = colorRampPalette(colors=c("darkblue","blue","lightblue","lightgreen","yellow","orange","red"))
    }
    if( is.function(col)){
      col = col(1000)
    }
    if( !any(is.na(c(legend_x,legend_y))) ){
      if( any(c(legend_x,legend_y) > 1.2) | any(c(legend_x,legend_y) < -0.2) ){
        stop("Check values for `legend_x` and `legend_y`")
      }
    }
    # Location of extrapolation-grid cells
    loc_g = map_list$PlotDF[which(map_list$PlotDF[,'Include']>0),c('Lon','Lat')]
    
    # CRS for original and new projections
    CRS_orig = sp::CRS( '+proj=longlat' )
    CRS_proj = sp::CRS( projargs )
    
    # Data for mapping
    #map_data = rnaturalearth::ne_coastline(scale=switch(map_resolution, "low"=110, "medium"=50, "high"=10, 50 ))# , continent="america")
    map_data = rnaturalearth::ne_countries(scale=switch(map_resolution, "low"=110, "medium"=50, "high"=10, 50), country=country)
    map_data = sp::spTransform(map_data, CRSobj=CRS_proj)
    
    ###################
    # Make panel figure
    ###################
    
    # Define device
    Par = list( mfrow=mfrow, mar=mar, oma=oma, ...)
    if(Format=="png"){
      png(file=paste0(working_dir,file_name,".png"),
          width=Par$mfrow[2]*MapSizeRatio[2],
          height=Par$mfrow[1]*MapSizeRatio[1], res=Res, units='in')
      on.exit( dev.off() )
    }
    if(Format=="jpg"){
      jpeg(file=paste0(working_dir,file_name,".jpg"),
           width=Par$mfrow[2]*MapSizeRatio[2],
           height=Par$mfrow[1]*MapSizeRatio[1], res=Res, units='in')
      on.exit( dev.off() )
    }
    if(Format%in%c("tif","tiff")){
      tiff(file=paste0(working_dir,file_name,".tif"),
           width=Par$mfrow[2]*MapSizeRatio[2],
           height=Par$mfrow[1]*MapSizeRatio[1], res=Res, units='in')
      on.exit( dev.off() )
    }
    if(add==FALSE) par( Par )
    
    # Loop across columns (years)
    for( tI in 1:ncol(Y_gt) ){
      # Read extrapolation grid
      Points_orig = sp::SpatialPointsDataFrame( coords=loc_g, data=data.frame("y"=Y_gt[,tI]), proj4string=CRS_orig )
      
      # Reproject to Lat-Long
      Points_LongLat = sp::spTransform( Points_orig, sp::CRS('+proj=longlat') )
      
      # Re-project to plotting CRS
      Points_proj = sp::spTransform( Points_orig, CRS_proj )
      
      # Interpolate to raster
      # library(plotKML)
      cell.size = mean(diff(Points_proj@bbox[1,]),diff(Points_proj@bbox[2,])) / floor(sqrt(n_cells))
      Raster_proj = plotKML::vect2rast( Points_proj, cell.size=cell.size, fun=fun )
      if(missing(xlim)) xlim = Raster_proj@bbox[1,]
      if(missing(ylim)) ylim = Raster_proj@bbox[2,]
      Zlim = zlim
      if(is.na(Zlim[1])) Zlim = range(Y_gt[,tI],na.rm=TRUE)
      image( Raster_proj, col=col, zlim=Zlim, xlim=xlim, ylim=ylim )
      
      # Plot maps using rnaturalearth
      sp::plot( map_data, col=land_color, add=TRUE, border='grey20')
      
      # Title and box
      title( panel_labels[tI], line=0.1, cex.main=ifelse(is.null(Par$cex.main), 1.5, Par$cex.main), cex=ifelse(is.null(Par$cex.main), 1.5, Par$cex.main) )
      box()
      
      # Add contour lines
      if( contour_nlevels > 0 ){
        contour( Raster_proj, add=TRUE, nlevels=contour_nlevels )
      }
      
      # Include legend
      if( !any(is.na(c(legend_x,legend_y))) & (tI==ncol(Y_gt) | is.na(zlim[1])) ){
        xl = (1-legend_x[1])*par('usr')[1] + (legend_x[1])*par('usr')[2]
        xr = (1-legend_x[2])*par('usr')[1] + (legend_x[2])*par('usr')[2]
        yb = (1-legend_y[1])*par('usr')[3] + (legend_y[1])*par('usr')[4]
        yt = (1-legend_y[2])*par('usr')[3] + (legend_y[2])*par('usr')[4]
        if( diff(legend_y) > diff(legend_x) ){
          align = c("lt","rb")[2]
          gradient = c("x","y")[2]
        }else{
          align = c("lt","rb")[1]
          gradient = c("x","y")[1]
        }
        plotrix::color.legend(xl=xl, yb=yb, xr=xr, yt=yt, legend=round(seq(Zlim[1],Zlim[2],length=4),1), rect.col=col, cex=cex.legend, align=align, gradient=gradient)
      }
    }
    
    # Margin text
    if(add==FALSE) mtext(side=1, outer=TRUE, outermargintext[1], cex=1.75, line=par()$oma[1]/2)
    if(add==FALSE) mtext(side=2, outer=TRUE, outermargintext[2], cex=1.75, line=par()$oma[2]/2)
    
    # return stuff as necessary
    return( invisible(list("Par"=Par, "cell.size"=cell.size, "n_cells"=n_cells, "xlim"=xlim, "ylim"=ylim)) )
  }
