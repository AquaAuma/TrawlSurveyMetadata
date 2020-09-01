rm(list=ls())

library(rgdal)
library(ggplot2)
library(readxl)
library(tidyr)
library(tidyverse)
library(raster)

##########################
#### 1. Load EEZs geo data
##########################
# Load EEZs shp file
eez <- readOGR(dsn = "data/eez",layer="eez_v11")

# plot EEZs
ggplot() + geom_polygon(data=eez2, aes(x=long, y=lat, group=group), fill='lightblue', color="black") 


########################################
#### 2. Load stock boundaries shapefiles
########################################
# load list of species
spe <- read.csv('data/species.range.covered.50+.csv')

# load metadata boundaries stocks
ram.metadata <- read_excel("data/RAMstocks/ramldb_v3.8_stock_boundary_table_v2_formatted.xlsx")
ram.metadata <- ram.metadata %>% 
  dplyr::select(assessid, stockid, species) %>% 
  filter(species %in% spe$Species) # only 8 species out of 10

world <- map_data("world")
pdf(file='figures/Socks.Maps.pdf')
for (i in 1:nrow(ram.metadata)){

  # load shapefiles for one stock
  stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[i])

  eez2 <- raster::subset(eez, Y_1>50)
  print(ggplot() + 
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
    geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
    ggtitle(paste(ram.metadata$stockid[1],ram.metadata$species[1], sep=' ')) + theme_bw() +
    geom_polygon(data=eez, aes(x=long, y=lat, group=group), fill='lightblue', color="black")) 
  
  # intersect beween stock and EEZs
  #tr <- intersect(eez, stock)
  
  #print(ggplot() + geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
   # geom_polygon(data=tr, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  #  ggtitle(paste(tr$stockid[1],tr$species[1], sep=' ')) + theme_bw())

}
dev.off()


### Plot one stock only
# create list of labels for each EEZ
idList <- as.data.frame(eez$SOVEREIGN1)
centroids.df <- as.data.frame(coordinates(eez))
idList <- cbind(idList, centroids.df)
names(idList) <- c('SOVEREIGN1', 'long','lat')

# load shapefiles for one stock
stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[1])
ggplot() + 
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(50,65), xlim=c(-180,180)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[1],ram.metadata$species[1], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('United States','Russia'),], aes(x=long, y=lat, group=group), col='grey', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('United States','Russia'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[2])
ggplot() + 
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(50,65), xlim=c(-180,-130)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[2],ram.metadata$species[2], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('United States'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('United States'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[3])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(50,85), xlim=c(-50,120)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[3],ram.metadata$species[3], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Norway', 'Russia','Denmark', 'Iceland', 'United Kingdom'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Norway', 'Russia','Denmark', 'Iceland', 'United Kingdom'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[4])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(50,85), xlim=c(-50,120)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[4],ram.metadata$species[4], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Norway', 'Russia','Denmark', 'Iceland', 'United Kingdom'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Norway', 'Russia','Denmark', 'Iceland', 'United Kingdom'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[5])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(50,85), xlim=c(-50,120)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[5],ram.metadata$species[5], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Norway', 'Russia','Denmark', 'Iceland', 'United Kingdom'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Norway', 'Russia','Denmark', 'Iceland', 'United Kingdom'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[6])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(30,60), xlim=c(-100,-30)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[6],ram.metadata$species[6], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('United States','Canada'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('United States','Canada'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[7])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(30,60), xlim=c(-100,-30)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[7],ram.metadata$species[7], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('United States','Canada'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('United States','Canada'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[8])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(30,60), xlim=c(-100,-30)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[8],ram.metadata$species[8], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('United States','Canada'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('United States','Canada'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[9])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(30,60), xlim=c(-100,-30)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[9],ram.metadata$species[9], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('United States','Canada'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('United States','Canada'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[10])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(30,60), xlim=c(-100,-30)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[10],ram.metadata$species[10], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('United States','Canada'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('United States','Canada'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[11])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(30,60), xlim=c(-100,-30)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[11],ram.metadata$species[11], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('United States','Canada'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('United States','Canada'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[12])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(30,60), xlim=c(-100,-50)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[12],ram.metadata$species[12], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('United States','Canada'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('United States','Canada'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[13])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(30,60), xlim=c(-100,-40)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[13],ram.metadata$species[13], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('United States','Canada'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('United States','Canada'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[14])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(30,60), xlim=c(-100,-40)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[14],ram.metadata$species[14], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('United States','Canada'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('United States','Canada'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[15])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(30,60), xlim=c(-100,-40)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[15],ram.metadata$species[15], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('United States','Canada'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('United States','Canada'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[16])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(30,60), xlim=c(-100,-40)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[16],ram.metadata$species[16], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('United States','Canada'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('United States','Canada'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[17])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(30,60), xlim=c(-150,-100)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[17],ram.metadata$species[17], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('United States','Canada'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('United States','Canada'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[18])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(30,60), xlim=c(-150,-100)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[18],ram.metadata$species[18], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('United States','Canada'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('United States','Canada'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[19])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(30,60), xlim=c(-150,-100)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[19],ram.metadata$species[19], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('United States','Canada'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('United States','Canada'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[21])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(-60,-30), xlim=c(-100,-50)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[21],ram.metadata$species[21], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Argentina','United Kingdom'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Argentina','United Kingdom'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[22])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(-40,-20), xlim=c(0,50)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[22],ram.metadata$species[22], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('South Africa','Namibia'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('South Africa','Namibia'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[23])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(30,60), xlim=c(-80,-30)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[23],ram.metadata$species[23], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Canada','United States'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Canada','United States'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[24])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(30,60), xlim=c(-80,-30)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[24],ram.metadata$species[24], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Canada','United States'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Canada','United States'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[25])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(30,60), xlim=c(-100,-40)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[25],ram.metadata$species[25], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Canada','United States'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Canada','United States'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[26])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(30,60), xlim=c(-100,-40)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[26],ram.metadata$species[26], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Canada','United States'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Canada','United States'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[27])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(30,60), xlim=c(-100,-40)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[26],ram.metadata$species[26], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Canada','United States'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Canada','United States'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[28])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(30,60), xlim=c(-100,-40)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[28],ram.metadata$species[28], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Canada','United States'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Canada','United States'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[29])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(30,60), xlim=c(-100,-40)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[29],ram.metadata$species[29], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Canada','United States'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Canada','United States'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[30])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(50,80), xlim=c(-50,0)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[30],ram.metadata$species[30], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Iceland'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Iceland'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[31])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(50,80), xlim=c(-50,20)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[31],ram.metadata$species[31], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Denmark','Iceland','United Kingdom'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Denmark','Iceland','United Kingdom'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[32])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(50,80), xlim=c(-50,0)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[32],ram.metadata$species[32], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Iceland','Denmark'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Iceland','Denmark'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[33])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(50,80), xlim=c(-50,20)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[33],ram.metadata$species[33], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Iceland','Denmark','United Kingdom'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Iceland','Denmark','United Kingdom'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[34])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(50,80), xlim=c(-50,20)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[34],ram.metadata$species[34], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Iceland','Denmark','United Kingdom'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Iceland','Denmark','United Kingdom'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[35])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(-60,-30), xlim=c(140,190)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[35],ram.metadata$species[35], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('New Zealand','Australia'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('New Zealand','Australia'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[36])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(30,50), xlim=c(-25,25)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[36],ram.metadata$species[36], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('France','Spain','Morocco','Algeria','Tunisia','Italia'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('France','Spain','Morocco','Algeria','Tunisia','Italia'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[37])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(30,45), xlim=c(-20,20)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[37],ram.metadata$species[37], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('France','Spain','Morocco','Algeria','Tunisia','Italy','Portugal'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('France','Spain','Morocco','Algeria','Tunisia','Italy','Portugal'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[38])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(35,50), xlim=c(0,40)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[38],ram.metadata$species[38], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('France','Spain','Algeria','Tunisia','Italy','Greece','Slovenia','Montenegro','Croatia','Albania'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('France','Spain','Algeria','Tunisia','Italy','Greece','Slovenia','Montenegro','Croatia','Albania'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[39])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(30,50), xlim=c(0,40)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[39],ram.metadata$species[38], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('France','Spain','Algeria','Tunisia','Italy','Greece','Slovenia','Montenegro','Croatia','Albania','Libya','Egypt'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('France','Spain','Algeria','Tunisia','Italy','Greece','Slovenia','Montenegro','Croatia','Albania','Libya','Egypt'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[40])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(30,50), xlim=c(0,40)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[40],ram.metadata$species[40], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('France','Spain','Algeria','Tunisia','Italy','Greece','Libya','Egypt'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('France','Spain','Algeria','Tunisia','Italy','Greece','Libya','Egypt'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')
