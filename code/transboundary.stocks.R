rm(list=ls())

library(rgdal)
library(ggplot2)
library(readxl)
library(tidyr)
library(tidyverse)
library(raster)
library(readr)

##########################
#### 1. Load EEZs geo data
##########################
# Load EEZs shp file
# publicly available for download there: https://www.marineregions.org/downloads.php
eez <- readOGR(dsn = "data/eez",layer="eez_v11_lowres")

# plot EEZs
#ggplot() + geom_polygon(data=eez2, aes(x=long, y=lat, group=group), fill='lightblue', color="black") 


########################################
#### 2. Map of cod stocks
########################################
# load metadata boundaries stocks
# stock spatial boundaries are available for download there: https://marine.rutgers.edu/~cfree/ram-legacy-stock-boundary-database/
ram.metadata <- read_excel("data/RAMstocks/ramldb_v3.8_stock_boundary_table_v2_formatted.xlsx")
ram.metadata <- ram.metadata %>% 
  dplyr::select(assessid, stockid, species) %>% 
  filter(species =='Gadus morhua')

world <- map_data("world")

### Plot one stock only
# create list of labels for each EEZ
idList <- as.data.frame(eez$SOVEREIGN1)
centroids.df <- as.data.frame(coordinates(eez))
idList <- cbind(idList, centroids.df)
names(idList) <- c('SOVEREIGN1', 'long','lat')

# load aquamaps habitat for cod
aquamaps <- read.csv('data/aquamaps/cod.csv')
aquamaps <- aquamaps %>% 
  filter(Overall.Probability>0)

# list of concerned EEZs
countries <- c('United States','Canada','Denmark','United Kingdom','Norway','Sweden','Germany','Ireland','Iceland',
               'France','Netherlands','Belgium','Russia','Finland','Estonia','Latvia','Poland','Lithuania','Spain','Portugal')
eez2 <- subset(eez, SOVEREIGN1 %in% countries)
cod <- ggplot() + 
  geom_polygon(data=eez2, aes(x=long, y=lat, group=group), col='grey', alpha=0.25) +
  coord_quickmap(ylim=c(38,80), xlim=c(-80,60)) + theme_bw() + ylab('') + xlab('') + theme_bw() +
  geom_tile(data=aquamaps, aes(x=Center.Long, y=Center.Lat), fill='grey30', alpha=0.5)


# load shapefiles of cod stocks and plot
stockList <- data.frame()
for (i in 1:21){
  stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[i])
  # store centroid of each stock polygonto add labels
  centroids.df <- as.data.frame(coordinates(stock))
  centroids.df <- cbind(i, ram.metadata$assessid[i], centroids.df)
  stockList <- rbind(stockList, centroids.df)
  # add stock shapefile on the map
  cod <- cod + geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='indianred3', colour='indianred3', alpha=0.5)
  rm(centroids.df, stock)
}
names(stockList) <- c('num', 'name', 'long', 'lat')

cod <- cod + geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +  
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_label(data=stockList, aes(x = long, y = lat, label = num), size = 3, alpha=0.6, col='white', fill='indianred3', fontface='bold')

#cod

### Plot number of EEZs crossed by each stock
#stockID <- stockList %>% 
#  dplyr::select(num, name)
#write.csv(stockID, file='data/cod.stocks.EEZs.csv')

cod.eez <- read_delim("data/cod.stocks.EEZs.csv", ";", escape_double = FALSE, trim_ws = TRUE)

bar.eez.cod <- ggplot() + geom_bar(data=cod.eez, aes(x=num, y=EEZ), stat="identity", color="indianred3", fill='indianred3', alpha=0.6) + theme_bw() +
 scale_x_continuous(breaks=c(1:21),labels=c(1:21)) + scale_y_continuous(breaks=seq(from=0, to=10, by=2), labels=seq(from=0, to=10, by=2)) +
  ylab('Number EEZs/stock') + xlab('') +
  theme(axis.title.y = element_text(face='bold',size=10),
        axis.title.x = element_text(colour='blue', size=15),
        axis.text.x = element_text(face='bold',color='indianred3',size=10, angle=45),
        axis.text.y = element_text(size=10, color='black'),
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank())

#bar.eez.cod
pdf('figures/cod.stocks.pdf')
egg::ggarrange(cod, bar.eez.cod,
               nrow=2, ncol=1, heights=c(1,0.3))
dev.off()

########################################
#### 3. Map of hake stocks
########################################
# load metadata boundaries stocks
# stock spatial boundaries are available for download there: https://marine.rutgers.edu/~cfree/ram-legacy-stock-boundary-database/
ram.metadata <- read_excel("data/RAMstocks/ramldb_v3.8_stock_boundary_table_v2_formatted.xlsx")
ram.metadata <- ram.metadata %>% 
  dplyr::select(assessid, stockid, species) %>% 
  filter(species =='Merluccius merluccius')

world <- map_data("world")

### Plot one stock only
# create list of labels for each EEZ
idList <- as.data.frame(eez$SOVEREIGN1)
centroids.df <- as.data.frame(coordinates(eez))
idList <- cbind(idList, centroids.df)
names(idList) <- c('SOVEREIGN1', 'long','lat')

# load aquamaps habitat for cod
aquamaps <- read.csv('data/aquamaps/merlu.csv')
aquamaps <- aquamaps %>% 
  filter(Overall.Probability>0)

# list of concerned EEZs
countries <- c('Denmark','United Kingdom','Norway','Sweden','Germany','Ireland','Iceland','France','Netherlands',
               'Belgium','Russia','Finland','Estonia','Latvia','Poland','Lithuania','Spain','Portugal',
               'Italy','Morocco','Tunisia','Algeria','Greece','Slovenia','Cyprus','Malta','Egypt','Libya',
               'Croatia','Albania','Montenegro','Bosnia','Turkey')
eez2 <- subset(eez, SOVEREIGN1 %in% countries)
merlu <- ggplot() + 
  geom_polygon(data=eez2, aes(x=long, y=lat, group=group), col='grey', alpha=0.25) +
  coord_quickmap(ylim=c(30,70), xlim=c(-20,30)) + theme_bw() + ylab('') + xlab('') + theme_bw() +
  geom_tile(data=aquamaps, aes(x=Center.Long, y=Center.Lat), fill='grey30', alpha=0.5)


# load shapefiles of cod stocks and plot
stockList <- data.frame()
for (i in 1:9){
  stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[i])
  # store centroid of each stock polygonto add labels
  centroids.df <- as.data.frame(coordinates(stock))
  centroids.df <- cbind(i, ram.metadata$assessid[i], centroids.df)
  stockList <- rbind(stockList, centroids.df)
  # add stock shapefile on the map
  merlu <- merlu + geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='forestgreen', colour='forestgreen', alpha=0.5)
  rm(centroids.df, stock)
}
names(stockList) <- c('num', 'name', 'long', 'lat')

merlu <- merlu + geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_label(data=stockList, aes(x = long, y = lat, label = num), size = 3, alpha=0.6, col='white', fill='forestgreen', fontface='bold')

#merlu

### Plot number of EEZs crossed by each stock
#stockID <- stockList %>% 
#  dplyr::select(num, name)
#write.csv(stockID, file='data/merlu.stocks.EEZs.csv')

merlu.eez <- read_delim("data/merlu.stocks.EEZs.csv", ";", escape_double = FALSE, trim_ws = TRUE)

bar.eez.merlu <- ggplot() + geom_bar(data=merlu.eez, aes(x=num, y=EEZ), stat="identity", color="forestgreen", fill='forestgreen', alpha=0.6) + theme_bw() +
  scale_x_continuous(breaks=c(1:9),labels=c(1:9)) + scale_y_continuous(breaks=seq(from=0, to=10, by=2),labels=seq(from=0, to=10, by=2)) +
  ylab('') + xlab('Stocks') +
  theme(axis.title.y = element_text(face='bold',size=10),
        axis.title.x = element_text(colour='forestgreen', size=10),
        axis.text.x = element_text(face='bold',color='forestgreen',size=10, angle=45),
        axis.text.y = element_text(size=10, color='black'),
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank())
  

#bar.eez.merlu
pdf('figures/merlu.stocks.pdf')
egg::ggarrange(merlu, bar.eez.merlu,
                     nrow=2, ncol=1, heights=c(1,0.3))
dev.off()

# plot both maps
pdf('figures/cod+merlu.pdf')
print(egg::ggarrange(cod, merlu, bar.eez.cod, bar.eez.merlu,
               nrow=2, ncol=2, widths=c(1,0.5), heights=c(1,0.25)))
dev.off()


########################################
#### 4. Load stock boundaries shapefiles
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

pdf('figures/Stocks.Maps.pdf')

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
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Algeria','Tunisia','Italy','Malta','Greece','Slovenia','Montenegro','Croatia','Albania','Libya','Egypt'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Algeria','Tunisia','Italy','Malta','Greece','Slovenia','Montenegro','Croatia','Albania','Libya','Egypt'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[40])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(30,50), xlim=c(0,40)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[40],ram.metadata$species[40], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Albania','Malta','Tunisia','Italy','Greece','Libya','Egypt','Turkey'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Albania','Malta','Tunisia','Italy','Greece','Libya','Egypt','Turkey'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[41])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(30,50), xlim=c(0,40)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[41],ram.metadata$species[41], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Algeria','Tunisia','Italy','Greece','Libya','Egypt','Turkey'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Algeria','Tunisia','Italy','Greece','Libya','Egypt','Turkey'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[42])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(30,50), xlim=c(0,40)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[42],ram.metadata$species[42], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('France','Spain','Algeria','Tunisia','Italy','Greece'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('France','Spain','Algeria','Tunisia','Italy','Greece'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[43])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(30,50), xlim=c(20,60)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[43],ram.metadata$species[43], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Bulgaria','Romania','Turkey','Russia','Ukraine','Georgia'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Bulgaria','Romania','Turkey','Ukraine','Russia','Georgia'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[44])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(30,60), xlim=c(-100,-50)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[44],ram.metadata$species[44], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Canada','United States'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Canada','United States'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[45])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(50,60), xlim=c(0,30)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[45],ram.metadata$species[45], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Denmark','Sweden','Germany','Poland'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Denmark','Sweden','Germany','Poland'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[46])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(50,70), xlim=c(0,40)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[46],ram.metadata$species[46], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Denmark','Sweden','Germany','Poland','Finland','Russia','Estonia','Lettonia'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Denmark','Sweden','Germany','Poland','Finland','Russia','Estonia','Lettonia'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[47])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(45,65), xlim=c(-20,10)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[47],ram.metadata$species[47], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Ireland','United Kingdom'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Ireland','United Kingdom'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[48])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(50,60), xlim=c(0,30)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[48],ram.metadata$species[48], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Denmark','Sweden','Germany','Norway'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Denmark','Sweden','Germany','Norway'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[49])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(45,60), xlim=c(-20,10)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[49],ram.metadata$species[49], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Ireland','United Kingdom','France'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Ireland','United Kingdom','France'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[50])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(50,65), xlim=c(-20,10)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[50],ram.metadata$species[50], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Ireland','United Kingdom','France'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Ireland','United Kingdom','France'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[51])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(35,65), xlim=c(-20,20)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[51],ram.metadata$species[51], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Ireland','United Kingdom','France','Spain','Norway','Denmark','Netherlands','Belgium','Sweden'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Ireland','United Kingdom','France','Spain','Norway','Denmark','Netherlands','Belgium','Sweden'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[52])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(40,65), xlim=c(-20,20)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[52],ram.metadata$species[52], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Ireland','United Kingdom','France','Spain','Norway','Denmark','Netherlands','Belgium','Sweden'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Ireland','United Kingdom','France','Spain','Norway','Denmark','Netherlands','Belgium','Sweden'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[53])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(40,65), xlim=c(-20,20)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[53],ram.metadata$species[53], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Ireland','United Kingdom'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Ireland','United Kingdom'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[54])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(40,65), xlim=c(-20,20)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[54],ram.metadata$species[54], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Ireland','United Kingdom','Denmark'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Ireland','United Kingdom','Denmark'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[55])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(40,65), xlim=c(-20,20)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[55],ram.metadata$species[55], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Ireland','United Kingdom','Denmark'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Ireland','United Kingdom','Denmark'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[56])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(40,65), xlim=c(-20,20)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[56],ram.metadata$species[56], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Ireland','United Kingdom','Denmark','France','Sweden','Norway','Belgium','Netherlands','Germany'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Ireland','United Kingdom','Denmark','France','Sweden','Norway','Belgium','Netherlands','Germany'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[57])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(40,65), xlim=c(-20,20)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[57],ram.metadata$species[57], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Ireland','United Kingdom','Denmark','France','Sweden','Norway','Belgium','Netherlands','Germany'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Ireland','United Kingdom','Denmark','France','Sweden','Norway','Belgium','Netherlands','Germany'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')


stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[57])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(40,65), xlim=c(-20,20)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[57],ram.metadata$species[57], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Ireland','United Kingdom','Denmark','France','Sweden','Norway','Belgium','Netherlands','Germany'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Ireland','United Kingdom','Denmark','France','Sweden','Norway','Belgium','Netherlands','Germany'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[58])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(40,65), xlim=c(-20,20)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[58],ram.metadata$species[58], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Ireland','Iceland','United Kingdom','Denmark','France','Sweden','Norway','Belgium','Netherlands','Germany'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Ireland','Iceland','United Kingdom','Denmark','France','Sweden','Norway','Belgium','Netherlands','Germany'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[59])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(40,65), xlim=c(-20,20)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[59],ram.metadata$species[59], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Ireland','Iceland','United Kingdom','Denmark','France','Sweden','Norway','Belgium','Netherlands','Germany'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Ireland','Iceland','United Kingdom','Denmark','France','Sweden','Norway','Belgium','Netherlands','Germany'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[60])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(40,65), xlim=c(-20,20)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[60],ram.metadata$species[60], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Ireland','Iceland','United Kingdom','Denmark','France','Sweden','Norway','Belgium','Netherlands','Germany'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Ireland','Iceland','United Kingdom','Denmark','France','Sweden','Norway','Belgium','Netherlands','Germany'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[61])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(40,65), xlim=c(-20,20)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[61],ram.metadata$species[61], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Ireland','Iceland','United Kingdom','Denmark','France','Sweden','Norway','Belgium','Netherlands','Germany'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Ireland','Iceland','United Kingdom','Denmark','France','Sweden','Norway','Belgium','Netherlands','Germany'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[62])
ggplot() +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(40,65), xlim=c(-20,20)) + theme_bw() + ylab('') + xlab('') +
  ggtitle(paste(ram.metadata$stockid[62],ram.metadata$species[62], sep=' ')) + theme_bw() +
  geom_polygon(data=eez[eez$SOVEREIGN1 %in% c('Ireland','Iceland','United Kingdom','Denmark','France','Sweden','Norway','Belgium','Netherlands','Germany'),], aes(x=long, y=lat, group=group), colour='black', alpha=0.5) +
  geom_text(data = idList[idList$SOVEREIGN1 %in% c('Ireland','Iceland','United Kingdom','Denmark','France','Sweden','Norway','Belgium','Netherlands','Germany'),], aes(x = long, y = lat, label = SOVEREIGN1), 
            size = 4, col='orange')

dev.off()