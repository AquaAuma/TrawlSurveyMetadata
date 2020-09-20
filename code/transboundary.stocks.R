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
  geom_tile(data=aquamaps, aes(x=Center.Long, y=Center.Lat), fill='grey20', alpha=0.5)


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

cod <- cod + geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="grey20") +  
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_label(data=stockList, aes(x = long, y = lat, label = num), size = 3, alpha=0.6, col='white', fill='indianred3', fontface='bold')

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
  geom_tile(data=aquamaps, aes(x=Center.Long, y=Center.Lat), fill='grey20', alpha=0.5)


# load shapefiles of hake stocks and plot
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

merlu <- merlu + geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="grey20") + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_label(data=stockList, aes(x = long, y = lat, label = num), size = 3, alpha=0.6, col='white', fill='forestgreen', fontface='bold')


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