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
eez <- readOGR(dsn = "data/eezs",layer="eez_v11")

# plot EEZs
ggplot() + geom_polygon(data=eez, aes(x=long, y=lat, group=group), fill='lightblue', color="black") 


########################################
#### 2. Load stock boundaries shapefiles
########################################
# load list of species
spe <- read.csv('data/species.range.covered.50+.csv')

# load metadata boundaries stocks
ram.metadata <- read_excel("data/RAMstocks/ramldb_v3.8_stock_boundary_table_v2_formatted.xlsx")
ram.metadata <- ram.metadata %>% 
  dplyr::select(assessid, species) %>% 
  filter(species %in% spe$Species) # only 8 species out of 10

world <- map_data("world")
pdf(file='figures/Transboundary.Maps.pdf')
for (i in 1:nrow(ram.metadata)){

  # load shapefiles for one stock
  stock <- readOGR(dsn = 'data/RAMstocks', layer=ram.metadata$assessid[i])

  ggplot() + geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  geom_polygon(data=stock, aes(x=long, y=lat, group=group), fill='blue', color="black") 
  
  # intersect beween stock and EEZs
  tr <- intersect(eez, stock)
  
  print(ggplot() + geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
    geom_polygon(data=tr, aes(x=long, y=lat, group=group), fill='blue', color="black") +
    ggtitle(paste(tr$stockid[1],tr$species[1], sep=' ')) + theme_bw())

}
dev.off()                