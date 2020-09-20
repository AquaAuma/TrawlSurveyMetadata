rm(list=ls())

library(ggplot2)
library(data.table)
library(rgdal)
library(tidyverse)
library(reshape2)
library(egg)
library(psych)
library(dplyr)

###########################
#### 1. Load Aquamaps
###########################
# Load FAO shp file
fao <- readOGR(dsn = "data/fao",layer="FAO_AREAS")

# Load Aquamaps data
fao.zone <- c(21,27,31,34,37,41,47,51,57,58,61,67,71,77,81,87)

dat.fao <- data.frame()
for (i in 1:length(fao.zone)){
  dat <- data.frame()
  for (j in 1:3){
    file1 <- read_csv(paste('data/aquamaps/spp',j,'.FAO',fao.zone[i],'.csv',sep=''))
    file1 <- file1 %>% 
      mutate(Species=paste(Genus, Species, rep=' '),
             FAO=fao.zone[i])
    
    setnames(file1, old=c('Center Lat','Center Long'), new=c('lat','long'))
    dato <- file1
    coordinates(dato) <- ~ long + lat
    proj4string(dato) <- proj4string(fao)
    aqua.fao <- over(dato, fao)
    aqua.fao <- aqua.fao %>% 
      dplyr::select(F_AREA)
    file1 <- cbind(file1, aqua.fao)  
    #file1 <- file1 %>% 
    #  filter(F_AREA==fao.zone[i])
    dat <- rbind(dat, file1)
    rm(file1, aqua.fao, dato)

  }
  dat.fao <- rbind(dat.fao, dat)
  rm(dat)
}

setnames(dat.fao, old=c('C-Square Code','Overall Probability'),
         new=c('square','probability'))

dat.fao$Species <- substr(dat.fao$Species, start=1, stop=nchar(dat.fao$Species)-2)

# Minimum probability:0.1
dat.fao <- dat.fao %>% 
  dplyr::select(-square, -Genus, -F_AREA) %>% 
  filter(probability>=0.5)

# Group species which are present on several FAO areas
spp.multiple <- c('Gadus morhua','Melanogrammus aeglefinus','Mugil cephalus','Trichiurus lepturus','Thyrsites atun',
                  'Gadus chalcogrammus','Merluccius productus','Harpadon nehereus','Macruronus magellanicus', 'Cephalopholis boenak')
dat <- subset(dat.fao, !Species %in% spp.multiple)
for (i in 1:length(spp.multiple)){
  sub.dat <- subset(dat.fao, Species==spp.multiple[i])
  faos <- c(sort(unique(sub.dat$FAO)))
  if(length(faos)==2){right.faos <- paste(faos[1], faos[2], sep=',')}
  if(length(faos)==3){right.faos <- paste(faos[1], faos[2], faos[3], sep=',')}
  if(length(faos)==4){right.faos <- paste(faos[1], faos[2], faos[3], faos[4], sep=',')}
  sub.dat$FAO <- right.faos
  sub.dat <- sub.dat %>% distinct()
  dat <- rbind(dat, sub.dat)
  rm(sub.dat, faos, right.faos)
}

# Add species code ASFIS 3 letters
codes <- read.csv("data/ASFIS_sp_2019.txt")
codes <- codes %>% 
  dplyr::select(X3A_CODE,Scientific_name) %>% 
  rename(ASFIS=X3A_CODE,
         Species=Scientific_name)

SPP <- sort(unique(dat$Species))
codes <- subset(codes, Species %in% SPP)
nrow(codes) == length(SPP)
dat <- left_join(dat, codes, by='Species')


###########################
#### 2. Convex hull
###########################
bts <- readOGR(dsn = "data/metadata/Metadata_18062020.shp",layer="Metadata_18062020")

# Merge convex hull with the FAO catch data
dato <- dat
coordinates(dato) <- ~ long + lat
proj4string(dato) <- proj4string(fao)
aqua.bts <- over(dato, bts)
dat <- cbind(dat, aqua.bts)

colOA <- list("Publicly available" = "blue",
              "Partly publicly available" = "orange",
              "Available upon request" = "purple",
              "Not publicly available"= "red",
              "Incomplete metadata" = "black",
              "NA"="grey")

dat <- dat %>%
  mutate(Spe.FAO = paste(Species, FAO, sep=' ')) %>% 
  dplyr::select(-link, -email, -Contact, -Providr, -minLat, -maxLat, -minLong, -maxLong, -minYear, -maxYear, -maxDpth, -minDpth)


# plot all species range overlap with convex hull
world <- map_data("world")
codes$Species <- as.character(codes$Species)
codes$ASFIS <- as.character(codes$ASFIS)
codes <- codes[order(codes$Species),]

pdf(file = "figures/SI.Appendix5.18.06.pdf")
for(i in 1:nrow(codes)){
  minlat <- min(dat[dat$Species==codes$Species[i],]$lat)
  maxlat <- max(dat[dat$Species==codes$Species[i],]$lat)
  minlong <- min(dat[dat$Species==codes$Species[i],]$long)
  maxlong <- max(dat[dat$Species==codes$Species[i],]$long)
  
  print(
    ggplot() + 
  geom_tile(data=dat[dat$Species==codes$Species[i],], aes(x=long, y=lat), fill='orange', col='orange')+
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(ylim=c(minlat-5,maxlat+5), xlim=c(minlong-5, maxlong+5)) + theme_bw() + ylab('') + xlab('') +
  theme(axis.text=element_text(size=14)) +
  geom_polygon(data=bts, aes(y=lat, x=long, group=group), fill=adjustcolor('black', alpha=0.5)) +
  ggtitle(paste(codes$Species[i],' (',codes$ASFIS[i],')',  sep=''))
  )
  
  rm(minlat, maxlat, minlong, maxlong)
}
dev.off()


########################################
#### 3. Integrate status of accessbility
########################################
#Per species
dat$ASFIS <- as.character(dat$ASFIS)
cov.sur <- table(as.character(dat$ASFIS), dat$Survey, useNA="ifany")
#NA = no ovelap between surveys and distribution


# Number of surveys per species
cov.sur <- data.frame(apply(cov.sur>0, 1, sum))
cov.sur$SpeFAO <- row.names(cov.sur)
cov.sur$FAO <- str_sub(cov.sur$SpeFAO, -2)
row.names(cov.sur) <- NULL
names(cov.sur)[1] <- 'NumberSurveys'
cov.sur$FAO <- as.factor(cov.sur$FAO)


# Number of surveys + OA status per species
cov.sur.oa <- table(dat$ASFIS, dat$Survey, dat$Opn_ccs, useNA="ifany")
cov.nboa <- apply(cov.sur.oa>0, c(1,3), sum)
colnames(cov.nboa)[ncol(cov.nboa)] <- "NA"

cov.nboa <- data.frame(cov.nboa)
names(cov.nboa) <- c('Available upon request','Incomplete metadata','Not publicly available',
                     'Partly publicly available','Publicly available','NA')
cov.nboa$FAOspe <- row.names(cov.nboa)
row.names(cov.nboa) <- NULL
cov.nboa <- cov.nboa %>% 
    dplyr::select(-'NA') %>% 
    gather("OA", "NumberSurveys", -FAOspe)
cov.nboa$NumberSurveys <- as.numeric(as.vector(cov.nboa$NumberSurveys))


#Percentage of coverage of species distribution per surveys
tapply(!is.na(dat$Survey), dat$ASFIS, sum)/table(dat$ASFIS)


#Divided per open access status
cov.spe <- table(dat$ASFIS, dat$Opn_ccs, useNA="ifany")/as.numeric(table(dat$ASFIS))
colnames(cov.spe)[ncol(cov.spe)] <- "NA"

cov.spe <- as.data.frame.matrix(cov.spe)
cov.spe$FAOspe <- row.names(cov.spe)
row.names(cov.spe) <- NULL
cov.spe <- cov.spe %>% 
  gather("OA", "Prop", -FAOspe)
cov.spe$Prop <- as.numeric(as.vector(cov.spe$Prop))

cov.spe <- cov.spe %>% 
  filter(OA !='NA')

cov <- left_join(cov.spe, cov.nboa, by=c('FAOspe','OA'))

sorting <- cov %>%
  dplyr::group_by(FAOspe) %>% 
  summarize(Prop = sum(Prop))
sorting <- sorting[order(sorting$Prop),]


###############################
# 4. Make figure2 in manuscript
###############################
ggnbr <- ggplot(cov, aes(x=reorder(FAOspe, -Prop), y=NumberSurveys, fill=OA)) + geom_bar(stat="identity", color="black") +
  theme_bw() + scale_fill_manual(values=c('purple','black','red','orange','blue')) +
  theme(axis.text.x=element_text(color = "black", size=8, angle=90, vjust=0.5, hjust=0.8)) + 
  ylab('Number of Surveys') + xlab('') + theme(legend.position ='') +
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=20),axis.title.y=element_blank(),
        axis.title.x=element_text(size=20)) + coord_flip() + theme(axis.text.x = element_text(angle=0),
                                                                   panel.grid.major = element_blank(),
                                                                   panel.grid.minor = element_blank()) +
  geom_vline(xintercept=10.5, lwd=2, lty=2) + annotate("rect", xmin = 10.5, xmax = 38, ymin = 0, ymax = 60,alpha = .25) + 
  scale_y_continuous(expand = c(0, 0))

gghabitat <- ggplot(cov, aes(x=reorder(FAOspe, -Prop), y=Prop, fill=OA)) + geom_bar(stat="identity", color="black") +
  theme_bw() + scale_fill_manual(labels=c('Available upon request','Incomplete metadata',
                                          'Not publicly available','Partly publicly available',
                                          'Publicly available'), 
                                 values=c('purple','black','red','orange','blue')) +
  theme(axis.text.x=element_text(color = "black", size=12, angle=90, vjust=0.5, hjust=0.8)) + 
  ylab('Proportion of habitat covered') + xlab('')  +
  theme(legend.position = c(0.50,0.90)) + theme(axis.text.y=element_text(size=20),axis.text.x=element_text(size=20),
                                               axis.title=element_text(size=20), legend.text = element_text(size=20),
                                               panel.grid.major = element_blank(),
                                               panel.grid.minor = element_blank()) + 
  labs(fill='') + coord_flip() + theme(axis.text.x = element_text(angle=0)) +
  geom_hline(yintercept=0.5, lwd=2, lty=2) + scale_y_continuous(expand = c(0, 0))

# stats for paper
max(sorting$Prop)
cov.sum <- cov %>% 
  group_by(FAOspe) %>% 
  summarize(Prop=sum(Prop), NumberSurveys=sum(NumberSurveys)) %>% 
  filter(Prop>=0.5)

max(cov.sum$Prop)
min(cov.sum$NumberSurveys)
max(cov.sum$NumberSurveys)
mean(cov.sum$NumberSurveys)
median(cov.sum$NumberSurveys)
geometric.mean(cov.sum$NumberSurveys)


#################################
# 5. Transboundary species ranges
#################################
# Load EEZs shp file
# publicly available for download there: https://www.marineregions.org/downloads.php
eez <- readOGR(dsn = "data/eez",layer="eez_v11_lowres")

# most 10 spp covered by survey data
cov.sum <- left_join(cov.sum, codes, by=c('FAOspe'='ASFIS'))

# get aquamaps data for those spp
dat <- dat %>% 
  dplyr::select(Species, ASFIS, lat, long, probability) %>% 
  filter(Species %in% cov.sum$Species)

# overlay with eez shapefile
cov.sum$EEZ <- NA

for (i in 1:nrow(cov.sum)){
  print(cov.sum$FAOspe[i])
  sub.dat <- subset(dat, Species==cov.sum$Species[i])
  xx <- sub.dat
  coordinates(xx) <- ~ long + lat
  proj4string(xx) <- proj4string(eez)
  tr <- over(xx, eez)
  tr <- tr %>% 
    filter(!is.na(GEONAME))
  cov.sum$EEZ[i] <- length(unique(tr$GEONAME))
}

ggeez <- ggplot(data=cov.sum, aes(x=reorder(FAOspe,-Prop), y=EEZ)) + geom_bar(stat='identity', fill='grey40') +
  theme_bw() +  theme(axis.text.x=element_text(color = "black", size=8, angle=90, vjust=0.5, hjust=0.8)) + 
  ylab('Number of EEZs/species') + xlab('') + theme(legend.position ='') +
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=20),axis.title.y=element_blank(),
        axis.title.x=element_text(size=20)) + coord_flip() + theme(axis.text.x = element_text(angle=0),
                                                                   panel.grid.major = element_blank(),
                                                                   panel.grid.minor = element_blank()) +
  geom_vline(xintercept=10.5, lwd=2, lty=2) + annotate("rect", xmin = 10.5, xmax = 38, ymin = 0, ymax = 32,alpha = .25) + 
  scale_y_continuous(expand = c(0, 0)) #+
  # add colors for spp detailed in the figure after
  #geom_bar(data=cov.sum[cov.sum$Species=='Gadus morhua',], stat='identity', fill='grey40', color='indianred3', size=1.5) +
  #geom_bar(data=cov.sum[cov.sum$Species=='Merluccius merluccius',], stat='identity', fill='grey40', color='forestgreen',size=1.5)

png(file='figures/figure2.09.2020.png', height=800, width=1200)
print(egg::ggarrange(gghabitat, ggnbr, ggeez, labels=c('','',''), nrow=1, ncol=3))
dev.off()


#########################
# 6. For other thresholds
#########################
ggnbr <- ggplot(cov, aes(x=reorder(FAOspe, -Prop), y=NumberSurveys, fill=OA)) + geom_bar(stat="identity", color="black") +
  theme_bw() + scale_fill_manual(values=c('purple','black','red','orange','blue')) +
  theme(axis.text.x=element_text(color = "black", size=8, angle=90, vjust=0.5, hjust=0.8)) + 
  ylab('Number of Surveys') + xlab('') + theme(legend.position ='') +
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=20),axis.title.y=element_blank(),
        axis.title.x=element_text(size=20)) + coord_flip() + theme(axis.text.x = element_text(angle=0),
                                                                   panel.grid.major = element_blank(),
                                                                   panel.grid.minor = element_blank()) +
  scale_y_continuous(expand = c(0, 0))

gghabitat <- ggplot(cov, aes(x=reorder(FAOspe, -Prop), y=Prop, fill=OA)) + geom_bar(stat="identity", color="black") +
  theme_bw() + scale_fill_manual(labels=c('Available upon request','Incomplete metadata',
                                          'Not publicly available','Partly publicly available',
                                          'Publicly available'), 
                                 values=c('purple','black','red','orange','blue')) +
  theme(axis.text.x=element_text(color = "black", size=12, angle=90, vjust=0.5, hjust=0.8)) + 
  ylab('Proportion of habitat covered') + xlab('')  +
  theme(legend.position = c(0.35,0.90)) + theme(axis.text.y=element_text(size=20),axis.text.x=element_text(size=20),
                                                axis.title=element_text(size=20), legend.text = element_text(size=20),
                                                panel.grid.major = element_blank(),
                                                panel.grid.minor = element_blank()) + 
  labs(fill='') + coord_flip()+ theme(axis.text.x = element_text(angle=0)) +
  geom_hline(yintercept=0.5, lwd=2, lty=2) + scale_y_continuous(expand = c(0, 0))


png(file='figures/appendix5.figure5.2.png', height=800, width=1200)
print(egg::ggarrange(gghabitat, ggnbr, labels=c('',''), nrow=1, ncol=2))
dev.off()

# stats for paper
max(sorting$Prop)
cov.sum <- cov %>% 
  group_by(FAOspe) %>% 
  summarize(Prop=sum(Prop), NumberSurveys=sum(NumberSurveys)) %>% 
  filter(Prop>=0.5)

max(cov.sum$Prop)
min(cov.sum$NumberSurveys)
max(cov.sum$NumberSurveys)
mean(cov.sum$NumberSurveys)
median(cov.sum$NumberSurveys)
geometric.mean(cov.sum$NumberSurveys)


