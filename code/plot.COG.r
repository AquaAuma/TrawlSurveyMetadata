rm(list=ls())


# Libraries
library(readr)
library(tidyverse)
library(ggrepel)

# Load data
cog <- read_csv("data/vast/cog.csv", quote = "\\\"")
names(cog) <- c('m','Year','cog','se')

cog <- cbind(subset(cog, m==1),subset(cog, m==2))
names(cog) <- c('m','year','long','se.long','m2','year2','lat','se.lat')
cog$m <- cog$year2 <- cog$m2 <- NULL
cog$latc <- cog$lat-cog$lat[1] 
cog$longc <- cog$long-cog$long[1]

plot(cog$long,cog$lat, type='l')

circles <- read.table(text="        x           y     sizes
    0 0 10", header=TRUE)

ii <- cut(cog$year, breaks = seq(min(cog$year), max(cog$year), len = 100), include.lowest = TRUE)
color <- colorRampPalette(c("blue", "skyblue2", "lightgoldenrodyellow","lightsalmon","indianred4"))(99)[ii]
cog$color <- color

windows()
ggplot(cog, aes(x=longc, y=latc, colour=year)) + geom_path(lwd=3) +
  theme_bw() + geom_point(size=3) +
  ylim(-70,70) + xlim(-220,220) +
  scale_color_gradient(low='blue', high='lightgreen') +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        line = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="none") + ylab('') + xlab('') +
  theme(axis.text = element_blank(),
        axis.title = element_text(size=14))+
  geom_vline(xintercept=0, lwd=1.25) + geom_hline(yintercept=0, lwd=1.25)

