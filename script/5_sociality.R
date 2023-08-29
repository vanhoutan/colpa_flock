#### this script is compiling data for the sociality index 
#### then some basic dataviz


library(ggplot2)      # plotting and viz
library(plyr)         # legacy df manipulation
library(dplyr)        # variable grouping and manipulation
library(reshape)      # legacy df manipulation
library(data.table)   # legacy functions on df 
library(tidyverse)
library(tidyr)        # gathering and spreading
library(zoo)          # roll mean
library(ggthemes)     # helpful ggplot themes
library(forcats)
library(scales)
library(RColorBrewer) # pretty colors
library(ggridges)
library(colorspace)
library(patchwork)
library(lubridate)    # formatting times dates


# my custom ggplot theme
themeKV <- theme_few()+
  theme(strip.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(colour = "black", margin = margin(0.2, unit = "cm")),
        axis.text.y = element_text(colour = "black", margin = margin(c(1, 0.2), unit = "cm")),
        axis.ticks.x = element_line(colour = "black"), axis.ticks.y = element_line(colour = "black"),
        axis.ticks.length=unit(-0.15, "cm"),element_line(colour = "black", linewidth=.25),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=.5),
        legend.title=element_blank(),
        strip.text=element_text(hjust=0))


#### read in beak and wing morph data
#setwd("/Users/kylevanhoutan/colpa_flock/")
df <- read.csv('data/sociality.csv')
sociality <- gather(df, key="OBSERV", value="ESTIMATE", 2:9) # gather wide data to make tall db

# make 13 color Brewer palette
getPalette = colorRampPalette(brewer.pal(11, "Spectral")) # interpolate colors to fit the 13 parrot species
Spectral13 <- getPalette(13) 

# now make box plot, sorting largest to smallest by species
ggplot(sociality, aes(fill=fct_reorder(SPECIES,ESTIMATE, .fun=median), x=ESTIMATE,
                      y=fct_reorder(SPECIES,ESTIMATE, .fun=median, .desc=FALSE))) +
  themeKV + theme(axis.text.y = element_text(size = 7), axis.text.x = element_text(size = 8),
                  axis.title.x = element_text(size = 9),axis.title.y = element_text(size = 0),
                  legend.position = "none") + 
  geom_boxplot(outlier.shape = NA, coef = 0, # remove outliers and whiskers
               lwd=0.25, alpha = 0.8) +
  scale_fill_manual(values = Spectral13) + # bring in manual palette
  xlab("social index value") +
  scale_x_continuous(breaks = seq(0, 100, by = 10))

