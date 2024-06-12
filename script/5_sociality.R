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
# df <- read.csv('data/sociality.csv')
df <- read.csv('data/sociality2.csv')
# sociality <- gather(df, key="OBSERV", value="ESTIMATE", 2:9) # gather wide data to make tall db
sociality <- df %>% gather(key="OBSERV", value="ESTIMATE", 2:9) 
sociality$OBSERV <- str_replace(sociality$OBSERV,"X", "")  
  

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


ggplot(sociality, aes(x=OBSERV, y=ESTIMATE, group = SPECIES)) +
  themeKV + theme(axis.text.x = element_text(size = 9),
                              axis.title.x = element_text(size = 10),
                              axis.text.y = element_text(size = 9),
                              axis.title.y = element_text(size = 10),
                              legend.key.height = unit(0.4, 'cm'), # shrink the native height of legend
                              legend.text = element_text(size=7.5)) + # reduce font size on legend
  geom_line(linewidth = 0.25, alpha = 0.9) + 
  geom_line(aes(color=fct_reorder(SPECIES,ESTIMATE, .fun=max)),linewidth = 2, alpha = 0.5) + 
  geom_point(shape = 21, size = 2, stroke = 2, alpha = 0.5,
             aes(color=fct_reorder(SPECIES,ESTIMATE, .fun=max)))+
  geom_point(shape = 21, size = 3.2, stroke = 0.25, alpha = 0.9)+
  scale_color_manual(values = Spectral13) + # bring in manual palette
  ylab("cumul. social index") +
  xlab("index component") +
  guides(color = guide_legend(reverse=TRUE, # reverse legend sort order to match data sort
                              override.aes = list(size=2))) # reduce point size in legend


ggplot(sociality, aes(x=OBSERV, y=ESTIMATE, group = SPECIES)) +
  themeKV + theme(axis.text.x = element_text(size = 9),
                  axis.title.x = element_text(size = 10),
                  axis.text.y = element_text(size = 9),
                  axis.title.y = element_text(size = 10),
                  legend.key.height = unit(0.4, 'cm'), # shrink the native height of legend
                  legend.text = element_text(size=7.5)) + # reduce font size on legend
  #geom_line(linewidth = 0.25, alpha = 0.9) + 
  geom_point(shape = 21, size = 2, stroke = 1.2,
             aes(color=fct_reorder(SPECIES,ESTIMATE, .fun=max), alpha=0.9)
             )+
  geom_line(aes(color=fct_reorder(SPECIES,ESTIMATE, .fun=max)),linewidth = 3, alpha = 0.7) + 
  #geom_point(shape = 21, size = 3.2, stroke = 0.25, alpha = 0.9)+
  scale_color_manual(values = Spectral13) + # bring in manual palette
#  scale_fill_manual(values = Spectral13) + # bring in manual palette
  ylab("cumul. social index") +
  xlab("index component") +
  guides(color = guide_legend(reverse=TRUE, # reverse legend sort order to match data sort
                              override.aes = list(size=2))) # reduce point size in legend
