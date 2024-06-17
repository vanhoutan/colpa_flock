#### this script organizes data, plots flush and fights data
#### should be figure ~4 in the manuscript

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


# custom ggplot theme
themeKV <- theme_few()+
  theme(strip.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(colour = "black", margin = margin(0.2, unit = "cm")),
        axis.text.y = element_text(colour = "black", margin = margin(c(1, 0.2), unit = "cm")),
        axis.ticks.x = element_line(colour = "black", linewidth=.25), 
        axis.ticks.y = element_line(colour = "black", linewidth=.25),
        axis.ticks.length=unit(-0.15, "cm"), 
        element_line(colour = "black", linewidth=.5),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=.5),
        legend.title=element_blank(),
        strip.text=element_text(hjust=0))

#### read in flush and fight data from csv
flush <- read.csv('data/flush.csv')
flush1 <- subset(flush, select = c(DATE)) # subset full dataset for only date column
flush2 <- flush1 %>% group_by(DATE) %>% # Group by count of multiple columns
  summarise(total_count=n(),.groups = 'drop') %>%
  as.data.frame() # make new df with these data

#### first to do, document # flushes per day, as density plot
p1 <- ggplot(flush2, aes(x = total_count, fill=c("#3288bd"))) +
  themeKV + theme(legend.position = "none", 
                  axis.text.y = element_text(size = 0),
                  axis.title.y = element_text(size = 9),
                  axis.ticks.y = element_blank(),
                  axis.text.x = element_text(size = 8),
                  axis.title.x = element_text(size = 9),
                  ) + 
  geom_density(size = 0.5, alpha = 0.8, adjust = 0.65) +
  scale_fill_manual(values=c("#3288bd")) +
  scale_x_continuous(breaks = seq(0, 40, by = 4)) +
  ylab("density (morning flocks)")+
  xlab("no. flushes")+
  annotate("text", x = 24, y = 0.005, label = "n = 1200", color = "white", alpha = 0.75, size = 2.7)
p1


#### now count the occurrences of each species was a sentinel
flush <- read.csv('data/flush.csv')
sentinel <- subset(flush, select = c(ALARM)) # subset full dataset for only species IDs
sentinel1 <- sentinel %>% 
  filter(!(ALARM == 'NA')) # filter out NA values

# make col plot of alarm species
p2 <- ggplot(sentinel1, aes(y = fct_rev(fct_infreq(ALARM)), group=ALARM)) +
  themeKV + theme(legend.position = "none", 
                  axis.text.y = element_text(size = 7),
                  axis.title.y = element_text(size = 9),
                  axis.text.x = element_text(size = 8),
                  axis.title.x = element_text(size = 9),
  ) + 
  geom_bar(fill = c("#66c2a5"), alpha =1, width=0.98) +
  scale_fill_manual(values=c("#66c2a5")) +
  scale_x_continuous(breaks = seq(0, 60, by = 8)) +
  ylab("known sentinels") + 
  xlab("no. alarms")
p2


#### now count the occurrences of each flush cause
flush <- read.csv('data/flush.csv')
cause <- subset(flush, select = c(CAUSE, CATEGORY)) # subset full dataset for only species IDs
cause1 <- cause %>% 
  filter(!(CAUSE == 'NA')) # filter out NA values

# count number of cause occurences
cause2 <- cause1 %>% group_by(CAUSE, CATEGORY) %>% # Group by count of multiple columns
  summarise(total_count=n(),.groups = 'drop') %>%
  as.data.frame() # make new df with these data
# manually reorder to sort by CATEGORY then CAUSE
reorder = c(3,1,6,4,5,7,2) # create reorder variables
cause2$reorder = reorder # add as col to df
# dplyr frustrating me too much so gave up and just manually redid it

p3 <- ggplot(cause2, aes(x=total_count, y=fct_rev(fct_reorder(CAUSE, reorder)), fill=CATEGORY)) +
  themeKV + theme(legend.position = "none", 
                  axis.text.y = element_text(size = 8),
                  axis.title.y = element_text(size = 9),
                  axis.text.x = element_text(size = 8),
                  axis.title.x = element_text(size = 9),
  ) + 
  geom_col(alpha = 0.8, width=0.9) +
  scale_fill_manual(values=c("#fdae61", "#f46d43")) +
  scale_x_continuous(breaks = seq(0, 90, by = 10),
                                  limits = c(0,99)) +
    ylab("known cause") + 
    xlab("no. flushes")
p3



#### WE'RE AT THE FIGHTS!!!
fights <- read.csv('data/fights.csv')
# make 14 color Brewer palette
getPalette = colorRampPalette(brewer.pal(11, "Spectral")) # interpolate colors to fit the 13 parrot species
Spectral14 <- getPalette(14) 


p4 <- ggplot(fights, aes(x=FIGHT_RT, y=WIN_RT)) + 
  themeKV + theme(axis.text.x = element_text(size = 8),
                  axis.title.x = element_text(size = 9),
                  axis.text.y = element_text(size = 8),
                  axis.title.y = element_text(size = 9),
                  legend.key.height = unit(0.3, 'cm'), # shrink the native height of legend
                  legend.text = element_text(size=6)) + # reduce font size on legend
  geom_vline(xintercept = ave(fights$FIGHT_RT), alpha = 0.25, size = 1.8, color = "#000000") +
  geom_hline(yintercept = ave(fights$WIN_RT), alpha = 0.25, size = 1.8, color = "#000000") +
  geom_point(shape =16, size = 3, alpha = 0.8, 
             aes(color = fct_rev(fct_reorder(SPECIES,FIGHT_RT))), # color code by species win rate 
             ) +
  geom_point(shape = 1,size = 3, colour = "black", stroke = 0.25,) +
  scale_color_manual(values = Spectral14) +
  scale_x_continuous(breaks = seq(0, 24, by = 4), limits = c(-1,24)) + # give a little more room 
  scale_y_continuous(breaks = seq(0, 0.8, by = 0.2), limits = c(-0.05,0.9)) +
  xlab("fight rate (fights indiv-1)") + 
  ylab("win rate (wins fights-1)") + 
  annotate("text", x = 0, y = 0.85, label = "II", alpha = 0.75, size = 2.7) +
  annotate("text", x = 0, y = 0.2, label = "IV", alpha = 0.75, size = 2.7) +
  annotate("text", x = 6, y = 0.85, label = "I", alpha = 0.75, size = 2.7) +
  annotate("text", x = 6, y = 0.2, label = "III", alpha = 0.75, size = 2.7)
p4



layout2 <- "
AB
CD"
  p1 + p2 + p3 + p4 +
  plot_layout(design = layout2) +
  plot_annotation(tag_levels = 'a') # add panel labels a, b, c... etc


