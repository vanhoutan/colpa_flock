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
  geom_density(size = 0.25, alpha = 0.8, adjust = 0.65) +
  scale_fill_manual(values=c("#3288bd")) +
  scale_x_continuous(breaks = seq(0, 40, by = 4)) +
  ylab("density (morning flocks)")+
  xlab("no. flushes")+
  annotate("text", x = 24, y = 0.005, label = "n = 1200", color = "white", alpha = 0.85, size = 3)
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
  scale_fill_manual(values=c("#f46d43","#fdae61")) +
  scale_x_continuous(breaks = seq(0, 100, by = 20),
                                  limits = c(0,100)) +
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
                  legend.key.height = unit(0.25, 'cm'), # shrink the native height of legend
                  legend.text = element_text(size=6),
                  ) + # reduce font size on legend
  geom_vline(xintercept = ave(fights$FIGHT_RT), alpha = 0.25, size = 1.5, color = "#000000") +
  geom_hline(yintercept = ave(fights$WIN_RT), alpha = 0.25, size = 1.5, color = "#000000") +
  geom_point(aes(fill = fct_rev(fct_reorder(SPECIES,WIN_RT))),# color code by species win rate 
             pch=21,color="black", stroke=0.25, size = 2.2, alpha = 0.8, # make point borders   
             ) +
  #geom_point(shape = 1,size = 3, colour = "black", stroke = 0.25,) +
  scale_fill_manual(values = Spectral14) +
  scale_x_continuous(breaks = seq(0, 28, by = 4), limits = c(-1,30)) + # give a little more room 
  scale_y_continuous(breaks = seq(0, 0.8, by = 0.2), limits = c(-0.05,0.9)) +
  xlab("interactions indiv-1") + 
  ylab("wins interactions-1") + 
  annotate("text", x = 0, y = 0.86, label = "II", alpha = 0.75, size = 3, fontface = "bold") +
  annotate("text", x = 0, y = 0.225, label = "IV", alpha = 0.75, size = 3, fontface = "bold") +
  annotate("text", x = 6.5, y = 0.86, label = "I", alpha = 0.75, size = 3, fontface = "bold") +
  annotate("text", x = 7, y = 0.225, label = "III", alpha = 0.75, size = 3, fontface = "bold") 
#  guides(color = guide_legend(override.aes = list(size = ))) 
p4



layout2 <- "
AB
CD"
  p1 + p2 + p3 + p4 +
  plot_layout(design = layout2) +
  plot_annotation(tag_levels = 'a') # add panel labels a, b, c... etc




#### try a hierarchy of dominance 
library(ggplot2)
displace <- read.csv('data/displacement.csv')
head(displace)

# error bar guidance
# http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization

p5 <- displace %>% 
  ggplot(aes(x=fct_reorder(WIN,ORDER), y=winrt_p,fill=LOSE))+ # prob need to manually sort from a list
  themeKV+ theme(axis.text.x = element_text(size = 7),
                 axis.title.x = element_text(size = 8),
                 axis.text.y = element_text(size = 7),
                 axis.title.y = element_text(size = 8),
                 legend.key.height = unit(0.25, 'cm'), # shrink the native height of legend
                 legend.text = element_text(size=7), # reduce font size on legend
                 #legend.position = "none", 
                 ) +
  geom_pointrange(aes(ymin=winrt_p-winrt_sd, ymax=winrt_p+winrt_sd, # combines error bar and point in one 
                      fill = fct_reorder(LOSE,-rank_q), size=tot # size is inflate for some reason, idk
                      ),
                  pch=21, color="black",stroke=0.25, # make point borders
                  alpha=0.7,
                  position = position_jitter(width = 0.2, height = 0.03))+
  scale_radius(range = c(0.4, 2.1))+ # manually re-size range of values
  scale_fill_manual(values = Spectral14) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(-0.1,1.1)) +
  geom_hline(yintercept = 0.5, alpha = 0.25, size = 1, color = "#000000") +
  ylab("win rate")+
  xlab(NULL)+
  coord_flip()
p5
# fct_reorder(LOSE,ORDER)



layout3 <- "
AAABBBEEE
CCCDDDEEE"
p1 + p3 + p2 + p4 + p5 +
  plot_layout(design = layout3) +
  plot_annotation(tag_levels = 'a') # add panel labels a, b, c... etc


# TO DO LIST:
# proper descending sort "-" using dominance
# shrink legend
# sync legend with other panel
# blanks in layout geometry
