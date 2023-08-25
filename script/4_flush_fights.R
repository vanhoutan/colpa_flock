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
                  axis.text.y = element_blank(),
                  axis.title.y = element_text(size = 9),
                  axis.ticks.y = element_blank(),
                  axis.text.x = element_text(size = 8),
                  axis.title.x = element_text(size = 9),
                  ) + 
  geom_density(size = 0.5, alpha = 0.8, adjust = 0.7) +
  scale_fill_manual(values=c("#3288bd")) +
  scale_x_continuous(breaks = seq(0, 40, by = 4)) +
  xlab("no. collpa flushes")
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
                  axis.title.y = element_blank(),
                  axis.text.x = element_text(size = 8),
                  axis.title.x = element_text(size = 9),
  ) + 
  geom_bar(fill = c("#66c2a5"), alpha =0.95) +
  scale_fill_manual(values=c("#66c2a5")) +
  scale_x_continuous(breaks = seq(0, 60, by = 8)) +
  xlab("no. sentinel alarms")
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
  themeKV + theme(#legend.position = "none", 
                  axis.text.y = element_text(size = 7),
                  axis.title.y = element_blank(),
                  axis.text.x = element_text(size = 8),
                  axis.title.x = element_text(size = 9),
  ) + 
  geom_col(alpha = 0.8) +
  scale_fill_manual(values=c("#9e0142", "#d53e4f")) +
  #scale_x_continuous(breaks = seq(0, 60, by = 8)) +
  xlab("no. flush causes")
p3


layout2 <- "
ABC"
  p1 + p2 + p3 +
  plot_layout(design = layout2) +
  plot_annotation(tag_levels = 'a') # add panel labels a, b, c... etc


