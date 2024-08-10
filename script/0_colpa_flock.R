#### this script is basic reporting on collpa morning flock size and spp richness
#### should be figure #0 in the manuscript
#### will have photos composited with figures 


library(ggplot2)      # plotting and viz
library(plyr)         # legacy df manipulation
library(dplyr)        # variable grouping and manipulation
library(reshape)      # legacy df manipulation
library(data.table)   # legacy functions on df 
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

#### read in raw survey data
# setwd("/Users/kylevanhoutan/colpa_flock/")
DF <- read.csv('data/survey_talldb0.csv')


#### subset full data set for just time/hour occurrence observation data 
# first remove unnecessary columns 
survey <- subset(DF, select = c(DATE, DAY, MIN_morn, SPECIES, COUNT)) # subset full dataset for only columns of interest
head(survey)

survey1 <- survey %>% 
  filter(!(MIN_morn == 'NA')) %>%  # remove rows with NA values, after the morning flock
  filter(!(COUNT == 0)) %>% # remove rows with 0 birds counted
  filter(!(MIN_morn > 90)) # remove rows >90min, as n=1 birds from 95-130min

# count the no. birds on collpa on each day, at each 5min interval
flock_birds <- survey1 %>% # new DF with only 3 cols: DAY, MIN_morn, and new summation
  group_by(DAY, MIN_morn) %>% 
  summarise(total_birds=sum(COUNT)) %>% # total_birds is the new sum/operator col
  ungroup()

  
# count the species richness on collpa on each day, at each 5min interval
flock_spp <- survey1 %>% # repeating form immediately above
  group_by(DAY, MIN_morn) %>% 
  summarise(spp_rich=n_distinct(SPECIES)) %>% # 'n_distinct' sums unique category/character entries in group
  ungroup()
  
#now make plots with new DFs
# make box plot of birds by monitoring interval
p1 <- ggplot(flock_birds, aes(x=MIN_morn, y=total_birds)) +
  themeKV + theme(axis.text.y = element_text(size = 7), axis.text.x = element_text(size = 7),
                  axis.title.x = element_text(size = 8),axis.title.y = element_text(size = 8),
                  legend.position = "none") + 
  geom_line(stat="smooth", method = "loess", formula = y ~ x, 
            span = 0.4, se = FALSE, linewidth = 2.5, alpha = 0.5) +
  geom_jitter(alpha=0.25, color="#f768a1", shape=16, size=2, width=1.25, height=0) +  
  geom_boxplot(aes(group = MIN_morn),
               outlier.shape = NA, # remove outliers, WE HAVE JITTER
               fatten=1, # NULL = remove median line
               fill = "#7a0177", coef = 1, # whiskers sd =1 
               lwd=0.375, alpha = 0.7) +
    xlab("flock duration (min.)") +
  ylab("no. birds") +
  scale_x_continuous(breaks = seq(10, 100, by = 10), limits = c(3,92)) +
  scale_y_continuous(breaks = seq(0, 350, by = 40), limits = c(0,320))

# make box plot of spp richness by monitoring interval
p2 <- ggplot(flock_spp, aes(x=MIN_morn, y=spp_rich)) +
  themeKV + theme(axis.text.y = element_text(size = 7), axis.text.x = element_text(size = 7),
                  axis.title.x = element_text(size = 8),axis.title.y = element_text(size = 8),
                  legend.position = "none") + 
  geom_line(stat="smooth", method = "loess", formula = y ~ x, 
            span = 0.4, se = FALSE, linewidth = 2.5, alpha = 0.5) +
  geom_jitter(alpha=0.25, color="#41b6c4", shape=16, size=2, width=1.25, height=0.5) +  
  geom_boxplot(aes(group = MIN_morn),
               outlier.shape = NA, # remove outliers, WE HAVE JITTER
               fatten=1, # NULL = remove median line, we're doing LOESS
               fill = "#253494", coef = 1, # whiskers sd =1 
               lwd=0.375, alpha = 0.7) +
  xlab("flock duration (min.)") +
  ylab("no. species") +
  scale_x_continuous(breaks = seq(10, 90, by = 10), limits = c(3,92)) +
  scale_y_continuous(breaks = seq(0, 14, by = 1), limits = c(0,9.5))
p2

# patch them together
layout <- "
AB"
p1 + p2 + 
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'a') # add panel labels a, b, c... etc


# END  