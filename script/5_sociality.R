#### this script compiles data metrics from previous scripts 
#### to assemble all the 9 components of the sociality index 
#### runs non-param bootstrap, visualizes all the results
#### will be figure #6 in the manuscript


library(ggplot2)      # plotting and viz
#library(plyr)         # legacy df manipulation
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
library(infer) # sampling


# my custom ggplot theme
themeKV <- theme_few()+
  theme(plot.margin = unit(c(0.1,0,0,0), "cm"),
        strip.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 7, colour = "black", margin = unit(c(0.15,0,0,0), "cm")),
        axis.text.y = element_text(size = 7, colour = "black", margin = unit(c(0,0.15,0,0), "cm")),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.ticks.x = element_line(colour = "black"), axis.ticks.y = element_line(colour = "black"),
        axis.ticks.length=unit(-0.15, "cm"),element_line(colour = "black", linewidth=.25),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=.25),
        legend.title=element_blank(),
        strip.text=element_text(hjust=0))


#### read in beak and wing morph data
#setwd("/Users/kylevanhoutan/colpa_flock/")
# df <- read.csv('data/sociality.csv')
df <- read.csv('data/sociality2.csv')
sociality <- df %>% gather(key="OBSERV", value="ESTIMATE", 2:10) 
sociality$OBSERV <- str_replace(sociality$OBSERV,"X", "")  # remove Xs that were added
sociality$OBSERV <- as.numeric(sociality$OBSERV)  # convert to numeric

# make 12 color Brewer palette
getPalette = colorRampPalette(brewer.pal(11, "Spectral")) # interpolate colors to fit the 13 parrot species
Spectral12 <- getPalette(12) 

# now make box plot, sorting largest to smallest by species
ggplot(sociality, aes(fill=fct_reorder(SPECIES,ESTIMATE, .fun=median), x=ESTIMATE,
                      y=fct_reorder(SPECIES,ESTIMATE, .fun=median, .desc=FALSE))) +
  themeKV + theme(axis.text.y = element_text(size = 7), axis.text.x = element_text(size = 8),
                  axis.title.x = element_text(size = 9),axis.title.y = element_text(size = 0),
                  legend.position = "none") + 
  geom_boxplot(outlier.shape = NA, coef = 0, # remove outliers and whiskers
               lwd=0.25, alpha = 0.8) +
  scale_fill_manual(values = Spectral12) + # bring in manual palette
  xlab("social index value")
 # scale_x_continuous(breaks = seq(0, 100, by = 10))


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
  scale_color_manual(values = Spectral12) + # bring in manual palette
  ylab("cumul. social index") +
  xlab("index component") +
  guides(color = guide_legend(reverse=TRUE, # reverse legend sort order to match data sort
                              override.aes = list(size=2))) # reduce point size in legend

vlines <- c(2.5, 4.5, 6.5) # define breaks between groups of sociality index factors

p1 <- ggplot(sociality, aes(x=OBSERV, y=ESTIMATE, group = SPECIES, color=fct_reorder(SPECIES,-ESTIMATE, .fun=min))) +
  themeKV + theme(axis.text.x = element_text(size = 8),
                  axis.title.x = element_text(size = 9),
                  axis.text.y = element_text(size = 8),
                  axis.title.y = element_text(size = 9),
                  legend.key.height = unit(0.35, 'cm'), # shrink the native height of legend
                  legend.text = element_text(size=7)) + # reduce font size on legend
  #geom_point(shape = 21, size = 2.5, stroke = 1.2,
   #          aes(color=fct_reorder(SPECIES,ESTIMATE, .fun=max, alpha=0.9), ))+
  geom_vline(xintercept = vlines, alpha = 0.2, size = 0.2, color = "#000000") +
  geom_line(aes(color=fct_reorder(SPECIES,-ESTIMATE, .fun=min)),
            linewidth = 3, lineend = "round", alpha = 0.75) + 
  # geom_point(shape = 21, size = 3.2, stroke = 0.25, alpha = 0.9)+
  scale_color_manual(values = Spectral12) + # bring in manual palette
  scale_x_continuous(breaks = seq(1, 10, by = 1), limits = c(1,9)) + # tighten up white space
  scale_y_continuous(breaks = seq(0, 300, by = 50), limits = c(0,250)) + 
  ylab("pi × n-1") +
  xlab("index metric") +
  annotate("text", x = 1.6, y = 245, label = "abundance", alpha = 0.75, size = 2.8)+
  annotate("text", x = 3.5, y = 245, label = "sequence", alpha = 0.75, size = 2.8)+
  annotate("text", x = 5.5, y = 245, label = "function", alpha = 0.75, size = 2.8)+
  annotate("text", x = 7.9, y = 245, label = "interaction", alpha = 0.75, size = 2.8)
p1
#  guides(color = guide_legend(reverse=TRUE,)) # reverse legend sort order to match data sort
#                              override.aes = list(size=2))) # reduce point size in legend
# consider post-process in Ai with effect/stylize/drop shadow and object/path/outline stroke


#### perform non-parametric bootstrap of social index components
#### sample and replicate with weighting scheme to equalize pulling from 4 factor categories

# first read in the sociality index component data & the weight data
df <- read.csv('data/sociality.csv')
df <- df %>% gather(key="COMPONENT", value="VALUE", 2:10) # wide to tall df
df$COMPONENT <- str_replace(df$COMPONENT,"X", "")  # remove Xs that were added
df$COMPONENT <- as.numeric(df$COMPONENT)  # convert to numeric
weight <- read.csv('data/weights.csv')
df2 <- left_join(df, weight) # join the 2 together

# run the nonparam bootstrap
set.seed(916) # ensure model result reproducibility
y=2000 # no. replicates
z=9 # 9 category factors in the index
x=9*z # no. sample draws, recursively, one for each component (z)
boots <- replicate(y, df2 %>% # y = no. replicates 
                   group_by(SPECIES) %>% # perform group operation by species
                   sample_n(size=x, replace=T, prob=NULL) %>% # no. samples, replacement YES, weighting 
                    #sample_n(size=x, replace=T, prob=WEIGHT) %>% # no. samples, replacement YES, weighting 
                     summarise(INDEX=sum(VALUE)) %>% # add all sampled components up
                   ungroup(), # undo grouping
                 simplify=FALSE) # creates a list
boots <- do.call(rbind.data.frame, boots) # turn the list output into a DF
boots$INDEX <- boots$INDEX/(x/z) # normalize value to one full set draw (n=9 components)


head(boots)
colourCount = length(unique(boots$SPECIES)) # but first expand the 11 brewer palette categories

p2 <- ggplot(boots, aes(x = INDEX, y = fct_reorder(SPECIES,INDEX), fill = fct_reorder(SPECIES,-INDEX))) + 
  # both y and fill are reordered by CODE's median value of MEASURE 
  themeKV + theme(legend.position = "none",
                  axis.text.x = element_text(size = 8),
                  axis.title.x = element_text(size = 9),
                  axis.text.y = element_text(size = 7),
                  axis.title.y = element_text(size = 9)) + # reduce font size on legend
  scale_fill_manual(values = getPalette(colourCount)) +
  geom_density_ridges(scale = 4, alpha = 0.75, linewidth = 0.35,
                      rel_min_height = 0.003, #bandwidth = 6.25,
                      ) +
  scale_x_continuous(breaks = seq(0, 400, by = 50), limits = c(0,300)) + 
  scale_y_discrete(expand = expand_scale(add = c(0.75, 1.5)))+
  xlab("M")+ylab(NULL)
p2


# patch them together
layout <- "
AAA##
AAABB
AAABB
AAABB
AAABB
AAABB
AAABB
AAABB
AAA##"
p1 + p2 + 
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'a') # add panel labels a, b, c... etc


# FIN  