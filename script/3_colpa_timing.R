#### this script is plotting parrot chronology on collpa
#### should be ~ 3rd figure in the manuscript
#### it communicates across the study period the numbers and duration of parrots on the cliff

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
DF <- read.csv('data/duration_relative_t.csv')

# rescale Brewer palette to 13 categories
colourCount = length(unique(DF$SPECIES)) 
getPalette = colorRampPalette(brewer.pal(11, "Spectral")) # interpolate colors to fit the 13 parrot species
Spectral12 <- getPalette(colourCount) # 13 color Spectral palette 

#reorder in order of appearance
DF$SPECIES <- factor(DF$SPECIES, levels=c("BHPA", "GRMA", "YCPA", "BYMA", "WEPA", "OCPA", "DHPA", "CWPA", "SCMA", "WBPA", "MEPA", "RGMA"))

# make plot
p1 <- ggplot(DF, aes(x = TIME_rel, y = KG_HR_DAY, group = SPECIES, color = SPECIES)) +
  themeKV + theme(legend.position = "none",
                  axis.text.y = element_text(size = 7),
                  axis.text.x = element_text(size = 7),
                  axis.title.x = element_text(size = 8),
                  axis.title.y = element_text(size = 8),
                  strip.text.x = element_text(size = 6),
                  panel.spacing.y = unit(-0.5, "lines"), # trying to reduce vertical spacing between panels
                  ) +
  geom_point(aes(fill = SPECIES), 
           alpha = 0.75) + 
  scale_color_manual(values = Spectral12) +
  geom_line(alpha = 0.25, size = 2, color = '#000000',
            stat = "smooth", method = 'loess', formula = 'y~x', span = 0.65) + 
  xlab("relative time") +
  ylab("duration (kg hr-1)") +
  scale_y_continuous(breaks=pretty_breaks()) +
  facet_wrap(~SPECIES, ncol=1, scales = "free_y")

p1

loess <- ggplot_build(p1) # extract LOESS trend model values from ggplot
# see https://ggplot2.tidyverse.org/reference/ggplot_build.html
loess_vals <- loess$data[[2]] # port these into a DF
# use "loess_vals" to determine sequencing of arrivals of species to collpa
# determine the relative time with the max value for each loess model
# use dplyr: https://stackoverflow.com/questions/24237399/how-to-select-the-rows-with-maximum-values-in-each-group-with-dplyr
loess_max <- loess_vals %>% 
  group_by(PANEL) %>%
  filter(y == max(y)) 
# subset full dataset for only columns of interest
loess_max <- subset(loess_max, select = c(PANEL,x,y)) 
# since the ggplot build dropped species names, add them back in
# generate the DF
spp_order <- data.frame(SPECIES = c("BHPA", "GRMA", "YCPA", "BYMA", "WEPA", "OCPA", "DHPA", "CWPA", "SCMA", "WBPA", "MEPA", "RGMA"))
# bind it to the previous output
loess_max2 <- cbind(loess_max,spp_order)
# export the CSV
write.csv(loess_max2, file="/Users/kylevanhoutan/colpa_flock/data/loess_max2", row.names=FALSE)


# plot again in correct order, reorder from loess_max2
DF$SPECIES <- factor(DF$SPECIES, levels=c("YCPA", "GRMA", "BHPA", "BYMA", "OCPA", "WEPA", "WBPA", "DHPA", "CWPA", "SCMA", "RGMA", "MEPA"))
# make plot
p2 <- ggplot(DF, aes(x = TIME_rel, y = KG_HR_DAY, group = SPECIES, color = SPECIES)) +
  themeKV + theme(legend.position = "none",
                  axis.text.y = element_text(size = 7),
                  axis.text.x = element_text(size = 7),
                  axis.title.x = element_text(size = 8),
                  axis.title.y = element_text(size = 8),
                  strip.text.x = element_text(size = 6),
                  panel.spacing.y = unit(0, "lines"), # trying to reduce vertical spacing between panels
  ) +
  geom_point(aes(fill = SPECIES), 
             alpha = 0.75, size = 2) + 
  scale_color_manual(values = Spectral12) +
  geom_point(shape = 1,size = 2, colour = "black", stroke = 0.25,) +
  geom_line(alpha = 0.4, size = 2.5, color = '#000000',
            stat = "smooth", method = 'loess', formula = 'y~x', span = 0.65) + 
  xlab("morning flock period (relative time)") +
  ylab("duration (kg hr-1)") +
  scale_y_continuous(breaks=pretty_breaks()) +
  facet_wrap(~SPECIES, ncol=1, scales = "free_y")
p2


#### read in first landing data
# this is already in shape for plotting
lands <- read.csv('data/first_down.csv')

p3 <- ggplot(lands, aes(x= fct_infreq(FIRST_DOWN), group=FIRST_DOWN, fill=FIRST_DOWN)) +
  themeKV + theme(legend.position = "none", 
                  axis.text.x = element_text(size = 7),
                  axis.text.y = element_text(size = 7),
                  axis.title.x = element_text(size = 8),
                  axis.title.y = element_text(size = 8)) + 
  geom_bar(aes(fill = fct_infreq(FIRST_DOWN)), alpha=0.85, 
           ) +
  scale_fill_manual(values=c("#005a32", "#238b45", "#74c476", "#c7e9c0")) +
  scale_y_continuous(breaks = seq(0, 50, by = 5)) +
  xlab("species") +
  ylab("first to land (days)")
p3

layout <- "
A#
AB
A#
A#"
p2 + p3 +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'a') # add panel labels a, b, c... et

# END