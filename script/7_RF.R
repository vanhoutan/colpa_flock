#### this script develops the RF model and resulting ICE and PDP dataviz 

library(ggplot2)      # plotting and viz
library(plyr)         # legacy df manipulation
library(dplyr)        # variable grouping and manipulation
library(tidyr)        # gathering and spreading
library(ggthemes)     # helpful ggplot themes
library(RColorBrewer) # pretty colors
library(colorspace)
library(patchwork)    # assembling composite plots
library(caret)        # RF
library(randomForest) # duh
library(pdp)          # PDP plots


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


#### first develop plots of raw xy relationships between variables
#### here dependent variable (y) is sociality index
#### independent variables (x) are the drivers or covariates 

#### begin by reading in data, building df for RF
# check the social index data generated in the '5_sociality.R'
head(boots)
# read in covariate data, also used in '6_morphs.R'
covars <- read.csv('data/covars.csv')
head(covars)
# join them together
loroRF <- left_join(boots, covars, by = "SPECIES")
head(loroRF)

#### make first xy raw relationship plot for covars
# first taxon, boxplot
p1 <- loroRF %>% ggplot(aes(x=fct_reorder(TRIBE,-INDEX), y=INDEX)) +
  themeKV + theme(legend.position = "none") + 
  geom_boxplot(outlier.shape = NA, # remove outliers
               fatten=1, # NULL = remove median line
               color = "#9e0142", coef = 1, # whiskers sd =1 
               lwd=0.5, alpha = 0.7, # lwd = linewidth
               width=0.65) + 
  xlab("taxonomic tribe") +
  ylab("sociality index") +
  scale_y_continuous(breaks = seq(0, 1000, by = 100))

# next brain size resids scatter w/loess
p2 <- loroRF %>% ggplot(aes(x=BRAIN_Yres, y=INDEX)) +
  themeKV + theme(legend.position = "none") + 
  geom_point(alpha=0.003, size=1) +
  geom_line(stat = "smooth", method = "loess", formula = y ~ x,
            color = "#f46d43", alpha = 0.6,
            span = 0.75, se = FALSE, linewidth = 2.5, lineend = "round")+
  xlab("brain volume residuals") +
  ylab("sociality index") +
  scale_y_continuous(breaks = seq(0, 1000, by = 100),limits = c(0,600))

# wing morph (choose one)
p3 <- loroRF %>% ggplot(aes(x=WING_load, y=INDEX)) +
  themeKV + theme(legend.position = "none") + 
  geom_point(alpha=0.003, size=1) +
  geom_line(stat = "smooth", method = "loess", formula = y ~ x,
            color = "#66c2a5", alpha = 0.6,
            span = 0.75, se = FALSE, linewidth = 2.5, lineend = "round")+
  xlab("wing loading (N m s-2)") +
  #xlab("hand wing index") +
  #xlab("total wing area (m2)") +
  ylab("sociality index") +
  scale_y_continuous(breaks = seq(0, 1000, by = 100),limits = c(0,600))


# beak size
p4 <- loroRF %>% ggplot(aes(x=BEAK_cmsl, y=INDEX)) +
  themeKV + theme(axis.text.y = element_text(size = 7), axis.text.x = element_text(size = 7),
                  axis.title.x = element_text(size = 8),axis.title.y = element_text(size = 8),
                  legend.position = "none") + 
  geom_point(alpha=0.003, size=1) +
  geom_line(stat = "smooth", method = "loess", formula = y ~ x,
            color = "#3288bd", alpha = 0.6,
            span = 0.75, se = FALSE, linewidth = 2.5, lineend = "round")+
  xlab("culmen + mandible (cm)") +
  ylab("sociality index") +
  scale_y_continuous(breaks = seq(0, 1000, by = 100),limits = c(0,600))+
  scale_x_continuous(breaks = seq(0, 14, by = 2))

#### patch them all together
layout <- "
A
B
C
D"
p1 + p2 + p3 + p4 +
plot_layout(design = layout) +
plot_annotation(tag_levels = 'a') # add panel labels a, b, c... etc

#### FIN