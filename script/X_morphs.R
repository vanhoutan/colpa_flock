#### this script is exploring the morphometric data 
#### from parrots, macaws, and parakeets measured in museums
#### interested in factors contributing to social roles and hierarchies
#### then some basic dataviz


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
# setwd("/Users/kylevanhoutan/colpa_flock/")
morphs <- read.csv('data/museo_morphs.csv')

#### subset full data set for just wing data 
# focusing here on dispersal ability, so pulling "wing_Hwi" 
# wing_Hwi = hand wing index from Claramunt & Wright 2017, https://doi.org/10.1201/9781315120454
morph_HWi <- subset(morphs, MORPH == "wing_Hwi")
morph_HWi <- morph_HWi[!(morph_HWi$CODE == ""), ]  # remove blank entries in species CODE, for congeners that haven't been rescaled and assigned a CODE

# make a density plot, facet by species CODE
colourCount = length(unique(morph_HWi$CODE)) # but first expand the 11 brewer palette categories
getPalette = colorRampPalette(brewer.pal(11, "Spectral")) # interpolate colors to fit the 13 parrot species
Spectral13 <- getPalette(colourCount) # 13 color Spectral palette 

# then make the plot and call in the expanded Brewer pal
ggplot(morph_HWi, aes(x = MEASURE, fill = CODE)) + 
  themeKV + theme(legend.position = "none") +
  scale_fill_manual(values = Spectral13) +
  #  scale_fill_brewer(palette = "Spectral") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank()) +
    geom_density(size = 0.2) +
  facet_wrap(~CODE, ncol=3, scales = "free_y") +
  scale_x_continuous(breaks = seq(20, 50, by = 5))

# same data, but using ridgeline plot w/o faceting
# again use above expanded Brewer palette
ggplot(morph_HWi, aes(x = MEASURE, y = fct_reorder(CODE,MEASURE), fill = fct_reorder(CODE,MEASURE))) + 
# both y and fill are reordered by CODE's median value of MEASURE 
  themeKV + theme(legend.position = "none") +
  scale_fill_manual(values = getPalette(colourCount)) +
  geom_density_ridges(scale = 2.5, alpha = 0.85, size = 0.2, rel_min_height = 0.01, bandwidth = 1.2) +
  stat_summary(geom = "text", fontface = "bold", alpha = 0.5, size = 3, vjust = -1, hjust = 3,
               fun = "median", aes(label = round(after_stat(x), 1))) +
  scale_x_continuous(breaks = seq(20, 60, by = 5)) + 
  xlab("hand wing index") + ylab("species")


# same data, but ridgeline plot continuous x axis fill 
p1 <- ggplot(morph_HWi, aes(x = MEASURE, y = fct_reorder(CODE,MEASURE), fill = after_stat(x))) + 
  # just y is reordered by CODE's median value of MEASURE of HWi 
  themeKV + theme(legend.position = "none",
                  axis.text.x = element_text(size = 7),
                  axis.title.x = element_text(size = 8),
                  axis.text.y = element_text(size = 7),
                  axis.title.y = element_text(size = 8),) +
  geom_density_ridges_gradient(scale = 2.5, alpha = 0.2, size = 0.2, rel_min_height = 0.01, bandwidth = 1) +
  scale_fill_gradientn(colours = c("#9e0142", "#d53e4f",  "#fdae61", "#fee08b", "#e6f598", "#abdda4", "#66c2a5", "#3288bd", "#313695")) +
  # more detail at https://ggplot2.tidyverse.org/reference/scale_gradient.html
  # also here https://r-graphics.org/recipe-colors-palette-continuous
  stat_summary(geom = "text", alpha = 0.5, size = 2.5, vjust = -1, hjust = 2.5,
               fun = "median", aes(label = round(after_stat(x), 1))) +
  scale_x_continuous(breaks = seq(25, 55, by = 5)) + 
  xlab("hand wing index") + 
  ylab("species")

# the distiller command doesn't work well here as it melds only 7 discrete colors 
# from an existing palette, like viridis or brewer
# see documentation here https://cran.r-project.org/web/packages/ggplot2/ggplot2.pdf



#### subset full data set for beak data
# focusing here on beak as a weapon, so want size
# pulling "culm_mand_SL" 
# culm_mand_SL = cumulative straight length of culmen and mandible in cm
morph_CMs <- subset(morphs, MORPH == "culm_mand_SL")
morph_CMs <- morph_CMs[!(morph_CMs$CODE == ""), ]  # remove blank entries in species CODE, for congeners that haven't been rescaled and assigned a CODE


colourCount = length(unique(morph_CMs$CODE))
getPalette = colorRampPalette(brewer.pal(11, "Spectral"))

# make ridgeline/joy plot of beak size
ggplot(morph_CMs, aes(x = MEASURE, y = fct_reorder(CODE,MEASURE), fill = fct_reorder(CODE,MEASURE))) + 
  # both y and fill are reordered by CODE's median value of MEASURE 
  themeKV + theme(legend.position = "none") +
  scale_fill_manual(values = getPalette(colourCount)) +
  geom_density_ridges(scale = 2.5, alpha = 0.85, size = 0.25, rel_min_height = 0.01, bandwidth = 0.4) +
  stat_summary(geom = "text", fontface = "bold", alpha = 0.5, size = 3, vjust = -1.5, hjust = 3,
               fun = "median", aes(label = round(after_stat(x), 1))) +
  scale_x_continuous(breaks = seq(0, 16, by = 2)) + 
  xlab("culmen + mandible (cm)") + ylab("species")


# same data, but ridgeline plot continuous x axis fill 
p2 <- ggplot(morph_CMs, aes(x = MEASURE, y = fct_reorder(CODE,MEASURE), fill = after_stat(x))) + 
  # just y is reordered by CODE's median value of MEASURE of CMs
  themeKV + theme(legend.position = "none",
                  axis.text.x = element_text(size = 7),
                  axis.title.x = element_text(size = 8),
                  axis.text.y = element_text(size = 7),
                  axis.title.y = element_text(size = 0),) +
  scale_fill_gradientn(colours = c("#9e0142", "#d53e4f",  "#fdae61", "#fee08b", "#e6f598", "#abdda4", "#66c2a5", "#3288bd", "#313695")) +
  #  scale_fill_distiller(palette = "Spectral", direction = 1) + # continuous 7 color Brewer
  geom_density_ridges_gradient(scale = 2.5, alpha = 0.6, size = 0.2, rel_min_height = 0.01, bandwidth = 0.4) +
  stat_summary(geom = "text", alpha = 0.5, size = 2.5, vjust = -1, hjust = 2.5,
               fun = "median", aes(label = round(after_stat(x), 1))) +
  scale_x_continuous(breaks = seq(0, 16, by = 2)) + 
  xlab("culmen + mandible (cm)") + 
  ylab("species")



#### subset full data set for beak data
# focusing here on beak as a weapon, so want size
# pulling "culmen_CL" 
# culmen_CL = curved culmen length in cm
morph_CCL <- subset(morphs, MORPH == "culmen_CL")
morph_CCL <- morph_CCL[!(morph_CCL$CODE == ""), ]  # remove blank entries in species CODE, for congeners that haven't been rescaled and assigned a CODE

colourCount = length(unique(morph_CCL$CODE))
getPalette = colorRampPalette(brewer.pal(11, "Spectral"))

# make ridgeline/joy plot of beak size
ggplot(morph_CCL, aes(x = MEASURE, y = fct_reorder(CODE,MEASURE), fill = fct_reorder(CODE,MEASURE))) + 
  # both y and fill are reordered by CODE's median value of MEASURE 
  themeKV + theme(legend.position = "none") +
  scale_fill_manual(values = getPalette(colourCount)) +
  geom_density_ridges(scale = 2.5, alpha = 0.9, size = 0.25, rel_min_height = 0.01, bandwidth = 0.4) +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) + 
  stat_summary(geom = "text", fontface = "bold", alpha = 0.5, size = 3, vjust = -1.5, hjust = -1.8,
               fun = "median", aes(label = round(after_stat(x), 1))) +
  xlab("curved culmen length (cm)") + ylab("species")


#### read in body mass and brain volume data
massvol <- read.csv('data/body_brain.csv')

#### bar plot of body mass
p3 <- ggplot(massvol, aes(x = MASS_g, color = MASS_g, 
                          y = fct_reorder(CODE, MASS_g))) + # reorder for geom_col
  themeKV + theme(legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 0),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 6)) + 
  geom_col(alpha = 0.85, width = 0.85) + # width controls gaps between bars
  # scale_y_continuous(breaks = seq(0, 40, by = 5)) +
  scale_x_continuous(breaks = seq(0, 1400, by = 200)) +
  xlab("body mass (g)")


#### allometric function of body mass vs brain volume
## model fit is power function, residuals form this model interpreted as proxy for cognitive aptitude
## see JR Krebs & NB Davies (1993) Introduction to behavioral ecology. Blackwell: Oxford UK, p 30  

p4 <- ggplot(massvol, aes(x = MASS_g, y = BRAIN_ml)) + 
  themeKV + theme(legend.position = "none",
                  axis.text.x = element_text(size = 7),
                  axis.title.x = element_text(size = 8),
                  axis.text.y = element_text(size = 7),
                  axis.title.y = element_text(size = 8),) +
  geom_line(alpha = 0.25, size = 3, color = '#000000',
            stat = "smooth", method = 'nls', formula = 'y~a*x^b', # power fit using non-linear squares regression
              method.args = list(start= c(a = 1,b=1)), se = FALSE) + 
  geom_point(shape =19, size = 3.2, alpha = 0.8, 
             aes(color = MASS_g)) +
  geom_point(shape = 1,size = 3.2, colour = "black", stroke = 0.25,) +
  scale_colour_gradientn(colours = c("#9e0142", "#d53e4f",  "#fdae61", "#fee08b", "#e6f598", "#abdda4", "#66c2a5", "#3288bd", "#313695")) +
  scale_y_continuous(breaks = seq(0, 25, by = 5)) +
  scale_x_continuous(breaks = seq(0, 1400, by = 200)) +
  xlab("body mass (g)") + 
  ylab("brain volume (ml)")


# try this plot a different way, ranking by CODE class
# but means have to force expand the palette

p5 <- ggplot(massvol, aes(x = MASS_g, y = BRAIN_ml)) + 
  themeKV + theme(axis.text.x = element_text(size = 7),
                  axis.title.x = element_text(size = 8),
                  axis.text.y = element_text(size = 7),
                  axis.title.y = element_text(size = 8),
                  legend.key.height = unit(0.3, 'cm'), # shrink the native height of legend
                  legend.text = element_text(size=6.5)) + # reduce font size on legend
  geom_line(alpha = 0.25, size = 3, color = '#000000',
            stat = "smooth", method = 'nls', formula = 'y~a*x^b', # power fit using non-linear squares regression
            method.args = list(start= c(a = 1,b=1)), se = FALSE) + 
  geom_point(shape = 16, size = 3.2, alpha = 0.8, 
             aes(color = fct_reorder(CODE,MASS_g))) + # color by CODE, sorted big to small by MASS_g
  scale_color_manual(values = Spectral13) +
  geom_point(shape = 1,size = 3.2, colour = "black", stroke = 0.25,) +
  scale_y_continuous(breaks = seq(0, 25, by = 5)) +
  scale_x_continuous(breaks = seq(0, 1400, by = 200)) +
  xlab("body mass (g)") + 
  ylab("brain volume (ml)") +
  guides(color = guide_legend(reverse=TRUE, # reverse legend sort order to match data sort
                              override.aes = list(size=2.6))) # reduce point size in legend
  

layout <- "
ABC"
p1 + p2 + p5 + 
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'a') # add panel labels a, b, c... etc

