#### this script is exploring the morphometric data 
#### from parrots, macaws, and parakeets measured in museums


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


# my custom ggplot theme
# I cannot stand the default thick grey lines
themeKV <- theme_few()+
  theme(strip.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(colour = "black", margin = margin(0.2, unit = "cm")),
        axis.text.y = element_text(colour = "black", margin = margin(c(1, 0.2), unit = "cm")),
        axis.ticks.x = element_line(colour = "black"), axis.ticks.y = element_line(colour = "black"),
        axis.ticks.length=unit(-0.15, "cm"),element_line(colour = "black", linewidth=.5),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=.5),
        legend.title=element_blank(),
        strip.text=element_text(hjust=0))


#### read in morphometric data
# setwd("/Users/kylevanhoutan/colpa_flock/")
morphs <- read.csv('data/museo_morphs.csv')

#### subset full data set for just wing data 
# focusing here on dispersal ability, so pulling "wing_Hwi" 
# wing_Hwi = hand wing index from Claramunt & Wright 2017, https://doi.org/10.1201/9781315120454
morph_HWi <- subset(morphs, MORPH == "wing_Hwi")
morph_HWi <- morph_HWi[!(morph_HWi$CODE == ""), ]  # remove blank entries in species CODE, for congeners that haven't been rescaled and assigned a CODE


# make a density plot, facet by species CODE
# but first expand the 11 brewer palette categories
# interpolate colors to fit the 13 parrot species
colourCount = length(unique(morph_HWi$CODE))
getPalette = colorRampPalette(brewer.pal(11, "Spectral"))


# then make the plot and call in the expanded Brewer pal
ggplot(morph_HWi, aes(x = MEASURE, fill = CODE)) + 
  themeKV + theme(legend.position = "none") +
  scale_fill_manual(values = getPalette(colourCount)) +
  #  scale_fill_brewer(palette = "Spectral") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank()) +
    geom_density() +
  facet_wrap(~CODE, ncol=3, scales = "free_y") +
  scale_x_continuous(breaks = seq(20, 50, by = 5))

# same data, different plot
# make a ridgeline plot without facets
# again use above expanded Brewer palette
ggplot(morph_HWi, aes(x = MEASURE, y = fct_reorder(CODE,MEASURE), fill = fct_reorder(CODE,MEASURE))) + 
# both y and fill are reordered by CODE's median value of MEASURE 
  themeKV + theme(legend.position = "none") +
  scale_fill_manual(values = getPalette(colourCount)) +
  geom_density_ridges(scale = 2.5, alpha = 0.85, size = 0.25, rel_min_height = 0.01, bandwidth = 1.2) +
  stat_summary(geom = "text", fontface = "bold", alpha = 0.5, size = 3, vjust = -1, hjust = 3,
               fun = "median", aes(label = round(after_stat(x), 1))) +
  scale_x_continuous(breaks = seq(20, 60, by = 5)) + 
  xlab("hand wing index") + ylab("species")


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
