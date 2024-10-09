#### this script organizes, tidies, and maps daily bird counts in dawn and day flocks
#### should be figure #3 in the manuscript
#### communicates the abundance (in mass, in no. indiv) and duration of species


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
  theme(plot.margin = unit(c(0,0,0,0), "cm"),
        strip.background = element_blank(),
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
lilDF <- read.csv('data/colpa_raw.csv')

### wrangle the tall db... gather, filter out monospp flocks
lilDF <- gather(lilDF, key="SPECIES", value="COUNT", 8:20) #gather
nrow(lilDF) #24414
lilDF <- lilDF %>% filter(!(SPP==0)) #filter out monospp flocks 
nrow(lilDF) #19175
100*19175/24414 # 78.4% of recorded data entries are legit mixed species aggregations
lilDF <- lilDF %>% filter(!(COUNT=="NA")) #filter out zero counts
nrow(lilDF) #5590
head(lilDF) #check it
write.csv(lilDF, 'data/lilDF.csv')
class(lilDF$HOUR) # character

###### need to remove DBPA data, there are only 0s now bc they're all monospp flocks
DF <- read.csv('data/survey_talldb.csv')
head(DF)

#### subset full data set for just time/hour occurrence observation data 
# first remove unnecessary columns 
survey <- DF %>% 
  filter(!(SPECIES=="DBPA")) %>% #filter out DBPA flocks 
  subset(select = c(SPECIES, TIMEad, DURATION_kg_hr)) # subset full dataset for only columns of interest
head(survey)

class(survey$TIMEad) # check on data format of "time" col
# returns [1] "character", need convert to time/date
#survey$TIME <- as.POSIXct(survey$TIME, format="%H:%M:%S")
survey$TIMEad <- as.POSIXct(survey$TIMEad, format="%H:%M")
class(survey$TIMEad) # check format: returns [1] "POSIXct" --> success! 
# now we can make some plots :)

# rescale Brewer palette to 13 categories
colourCount = length(unique(survey$SPECIES)) 
getPalette = colorRampPalette(brewer.pal(11, "Spectral")) # interpolate colors to fit the 12 parrot species
Spectral12 <- getPalette(colourCount) # 12 color Spectral palette 

#make in another Brewer palette too
getPalette = colorRampPalette(brewer.pal(11, "YlGnBu")) # interpolate different color palette
Spectral12v2 <- getPalette(colourCount) # 12 color Spectral palette 


# make col plot
ggplot(survey, aes(x = TIMEad, y = DURATION_kg_hr, group = SPECIES, color = SPECIES, alpha = 0.5)) +
  themeKV + theme(axis.text.y = element_text(size = 8),
                  axis.text.x = element_text(size = 8),
                  axis.title.x = element_text(size = 9),
                  axis.title.y = element_text(size = 9),
                  #legend.position=c(.16,.14),
                  #legend.key.height = unit(0.4, 'cm')
                  ) +
  geom_col(aes(fill = SPECIES), 
           alpha = 0.5) + 
  scale_color_manual(values = Spectral13) +
  scale_fill_manual(values = Spectral13) +
  scale_x_datetime(breaks = breaks_width("2 hour"), 
                   date_labels = "%H") + # set the time scales and drop date info
  xlab("time after dawn") +
  ylab("duration (kg hr-1)") +
  scale_y_continuous( breaks=pretty_breaks()) +
  facet_wrap(~SPECIES, ncol=2, scales = "free_y")


# try different way
# read in summarized survey data
DF2 <- read.csv('data/survey_histogr.csv')
DF2 <- DF2 %>% filter(!(SPECIES == "DBPA"))

# subset full data set for just time/hour occurrence observation data 
# first remove unnecessary columns 
survey2 <- subset(DF2, select = c(SPECIES, TIMEad, KG_HR_DAY)) # subset full dataset for only columns of interest
class(survey2$TIMEad) # check on data format of "time" col
# returns [1] "character", need convert to time/date
#survey2$TIME <- as.POSIXct(survey2$TIME, format="%H:%M:%S")
survey2$TIMEad <- as.POSIXct(survey2$TIME, format="%H:%M")
class(survey2$TIMEad) # check format: returns [1] "POSIXct" --> success! 
# now we can make some plots :)

# reorder by duration most to fewest
#survey2$SPECIES <- factor(survey2$SPECIES, levels=c("MEPA", "BHPA", "RGMA", "GRMA", "YCPA", "SCMA", "BYMA", "WEPA", "CWPA", "OCPA", "DHPA", "WBPA", "DBPA"))
#reorder in order of appearance
survey2$SPECIES <- factor(survey2$SPECIES, levels=c("YCPA", "GRMA", "WEPA", "BHPA", "BYMA", "OCPA", "DHPA", "WBPA", "MEPA", "SCMA", "RGMA", "CWPA"))


p1 <- ggplot(survey2, aes(x = TIMEad, y = KG_HR_DAY, group = SPECIES, fill = SPECIES)) +
  themeKV + theme(legend.position = "none",
                  axis.text.y = element_text(size = 7),
                  axis.text.x = element_text(size = 8),
                  axis.title.x = element_text(size = 9),
                  axis.title.y = element_text(size = 9),
                  strip.text.x = element_text(size = 7),
                  panel.spacing.y = unit(-0.25, "lines"),) + # trying to reduce vertical spacing between panels
  # see: https://stackoverflow.com/questions/3681647/ggplot-how-to-increase-spacing-between-faceted-plots
  geom_area(alpha=0.85) +
#  scale_fill_manual(values = rev(Spectral12)) +
  scale_fill_manual(values = rev(Spectral12v2)) +
  geom_line(linewidth = 0.25) +
  scale_x_datetime(breaks = breaks_width("1 hour"), 
                   date_labels = "%H") + # set the time scales and drop date info
  xlab("hour after dawn") +
  ylab("abundance (kg hr-1 day-1)") +
  scale_y_continuous(breaks = pretty_breaks()) +
  facet_wrap(~SPECIES, ncol=2, scales = "free_y") +
  geom_vline(xintercept = as.POSIXct("00:00", format="%H:%M"), alpha = 0.25, size = 0.25, color = "#000000") +
  geom_vline(xintercept = as.POSIXct("02", format="%H"), alpha = 0.25, size = 0.25, color = "#000000")
p1



# sum duration for each group, in the KG_HR_DAY col
surveytotal <- survey2 %>% # see https://stackoverflow.com/questions/58197990/r-dplyr-grouping-and-adding-new-column
  group_by(SPECIES) %>% 
  summarise(total_KG=sum(KG_HR_DAY))
# then determine the hour with the max value for each species abundance
# use dplyr: https://stackoverflow.com/questions/24237399/how-to-select-the-rows-with-maximum-values-in-each-group-with-dplyr
sp_t_max <- survey2 %>% 
  group_by(SPECIES) %>%
  filter(KG_HR_DAY == max(KG_HR_DAY)) %>%
  arrange(SPECIES,TIMEad,KG_HR_DAY)
# join 2 DFs together
survey3 <- left_join(surveytotal, sp_t_max, by ='SPECIES') # see: https://www.guru99.com/r-dplyr-tutorial.html 
survey3 #check result 
survey3 <- survey3[!duplicated(survey3$SPECIES), ] #remove duplicate rows (there are >1 max values)
survey3 #check result 

# build palette
colourCount = length(unique(survey3$SPECIES)) # but first expand the 11 brewer palette categories
getPalette = colorRampPalette(brewer.pal(11, "Spectral")) # interpolate colors to fit the 13 parrot species
Spectral13 <- getPalette(colourCount) # 13 color Spectral palette 

# now make plot
# highlight clusters of activity w/scatter plot of max value vs hour 
lims <- as.POSIXct(strptime(c("0","9"), format = "%H")) # manually set x axis time limits
p2 <- ggplot(survey3, aes(x = TIMEad, y = KG_HR_DAY, group = SPECIES, fill = SPECIES)) +
  themeKV + theme(legend.position = "none",
                  axis.text.y = element_text(size = 7),
                  axis.text.x = element_text(size = 7),
                  axis.title.x = element_text(size = 9),
                  axis.title.y = element_text(size = 8),) +
  geom_point(shape = 16, size = 3, alpha = 0.8,
             aes(color = fct_reorder(SPECIES,total_KG))) +
  scale_color_manual(values = rev(Spectral12)) + # reverse the order of the palette to match p1
  geom_point(shape = 1,size = 3, colour = "black", stroke = 0.25,) +
  scale_y_continuous(trans = log_trans(), 
                     breaks = c(0.001, 0.01, 0.1, 1, 10, 100),
                     limits = c(0.0007,10),
                     labels = comma) +
  scale_x_datetime(breaks = breaks_width("1 hour"), 
                   date_labels = "%H",
                   limits =lims) + # set the time scales and drop date info
  annotate("text", x =  as.POSIXct("01:30", format="%H:%M"), y = 7, # hardwire coord locations for mapping using POSIX
           label = "morning", alpha = 0.75, size = 3)+ 
  annotate("text", x =  as.POSIXct("07:30", format="%H:%M"), y = 1, 
           label = "afternoon", alpha = 0.75, size = 3)+
  xlab("hour after dawn") +
  ylab("max biomass (kg hr-1 dy-1)")



# make plots with cumulative abundance expressed by mass 
p3 <- ggplot(survey3, aes(y = fct_reorder(SPECIES,total_KG), x = total_KG, group = SPECIES, fill = SPECIES)) +
  themeKV + theme(legend.position = "none",
                  axis.text.y = element_text(size = 7),
                  axis.text.x = element_text(size = 7),
                  axis.title.x = element_text(size = 9),
                  axis.title.y = element_text(size = 0),) +
  geom_point(shape = 16, size = 3, alpha = 0.8,
             aes(color = fct_reorder(SPECIES,total_KG))) +
  scale_color_manual(values = rev(Spectral12)) + # reverse the order of the palette to match p1
  geom_point(shape = 1,size = 3, colour = "black", stroke = 0.25,) +
  scale_x_continuous(breaks = seq(0, 20, by = 2), 
                     #limits = c(0,16),
                     ) +
  #scale_x_datetime(breaks = breaks_width("1 hour"), 
  #                 date_labels = "%H") + # set the time scales and drop date info
  # xlab("hour of day") +
  xlab("tot. biomass (kg hr-1 dy-1)")



#### make new plot with #birds instead of abundance by mass
# read in body mass data
massvol <- read.csv('data/body_brain.csv')
# subset full data set for just species and mass data, remove unnecessary columns 
mass <- subset(massvol, select = c(CODE, MASS_g)) # subset full dataset for only columns of interest
# join 2 DFs together
# Rename second column so colnames match
colnames(mass)[1] ="SPECIES"
mass_ind <- left_join(survey3, mass, by ='SPECIES') # see: https://www.guru99.com/r-dplyr-tutorial.html 
mass_ind <- subset(mass_ind, select = c(SPECIES, total_KG, MASS_g)) # subset full dataset for only columns of interest
mass_ind$indiv <- mass_ind$total_KG * 1000/mass_ind$MASS_g # convert from survey mass to individuals birds 


p4 <- ggplot(mass_ind, aes(y = fct_reorder(SPECIES,indiv), x = indiv, group = SPECIES, fill = SPECIES)) +
  themeKV + theme(legend.position = "none",
                  axis.text.y = element_text(size = 7),
                  axis.text.x = element_text(size = 7),
                  axis.title.x = element_text(size = 9),
                  axis.title.y = element_text(size = 0),) +
  geom_point(shape = 16, size = 3, alpha = 0.8,
             aes(color = fct_reorder(SPECIES,total_KG))) +
  scale_color_manual(values = rev(Spectral12)) + # reverse the order of the palette to match p1
  geom_point(shape = 1,size = 3, colour = "black", stroke = 0.25,) +
  scale_x_continuous(breaks = seq(0, 50, by = 5), 
                     #limits = c(0,45),
                     ) +
  #scale_x_datetime(breaks = breaks_width("1 hour"), 
  #                 date_labels = "%H") + # set the time scales and drop date info
  # xlab("hour of day") +
  xlab("tot. birds (indiv. hr-1 dy-1)")


# patch them together
layout <- "
AAAB
AAAC
AAAD"
p1 + p2 + p3 + p4 +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'a') # add panel labels a, b, c... etc


# END  