#### this script is plotting the high-level stats of collpa activity and monitoring effort
#### should be one of the first figures in the manuscript
#### it communicates across the study period the diurnal activity and monitoring effort


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
library(lubridate)    # formating times dates


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

#### read in collpa data
# setwd("/Users/kylevanhoutan/colpa_flock/")
DF <- read.csv('data/collpa_activity.csv')

#### subset full data set for just time/hour occurrence observation data 
#### cleaning steps: subset cols, gather cols, remove rows
# first remove unnecessary columns 
collpa <- subset(DF, select = c(date_long, julian_day, z_dawn, y_call, x_dance, w_land, u_end)) # subset full dataset for only columns of interest
# second convert from wide to long format
collpa1 <- gather(collpa, key="event", value="time", 3:7) 
# third remove rows with "na" data
collpa1 <- collpa1[!(collpa1$time == "na"), ]
class(collpa1$time) # check on data format of "time" col
# returns [1] "character", need convert to time/date
collpa1$time <- as.POSIXct(collpa1$time, format="%H:%M:%S")
class(collpa1.time) # check format: returns [1] "POSIXct" --> success! 
# now we can make some plots :)

# I find this Brewer palette map guidance useful
# https://colorbrewer2.org/#type=diverging&scheme=Spectral&n=11

p1 <- ggplot(collpa1, aes(x = julian_day, y = time, group = event, color = event, shape = event)) +
  themeKV + theme(axis.text.y = element_text(size = 8),
                  axis.text.x = element_text(size = 8),
                  axis.title.x = element_text(size = 9),
                  axis.title.y = element_text(size = 9),
                  legend.position=c(.16,.14),
                  legend.key.height = unit(0.4, 'cm')) + 
  geom_line(aes(color=event), stat="smooth", method = "loess", formula = y ~ x, span = 0.85, 
            se = FALSE, linewidth = 2, alpha = 0.5)  +
  scale_color_manual(values=c("#fdae61", "#f46d43", "#9e0142", "#5e4fa2", "#3288bd")) + 
  geom_point(size = 2.2, alpha = 0.6, stroke = 0.5) +
  scale_shape_manual(values = c(16, 1, 16, 1, 16)) +
  scale_x_continuous(breaks = seq(170, 280, by = 10)) +
  scale_y_datetime(breaks = breaks_width("20 min"), date_labels = "%H:%M") + # set the time scales and drop date info
  ylab("time of day (hr:min)") +
  xlab("Julian day")


#### make 2 plots for the duration of flocks dancing and foraging
#### subset full data set for just stage duration data + wrangle 

# first try as a facet with both plots
dance_eat <- subset(DF, select = c(date_long, julian_day, dawn_to_dance, duration_dance, duration_collpa)) # remove all the time occurrence data 
dance_eat1 <- gather(dance_eat, key="duration", value="time", 3:5) # convert from wide to long format
dance_eat1 <- dance_eat1[!(dance_eat1$time == "na"), ] # remove rows with "na" data
dance_eat1$time <- as.POSIXct(dance_eat1$time, format="%H:%M:%S") # convert from character to time/date

# try one faceted plot with both durations
p2 <- ggplot(dance_eat1, aes(x = time, fill = duration)) +
  themeKV + theme(legend.position = "none", 
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.text.x = element_text(size = 9)) + 
  geom_density(size = 0.5, alpha = 0.5, adjust = 0.5) +
  scale_fill_manual(values=c("#fdae61", "#9e0142", "#3288bd")) +
  facet_wrap(~duration, ncol=1, scales = "free")

layout <- "
AAB
AAB"
p1 + p2 +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'a') # add panel labels a, b, c... etc


#### now try as 2 separate plots
# no need to use gather or remove NA
# fist plot dance duration
dance_eat$duration_dance <- as.POSIXct(dance_eat$duration_dance, format="%H:%M:%S") # convert from character to time/date
p3 <- ggplot(dance_eat, aes(x = duration_dance, fill="#9e0142")) +
  themeKV + theme(legend.position = "none", 
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.text.x = element_text(size = 8),
                  axis.title.x = element_text(size = 9),
                  axis.title.y = element_text(size = 9)) + 
  geom_density(size = 0.5, alpha = 0.5, adjust = 0.5) +
  scale_fill_manual(values=c("#9e0142")) +
  scale_x_datetime(breaks = breaks_width("4 min"), date_labels = "%M") + # set the time scales and drop date info
  xlab("dance duration (min)")

#second plot is duration of foraging flock on collpa
dance_eat$duration_collpa <- as.POSIXct(dance_eat$duration_collpa, format="%H:%M:%S") # convert from character to time/date
p4 <- ggplot(dance_eat, aes(x = duration_collpa, fill="#fdae61")) +
  themeKV + theme(legend.position = "none", 
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.text.x = element_text(size = 8),
                  axis.title.x = element_text(size = 9),
                  axis.title.y = element_text(size = 9)) + 
  geom_density(size = 0.5, alpha = 0.5, adjust = 0.5) +
  scale_fill_manual(values=c("#fdae61")) +
  scale_x_datetime(breaks = breaks_width("20 min"), date_labels = "%H:%M") + # set the time scales and drop date info
  xlab("collpa duration (hr:min)")

#third plot of the time between dawn and first flock dance
dance_eat$dawn_to_dance <- as.POSIXct(dance_eat$dawn_to_dance, format="%H:%M:%S") # convert from character to time/date
p5 <- ggplot(dance_eat, aes(x = dawn_to_dance, fill="#fdae61")) +
  themeKV + theme(legend.position = "none", 
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.text.x = element_text(size = 8),
                  axis.title.x = element_text(size = 9),
                  axis.title.y = element_text(size = 9)) + 
  geom_density(size = 0.5, alpha = 0.5, adjust = 0.5) +
  scale_fill_manual(values=c("#3288bd")) +
  scale_x_datetime(breaks = breaks_width("10 min"), date_labels = "%M") + # set the time scales and drop date info
  xlab("dawn to dance (min)")



#### read in monitoring effort data
DF1 <- read.csv('data/monitor_hours.csv')
# sum the total person monitoring hours across the project, for each calendar day
DF1 <- DF1 %>% 
  mutate(day_tothrs = rowSums(.[3:26])/2) %>% # divide by 2 as the breaks are 30min and we want hours
  mutate(cumul_hours = cumsum(day_tothrs)) # create a cumulative summary running through monitor days 


p6 <- ggplot(DF1, aes(x=DAY, y=cumul_hours)) +
  themeKV + theme(legend.position = "none", 
                  axis.text.x = element_text(size = 7),
                  axis.text.y = element_text(size = 7),
                  axis.title.x = element_text(size = 8),
                  axis.title.y = element_text(size = 8)) + 
  geom_area(fill="#238b45", alpha=0.75) +
  geom_line(linewidth = 0.5) +
  scale_x_continuous(breaks = seq(170, 280, by = 10)) +
  scale_y_continuous(breaks = seq(0, 800, by = 100)) +
  xlab("Julian day") +
  ylab("cumulative effort (hrs)")


p7 <- ggplot(DF1, aes(x=DAY, y=cumul_hours)) +
  themeKV + theme(legend.position = "none", 
                  axis.text.x = element_text(size = 7),
                  axis.text.y = element_text(size = 7),
                  axis.title.x = element_text(size = 8),
                  axis.title.y = element_text(size = 8)) + 
  geom_area(fill="#238b45", alpha=0.75) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = seq(170, 280, by = 10)) +
  scale_y_continuous(breaks = seq(0, 800, by = 100)) +
  xlab("Julian day") +
  ylab("cumulative effort (hrs)")


layout <- "
AAAAABB
AAAAABB
AAAAABB
AAAAABB
AAAAACC
AAAAACC
AAAAACC
AAAAACC
AAAAADD
AAAAADD
AAAAADD
AAAAADD"
p1 + 
  p4 + p3 + p5 + 
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'a') # add panel labels a, b, c... etc


