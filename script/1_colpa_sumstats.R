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
library(lubridate)    # formatting times dates
library(broom)
library(ggrepel)


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
collpa1 <- collpa1[!(collpa1$time == "NA"), ]
class(collpa1$time) # check on data format of "time" col
# returns [1] "character", need convert to time/date
collpa1$time <- as.POSIXct(collpa1$time, format="%H:%M:%S")
class(collpa1$time) # check format: returns [1] "POSIXct" --> success! 
# now we can make some plots :)

# I find this Brewer palette map guidance useful
# https://colorbrewer2.org/#type=diverging&scheme=Spectral&n=11

collpa1 <- na.omit(collpa1)
p1 <- ggplot(collpa1, aes(x = julian_day, y = time, na.rm = TRUE,
                          group = event, color = event, shape = event)) +
  themeKV + theme(axis.text.y = element_text(size = 8),
                  axis.text.x = element_text(size = 8),
                  axis.title.x = element_text(size = 9),
                  axis.title.y = element_text(size = 9),
                  legend.position=c(.16,.14),
                  legend.key.height = unit(0.4, 'cm')) + 
  geom_line(aes(color=event), stat="smooth", method = "loess", formula = y ~ x, span = 0.85, 
            se = FALSE, linewidth = 2, alpha = 0.5)  +
  scale_color_manual(values=c("#fdae61", "#f46d43", "#9e0142", "#5e4fa2", "#3288bd")) + 
  geom_point(size = 3, alpha = 0.6, stroke = 0.5) +
  scale_shape_manual(values = c(16, 1, 16, 1, 16)) +
  scale_x_continuous(breaks = seq(170, 280, by = 10)) +
  scale_y_datetime(breaks = breaks_width("20 min"), date_labels = "%H:%M") + # set the time scales and drop date info
  ylab("time of day") +
  xlab("Julian day")
p1


#### bc of the apparent correlation bw sunrise and bird activity
# let's examine the formal statistical relationship
# lm() first then plot to explore
# but first need to deal with NA and time format

# write function to convert HH:MM:SS to decimal hour
decimateTime=function(timez) {
  timez=as.numeric(unlist(strsplit(timez, ":")))
  timez = (timez[1]*60+timez[2]+timez[3]/60)/60 # decimal hour of day
  return(timez)
}
# remove NA data, then apply decimateTime
head(collpa) 
collpa2 <- collpa %>% filter(!is.na(x_dance))
collpa2$x_dance <- sapply(collpa2$x_dance,decimateTime)  # do each col piecemeal
collpa2$z_dawn <- sapply(collpa2$z_dawn,decimateTime)
head(collpa2)  # looks good

# regression of "z_dawn" to "x_dance" 
fit <- collpa2 %>% 
  lm(x_dance ~ z_dawn, data = .)
# tidy(fit, conf.int = TRUE)
# summary(fit)
glance(fit) # r-squared = 0.771, p < 0.0001
# ergo beginning of dance is correlated with sunrise/dawn

# plot "z_dawn" to "x_dance" 
figtext1 <- paste("r2 =", round(glance(fit)[1],3))
figtext2 <- "p < 0.0001"
p8 <- collpa2 %>% 
  ggplot(aes(z_dawn,x_dance)) +
  themeKV + theme(legend.position = "none", 
                  axis.text.x = element_text(size = 8),
                  axis.text.y = element_text(size = 8),
                  axis.title.x = element_text(size = 9),
                  axis.title.y = element_text(size = 9)) + 
  geom_point(color = "#9e0142", size = 3, shape = 16, alpha = 0.4) + 
  geom_point(size = 2.8, shape = 21, alpha = 0.25) +
  geom_line(method = "lm", stat="smooth", linewidth = 1, color = "black", alpha=0.75) +
  scale_y_continuous(breaks = seq(5.5, 7, by = 0.2)) +
  scale_x_continuous(breaks = seq(5, 6, by = 0.1)) +
  xlab("dawn (hour)") + ylab("dance begin (hour)") +
  annotate("text", x = 5.2, y = 6.55, label = figtext1, alpha = 0.75, size = 3)+
  annotate("text", x = 5.2, y = 6.4, label = figtext2, alpha = 0.75, size = 3)
p8

#### --> for Fig 2 survey stats let's benchmark hour of day relative to dawn
# instead of hour from midnight, or absolute clock time




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
  annotate("text", x =  as.POSIXct("05", format="%M"), y = 0.0026, # hardwire coord locations for mapping using POSIX
           label = "3.0", alpha = 0.75, size = 2.7)+
  scale_x_datetime(breaks = breaks_width("4 min"), date_labels = "%M") + # set the time scales and drop date info
  xlab("dance duration (min)") +
  ylab("")
p3

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
  annotate("text", x =  as.POSIXct("57", format="%M"), y = 0.00037, # hardwire coord locations for mapping using POSIX
          label = "53.0", alpha = 0.75, size = 2.7)+ 
  scale_fill_manual(values=c("#fdae61")) +
  scale_x_datetime(breaks = breaks_width("10 min"), date_labels = "%M") + # dropping the hour for more spacing
  xlab("forage duration (min)") +
  ylab("")

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
  annotate("text", x =  as.POSIXct("42", format="%M"), y = 0.0013, # hardwire coord locations for mapping using POSIX
           label = "35.0", alpha = 0.75, size = 2.7)+
  scale_fill_manual(values=c("#3288bd")) +
  scale_x_datetime(breaks = breaks_width("10 min"), date_labels = "%M") + # set the time scales and drop date info
  xlab("dawn to dance (min)") +
  ylab("density")

# deprecated now since we have the lm(dawn ~ dance) panel
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
p1 + p4 + p3 + p5 + 
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'a') # add panel labels a, b, c... etc


#### read in monitoring effort data
DF1 <- read.csv('data/monitor_hours.csv')
# sum the total person monitoring hours across the project, for each calendar day
DF1 <- DF1 %>% 
  mutate(day_tothrs = rowSums(.[3:27])/2) %>% # divide by 2 as the breaks are 30min and we want hours
  mutate(cumul_hours = cumsum(day_tothrs)) # create a cumulative summary running through monitor days 


p6 <- ggplot(DF1, aes(x=DAY, y=cumul_hours)) +
  themeKV + theme(legend.position = "none", 
                  axis.text.x = element_text(size = 8),
                  axis.text.y = element_text(size = 8),
                  axis.title.x = element_text(size = 9),
                  axis.title.y = element_text(size = 9)) + 
  geom_area(fill="#41ab5d", alpha=0.6) +
  geom_line(linewidth = 0.5) +
  scale_x_continuous(breaks = seq(170, 280, by = 15)) +
  scale_y_continuous(breaks = seq(0, 800, by = 100)) +
  xlab("Julian day") +
  ylab("cumul. effort (hrs)")
p6

DF2 <- DF1 %>% 
  summarise("5:00" = sum(X5.00 > 0), # this counts no. days with observer coverage from 5-5:30am
            "5:30" = sum(X5.30 > 0),
            "6:00" = sum(X6.00 > 0),
            "6:30" = sum(X6.30 > 0),
            "7:00" = sum(X7.00 > 0),
            "7:30" = sum(X7.30 > 0),
            "8:00" = sum(X8.00 > 0),
            "8:30" = sum(X8.30 > 0),
            "9:00" = sum(X9.00 > 0),
            "9:30" = sum(X9.30 > 0),
            "10:00" = sum(X10.00 > 0),
            "10:30" = sum(X10.30 > 0),
            "11:00" = sum(X11.00 > 0),
            "11:30" = sum(X11.30 > 0),
            "12:00" = sum(X12.00 > 0),
            "12:30" = sum(X12.30 > 0),
            "13:00" = sum(X13.00 > 0),
            "13:30" = sum(X13.30 > 0),
            "14:00" = sum(X14.00 > 0),
            "14:30" = sum(X14.30 > 0),
            "15:00" = sum(X15.00 > 0),
            "15:30" = sum(X15.30 > 0),
            "16:00" = sum(X16.00 > 0),
            "16:30" = sum(X16.30 > 0),
            "17:00" = sum(X17.00 > 0))  

DF3 <- DF1 %>% 
  summarise("5:00" = sum(X5.00), # counts no. observer hours during 5-5:30am over all days
            "5:30" = sum(X5.30),
            "6:00" = sum(X6.00),
            "6:30" = sum(X6.30),
            "7:00" = sum(X7.00),
            "7:30" = sum(X7.30),
            "8:00" = sum(X8.00),
            "8:30" = sum(X8.30),
            "9:00" = sum(X9.00),
            "9:30" = sum(X9.30),
            "10:00" = sum(X10.00),
            "10:30" = sum(X10.30),
            "11:00" = sum(X11.00),
            "11:30" = sum(X11.30),
            "12:00" = sum(X12.00),
            "12:30" = sum(X12.30),
            "13:00" = sum(X13.00),
            "13:30" = sum(X13.30),
            "14:00" = sum(X14.00),
            "14:30" = sum(X14.30),
            "15:00" = sum(X15.00),
            "15:30" = sum(X15.30),
            "16:00" = sum(X16.00),
            "16:30" = sum(X16.30),
            "17:00" = sum(X17.00)) 

hour_effort <- gather(DF3, key="time", value="hours", 1:25) # convert from wide to long format
class(hour_effort$time) # returns "character"
format(as.POSIXct(hour_effort$time,format='%H:%M'),format="%H:%M") # for some reasons needs reformatting
hour_effort$time <- as.POSIXct(hour_effort$time, format="%H:%M") # still character, so convert to time/date
class(hour_effort$time) # check format, should read --> "POSIXct" "POSIXt"

p7 <- ggplot(hour_effort, aes(x=time, y=hours)) +
  themeKV + theme(#legend.position = "none", 
                  axis.text.x = element_text(size = 8),
                  axis.text.y = element_text(size = 8),
                  axis.title.x = element_text(size = 9),
                  axis.title.y = element_text(size = 9)) + 
  geom_col(fill="#006d2c", alpha = 0.75, width = 1800) + # ¡¡exaggerated!! width bc date/time format
  # geom_bar(fill="#006d2c", alpha = 0.75, width = 0.9) + # cannot use width as fiddly in date/time format
  # see: https://github.com/tidyverse/ggplot2/issues/2187
  scale_x_datetime(breaks = breaks_width("120 min"), date_labels = "%H") + # set the time scales and drop date info
  scale_y_continuous(breaks = seq(0, 120, by = 20)) +
  xlab("hour of day") +
  ylab("effort (person days)")
p7

layout2 <- "
A
B
C"
  p8 + p7 + p6 +
  plot_layout(design = layout2) +
  plot_annotation(tag_levels = 'a') # add panel labels a, b, c... etc



layout3 <- "
AAAAABBEE
AAAAABBEE
AAAAABBEE
AAAAABBEE
AAAAACCFF
AAAAACCFF
AAAAACCFF
AAAAACCFF
AAAAADDGG
AAAAADDGG
AAAAADDGG
AAAAADDGG"
p1 + p4 + p3 + p5 + p8 + p7 + p6 +
  plot_layout(design = layout3) +
  plot_annotation(tag_levels = 'a') # add panel labels a, b, c... etc