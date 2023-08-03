#### this script is plotting the high-level monitoring stats of collpa activity 
#### should be one of the first figures in the manuscript
#### it communicates across the monitoring period the periods of diurnal activity and monitoring effort


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
# check on data format of "time" col
class(collpa1$time)
# returns [1] "character", need convert to time/date

collpa1 <- strptime(collpa1$time, format = "%H:%M:%S")
class(collpa1$time)


collpa1.time <- as.POSIXct(collpa1$time, format="%H:%M")
class(collpa1.time)

collpa1 %>% 
  tibble() %>% 
  mutate(Watt_pro_m2 = as.numeric(V2),
         time = as.POSIXct(strptime(V1, "%H:%M"))) %>%
  


# now we can make some plots :)


ggplot(collpa1, aes(x = julian_day, y = time, group = event, color = event, shape = event)) +
  themeKV + 
  theme(axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 9)) + 
  geom_line(aes(color=event), stat="smooth", method = "loess", formula = y ~ x, span = 0.75, 
            se = FALSE, linewidth = 1.5, alpha = 0.5)  +
  # scale_color_manual(values=c("#abdda4", "#3288bd")) + 
  geom_point(size = 2, alpha = 0.5, stroke = 0.5) +
  scale_shape_manual(values = c(16, 1, 16, 1, 16)) +
  # scale_fill_manual(values=c("#abdda4", "#3288bd")) + 
  scale_x_continuous(breaks = seq(170, 280, by = 10)) +
#  scale_y_datetime(breaks = breaks_width("1 hour"),labels=date_format("%H:%M")) +
  ylab("hour of day") +
  xlab("Julian day")



