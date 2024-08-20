#### this script develops the RF model and resulting ICE and PDP dataviz 

library(ggplot2)      # plotting and viz
library(plyr)         # legacy df manipulation
library(dplyr)        # variable grouping and manipulation
library(tidyr)        # gathering and spreading
library(tidyverse)
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

#### first make raw xy pair-wise comparison plots for covars
# first taxon, boxplot
p5 <- loroRF %>% ggplot(aes(x=fct_reorder(TRIBE,-INDEX), y=INDEX)) +
  themeKV + theme(legend.position = "none",
                  axis.text.x = element_text(size = 6, colour = "black", margin = unit(c(0.15,0,0,0), "cm"))) + 
  geom_boxplot(outlier.shape = NA, # remove outliers
               fatten=1, # NULL = remove median line
               color = "#9e0142", coef = 1, # whiskers sd =1 
               lwd=0.5, alpha = 0.7, # lwd = linewidth
               width=0.65) + 
  xlab("taxonomic tribe") +
  ylab("sociality index") +
  scale_y_continuous(breaks = seq(0, 1000, by = 100))

# next brain size resids scatter w/loess
p1 <- loroRF %>% ggplot(aes(x=BRAIN_Yres, y=INDEX)) +
  themeKV + theme(legend.position = "none") + 
  # geom_point(alpha=0.003, size=1) +
  geom_boxplot(aes(group = SPECIES),
               fatten=NULL, # NULL = remove median line
               outlier.shape = NA, coef = 1, # remove outliers and whiskers
               lwd=0.25, alpha = 1, 
               width=0.2,varwidth = TRUE, position=position_dodge())+ # control box width w/overlap
  geom_line(stat = "smooth", method = "loess", formula = y ~ x,
            color = "#5e4fa2", alpha = 0.6,
            span = 0.8, se = FALSE, linewidth = 2.5, lineend = "round")+
  xlab("brain volume residuals") + ylab("sociality index") +
  scale_y_continuous(breaks = seq(0, 1000, by = 100),limits = c(0,600))

# hand wing index
p2 <- loroRF %>% ggplot(aes(x=WING_hwi, y=INDEX)) +
  themeKV + theme(legend.position = "none") + 
  geom_boxplot(aes(group = SPECIES), fatten=NULL, outlier.shape = NA, 
               coef = 1, lwd=0.25, alpha = 1, width=0.8, varwidth = TRUE, position=position_dodge())+
  geom_line(stat = "smooth", method = "loess", formula = y ~ x,
            color = "#3288bd", alpha = 0.6, span = 0.8, se = FALSE, linewidth = 2.5, lineend = "round")+
  xlab("hand wing index") + ylab("sociality index") +
  scale_y_continuous(breaks = seq(0, 1000, by = 100),limits = c(0,600))+
  scale_x_continuous(breaks = seq(0, 50, by = 4))

# wing load
p3 <- loroRF %>% ggplot(aes(x=WING_load, y=INDEX)) +
  themeKV + theme(legend.position = "none") + 
  geom_boxplot(aes(group = SPECIES), fatten=NULL, outlier.shape = NA, 
               coef = 1, lwd=0.25, alpha = 1, width=1, varwidth = TRUE, position=position_dodge())+
  geom_line(stat = "smooth", method = "loess", formula = y ~ x,
            color = "#66c2a5", alpha = 0.6, span = 0.8, se = FALSE, linewidth = 2.5, lineend = "round")+
  xlab("wing loading (N m s-2)") + ylab("sociality index") +
  scale_y_continuous(breaks = seq(0, 1000, by = 100),limits = c(0,600))

# beak size
p4 <- loroRF %>% ggplot(aes(x=BEAK_cmsl, y=INDEX)) +
  themeKV + theme(axis.text.y = element_text(size = 7), axis.text.x = element_text(size = 7),
                  axis.title.x = element_text(size = 8),axis.title.y = element_text(size = 8),
                  legend.position = "none") + 
  geom_boxplot(aes(group = SPECIES), fatten=NULL, outlier.shape = NA, coef = 1, lwd=0.25, 
               width=0.4,varwidth = TRUE, position=position_dodge())+
  geom_line(stat = "smooth", method = "loess", formula = y ~ x,
            color = "#f46d43", alpha = 0.6, span = 0.8, se = FALSE, linewidth = 2.5, lineend = "round")+
  xlab("culmen + mandible (cm)") + ylab("sociality index") +
  scale_y_continuous(breaks = seq(0, 1000, by = 100),limits = c(0,600))+
  scale_x_continuous(breaks = seq(0, 14, by = 2))

#### patch them all together
layout <- "
A
B
C
D
E"
p1 + p2 + p3 + p4 + p5 +
plot_layout(design = layout) +
plot_annotation(tag_levels = 'a') # add panel labels a, b, c... etc



#### develop, train, and test RF model
head(loroRF)
set.seed(916)
test_index <- createDataPartition(loroRF$INDEX, times = 1, p = 0.2, list = FALSE)
test_loro <- loroRF[test_index, ]
train_loro <- loroRF[-test_index, ]

nrow(train_loro) # 9060 obs in training set
nrow(test_loro) # 2400 obs in test set
# What proportion of species in training set have INDEX value > 250?
# should be ~ 50%
mean(train_loro$INDEX > 250) # 0.5316


# turn on parallel processing
library(parallel)
library(doParallel) # begin the parallel processing
# see: https://cran.r-project.org/web/packages/doParallel/doParallel.pdf
# https://topepo.github.io/caret/parallel-processing.html
cores = detectCores()-1
c1 <- makePSOCKcluster(cores)
registerDoParallel(c1) # just to save some processing time


# for resampling, use cross validation instead of bootstrap
# 10-fold cv, repeated 5x
tc <- trainControl(method = "repeatedcv", number = 10, repeats = 5) # http://zevross.com/blog/2017/09/19/predictive-modeling-and-machine-learning-in-r-with-the-caret-package/


# develop full RF with all 9 covariates
set.seed(916)
train_rf9 <- train(INDEX ~ BEAK_cmsl + WING_load + WING_hwi + BRAIN_Yres + TRIBE + WING_twai + MASS + BRAIN_ml + GENUS,
                  method = "rf", trControl = tc,
                  ntree = 1000,
                  tuneGrid = data.frame(mtry = seq(1:9)),
                  data=loroRF)
# examine results
plot(train_rf9) 
train_rf9$results
train_rf9$bestTune # 9 covars, ntree =1000, CV resampling, mtry=9, Rsq = 0.96


# drop highly correlated covars
set.seed(916)
train_rf5 <- train(INDEX ~ BEAK_cmsl + WING_load + WING_hwi + BRAIN_Yres + TRIBE, # these 5 are independent
                   method = "rf", trControl = tc,
                   ntree = 1000,
                   tuneGrid = data.frame(mtry = seq(1:5)),
                   data=loroRF)
# results
plot(train_rf5) 
train_rf5$results
train_rf5$bestTune # 5 covars, ntree =1000, CV resampling, mtry=5, Rsq = 0.96

stopCluster(c1) # end parallel processing

# run tuned RF model 
set.seed(0819)
rf <- randomForest(INDEX ~ BEAK_cmsl + WING_load + WING_hwi + BRAIN_Yres + TRIBE,
                   data=loroRF, 
                   importance = TRUE, 
                   ntree = 1000, mtry = 5,trControl = tc)
# call in results
rf # MS resids = 810.0965, Rsquare = 0.9638

# calculate variable importance 
rf_imp <- importance(rf, type=1)
# make into df
rf_imp <- as.data.frame(rf_imp)
rf_imp <- tibble::rownames_to_column(rf_imp, "value")
head(rf_imp)







#### FIN