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
  theme(plot.margin = unit(c(0,0,0,0), "cm"),
        strip.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 7, colour = "black", margin = unit(c(0.15,0,0,0), "cm")),
        axis.text.y = element_text(size = 7, colour = "black", margin = unit(c(0,0.15,0,0), "cm")),
        axis.title.x = element_text(size = 8),axis.title.y = element_text(size = 8),
        axis.ticks.x = element_line(colour = "black", linewidth=.25), axis.ticks.y = element_line(colour = "black", linewidth=.25),
        axis.ticks.length=unit(-0.15, "cm"),element_line(colour = "black", linewidth=.25),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=.25),
        legend.title=element_text(size = 8), legend.text=element_text(size = 6), 
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
  scale_y_continuous(breaks = seq(0, 1000, by = 150))

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
  scale_y_continuous(breaks = seq(0, 1000, by = 150),limits = c(0,600))

# hand wing index
p2 <- loroRF %>% ggplot(aes(x=WING_hwi, y=INDEX)) +
  themeKV + theme(legend.position = "none") + 
  geom_boxplot(aes(group = SPECIES), fatten=NULL, outlier.shape = NA, 
               coef = 1, lwd=0.25, alpha = 1, width=0.8, varwidth = TRUE, position=position_dodge())+
  geom_line(stat = "smooth", method = "loess", formula = y ~ x,
            color = "#3288bd", alpha = 0.6, span = 0.8, se = FALSE, linewidth = 2.5, lineend = "round")+
  xlab("hand wing index") + ylab("sociality index") +
  scale_y_continuous(breaks = seq(0, 1000, by = 150),limits = c(0,600))+
  scale_x_continuous(breaks = seq(0, 50, by = 4))

# wing load
p3 <- loroRF %>% ggplot(aes(x=WING_load, y=INDEX)) +
  themeKV + theme(legend.position = "none") + 
  geom_boxplot(aes(group = SPECIES), fatten=NULL, outlier.shape = NA, 
               coef = 1, lwd=0.25, alpha = 1, width=1, varwidth = TRUE, position=position_dodge())+
  geom_line(stat = "smooth", method = "loess", formula = y ~ x,
            color = "#66c2a5", alpha = 0.6, span = 0.8, se = FALSE, linewidth = 2.5, lineend = "round")+
  xlab("wing load (N m s-2)") + ylab("sociality index") +
  scale_y_continuous(breaks = seq(0, 1000, by = 150),limits = c(0,600))

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
  scale_y_continuous(breaks = seq(0, 1000, by = 150),limits = c(0,600))+
  scale_x_continuous(breaks = seq(0, 14, by = 2))

#### patch together
layout <- "
A
B
C
D
E"
p1 + p2 + p3 + p4 + p5 +
plot_layout(design = layout) +
plot_annotation(tag_levels = 'a') # add panel labels



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

# drop highly correlated covars, since they're poop
set.seed(916)
train_rf5 <- train(INDEX ~ BEAK_cmsl + WING_load + WING_hwi + BRAIN_Yres + TRIBE, # these 5 are independent
                   method = "rf", trControl = tc,
                   ntree = 1000,
                   tuneGrid = data.frame(mtry = seq(1:5)),
                   data=loroRF)
# training model results
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
rf_imp <- as.data.frame(rf_imp) # make into df
rf_imp <- tibble::rownames_to_column(rf_imp, "value") # convert row name to col
names(rf_imp)[names(rf_imp) == "%IncMSE"] <- "IncMSE" # rename col header
head(rf_imp)


#### develop the data for the full 2-way PDPs
# first define lists of factor pairings to plot
flistx <- c("BRAIN_Yres", "BRAIN_Yres", "BRAIN_Yres", "BRAIN_Yres")
flisty <- c("WING_hwi", "WING_load", "BEAK_cmsl", "TRIBE")
j <- length(flistx)
partial_factxy_all <- NULL

for (x in 1:j) {
  partial_factxy <- pdp::partial(rf, pred.var = c(flistx[x], flisty[x]), plot = F, rug = T, chull = T)
  partial_factxy$mfactorsxy <- paste0(flistx[x],":", flisty[x])
  partial_factxy <- partial_factxy %>% rename("factorx" = names(partial_factxy[1]), "factory" = names(partial_factxy[2]))
  partial_factxy_all <- rbind(partial_factxy, partial_factxy_all)
}

partial_factxy_all$mfactorsxy <- as.factor(partial_factxy_all$mfactorsxy)

# define color ramp + fix scale limits
cols = c("#9e0142", "#d53e4f",  "#fdae61", "#fee08b", "#e6f598", "#abdda4", "#66c2a5", "#3288bd", "#313695")
lims = c(min(partial_factxy_all$yhat), max(partial_factxy_all$yhat)) 

# reorder levels
partial_factxy_all$mfactorsxy<- factor(partial_factxy_all$mfactorsxy, 
                                       levels = c("BRAIN_Yres:WING_hwi","BRAIN_Yres:WING_load","BRAIN_Yres:BEAK_cmsl","BRAIN_Yres:TRIBE"))

# plot categorical PDP first
p10<- ggplot(data = partial_factxy_all, aes(x = factorx, y = factory, fill = yhat))+
  themeKV+ theme(legend.key.height=unit(0.4,"cm"), legend.key.width=unit(0.32,"cm"), 
                 axis.text.x = element_text(size = 6, colour = "black", margin = unit(c(0.15,0,0,0), "cm"))) + 
  geom_tile(data = filter(partial_factxy_all, mfactorsxy == "BRAIN_Yres:TRIBE"))+
  scale_fill_gradientn(colours = cols, limits=lims)+
  scale_x_continuous(breaks = seq(-2, 2.3)) +
  xlab("brain vol. resids")+ ylab(NULL) + coord_flip()

# force numeric format on factory, ! this converts TRIBE values to NAs but we no longer need
partial_factxy_all$factory <- as.numeric(partial_factxy_all$factory)

# plot continuous PDPs
p7 <- ggplot(data = partial_factxy_all, aes(x = factorx, y = factory, fill = yhat))+
  themeKV+ theme(legend.position = "none") +
  geom_tile(data = filter(partial_factxy_all, mfactorsxy == "BRAIN_Yres:WING_hwi"))+
  scale_fill_gradientn(colours = cols, limits=lims)+
  #scale_x_continuous(breaks = seq(-2, 2),limits = c(-2, 2.3)) +
  xlab("brain volume residuals") + ylab("hand wing index")

p8 <- ggplot(data = partial_factxy_all, aes(x = factorx, y = factory, fill = yhat))+
  themeKV+ theme(legend.position = "none") +
  geom_tile(data = filter(partial_factxy_all, mfactorsxy == "BRAIN_Yres:WING_load"))+
  scale_fill_gradientn(colours = cols, limits=lims)+
  #scale_x_continuous(breaks = seq(-2, 2),limits = c(-2, 2.3)) +
  xlab("brain volume residuals") + ylab("wing load (N m s-2)")

p9 <- ggplot(data = partial_factxy_all, aes(x = factorx, y = factory, fill = yhat))+
  themeKV+ theme(legend.position = "none") +
  geom_tile(data = filter(partial_factxy_all, mfactorsxy == "BRAIN_Yres:BEAK_cmsl"))+
  scale_fill_gradientn(colours = cols, limits=lims)+
  scale_x_continuous(breaks = seq(-2, 2),limits = c(-2, 2.3)) +
  scale_y_continuous(breaks = seq(0, 14, by = 2)) +
  xlab("brain volume residuals") + ylab("culmen + mandible (cm)")


# plot variable importance ranks
# sort df by rank order to retain colors fr pair-wise plots
rf_imp <- rf_imp[order(rf_imp$value, decreasing = FALSE),]

p6 <- rf_imp %>% ggplot(aes(x = IncMSE, y = fct_rev(fct_infreq(value, IncMSE)))) +
  themeKV + theme(axis.text.y = element_text(size = 6)) +
  geom_col(alpha =0.9, width=0.85,
           fill = c("#5e4fa2", "#3288bd", "#66c2a5", "#f46d43", "#9e0142")) +
  scale_x_continuous(breaks = seq(0, 1000, by = 150)) +
  scale_y_discrete(expand = expand_scale(add = c(0.8, 0.8)))+
  ylab(NULL) + xlab("Δ MSE if removed (%)")

#### patch them all together
layout <- "
AF
BG
CH
DI
EJ"
p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'a')




#### FIN