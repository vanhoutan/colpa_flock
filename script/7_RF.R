#### this script develops the RF model and resulting pairwise xy and PDP dataviz 

library(dplyr)        # variable grouping and manipulation
library(tidyverse)    # gathering and spreading
library(caret)        # RF
library(randomForest) # duh
library(pdp)          # PDP plots
library(ggplot2)      # plotting and viz
library(ggthemes)     # helpful ggplot themes
library(RColorBrewer) # pretty colors
library(colorspace)
library(patchwork)    # assembling composite plots


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
                  axis.text.x = element_text(size = 7, colour = "black", margin = unit(c(0.15,0,0,0), "cm"))) + 
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
  xlab("brain size") + ylab("sociality index") +
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

# bill lengths
p4 <- loroRF %>% ggplot(aes(x=BEAK_cmsl, y=INDEX)) +
  themeKV + theme(axis.text.y = element_text(size = 7), axis.text.x = element_text(size = 7),
                  axis.title.x = element_text(size = 8),axis.title.y = element_text(size = 8),
                  legend.position = "none") + 
  geom_boxplot(aes(group = SPECIES), fatten=NULL, outlier.shape = NA, coef = 1, lwd=0.25, 
               width=0.4,varwidth = TRUE, position=position_dodge())+
  geom_line(stat = "smooth", method = "loess", formula = y ~ x,
            color = "#f46d43", alpha = 0.6, span = 0.8, se = FALSE, linewidth = 2.5, lineend = "round")+
  xlab("bill lengths (cm)") + ylab("sociality index") +
  scale_y_continuous(breaks = seq(0, 1000, by = 150),limits = c(0,600))+
  scale_x_continuous(breaks = seq(0, 14, by = 2))

#### patch together
layout <- "
A
B
C
D
E"
p1 + p2 + p4 + p3 + p5 +
plot_layout(design = layout) +
plot_annotation(tag_levels = 'a') # add panel labels



#### develop, train, and test RF model
head(loroRF)

#### don't need to manually partition the data as it's happening in CARET
set.seed(916)
test_index <- createDataPartition(loroRF$INDEX, times = 1, p = 0.2, list = FALSE)
test_loro <- loroRF[test_index, ]
train_loro <- loroRF[-test_index, ]
#check the product
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
tc2 <- trainControl(method = "repeatedcv", number = 5, repeats = 5)


# develop full RF with all 9 covariates
set.seed(916)
train_rf10 <- train(INDEX ~ BEAK_cmsl + WING_load + WING_hwi + BRAIN_Yres + TRIBE + WING_twai + MASS + BRAIN_ml + GENUS + SPECIES,
                  method = "rf", trControl = tc,
                  ntree = 2000,
                  tuneGrid = data.frame(mtry = seq(1:10)),
                  data=loroRF)
# examine results
plot(train_rf10) 
train_rf10$results
train_rf10$bestTune # 10 covars, ntree =2000, CV resampling, mtry=9, Rsq = 0.96

# run the trained RF with all 10 predictors
set.seed(0819)
rf10 <- randomForest(INDEX ~ BEAK_cmsl + WING_load + WING_hwi + BRAIN_Yres + TRIBE + WING_twai + MASS + BRAIN_ml + GENUS + SPECIES,
                   data=loroRF, importance = T, 
                   trControl = tc,
                   ntree = 2000, mtry = 9)
# extract variable importance rankings
rf10_imp <- importance(rf10, type=1)
rf10_imp <- as.data.frame(rf10_imp) # make into df
rf10_imp <- tibble::rownames_to_column(rf10_imp, "value") # convert row name to col
names(rf10_imp)[names(rf10_imp) == "%IncMSE"] <- "IncMSE" # rename col header
rf10_imp <- rf10_imp[order(rf10_imp$IncMSE, decreasing = T),]
rf10_imp <- rf10_imp %>% mutate(relIncMSE = 100*(IncMSE/max(IncMSE))) # add rel IncMSE
rf10_imp


# drop highly correlated predictors since they're redunaant
# from a mechanism standpoints they're also indirect
set.seed(916)
train_rf5 <- train(INDEX ~ BEAK_cmsl + WING_load + WING_hwi + BRAIN_Yres + TRIBE, # these 5 are independent
                   method = "rf", trControl = tc,
                   ntree = 2000,
                   tuneGrid = data.frame(mtry = seq(1:5)),
                   data=loroRF)
# training model results
plot(train_rf5) 
train_rf5$results
train_rf5$bestTune # 5 covars, ntree =1000, CV resampling, mtry=5, Rsq = 0.96

# run tuned RF model 
set.seed(0819)
rf5 <- randomForest(INDEX ~ BEAK_cmsl + WING_load + WING_hwi + BRAIN_Yres + TRIBE,
                   data=loroRF, importance = T, 
                   trControl = tc,
                   ntree = 2000, mtry = 5)
# call in results
rf5 # MS resids = 917, Rsquare = 0.9592

# calculate variable importance 
rf_imp <- importance(rf5, type=1)
rf_imp <- as.data.frame(rf_imp) # make into df
rf_imp <- tibble::rownames_to_column(rf_imp, "value") # convert row name to col
names(rf_imp)[names(rf_imp) == "%IncMSE"] <- "IncMSE" # rename col header
rf_imp <- rf_imp[order(rf_imp$IncMSE, decreasing = T),] # sort df by rank order to retain colors fr pair-wise plots
rf_imp <- rf_imp %>% mutate(relIncMSE = 100*(IncMSE/max(IncMSE))) # add rel IncMSE
rf_imp # now ready for ggplot


#### develop the data for the full 2-way PDPs
# first define lists of factor pairings to plot
flistx <- c("BRAIN_Yres", "BRAIN_Yres", "BRAIN_Yres", "BRAIN_Yres")
flisty <- c("WING_hwi", "WING_load", "BEAK_cmsl", "TRIBE")
j <- length(flistx)
partial_factxy_all <- NULL

for (x in 1:j) {
  partial_factxy <- pdp::partial(rf, pred.var = c(flistx[x], flisty[x]), plot = F, rug = T, chull = T)
  partial_factxy$mfactorsxy <- paste0(flistx[x],":", flisty[x])
  partial_factxy <- partial_factxy %>% dplyr::rename("factorx" = names(partial_factxy[1]), "factory" = names(partial_factxy[2]))
  partial_factxy_all <- rbind(partial_factxy, partial_factxy_all)
}
# note that if plyr loaded, could potentially pull in plyr 'rename()' command causing errors
# don't need plyr here so don't load! 
# when it was loaded, I was getting the following error message:
# Error in rename(., factorx = names(partial_factxy[1]), factory = names(partial_factxy[2])) : 
# unused arguments (factorx = names(partial_factxy[1]), factory = names(partial_factxy[2]))


partial_factxy_all$mfactorsxy <- as.factor(partial_factxy_all$mfactorsxy)
partial_factxy_all <- as.data.frame(partial_factxy_all)

# define color ramp + fix scale limits
cols = c("#9e0142", "#d53e4f",  "#fdae61", "#fee08b", "#e6f598", "#abdda4", "#66c2a5", "#3288bd", "#313695")
lims = c(min(partial_factxy_all$yhat), max(partial_factxy_all$yhat)) 

# reorder levels
partial_factxy_all$mfactorsxy<- factor(partial_factxy_all$mfactorsxy, 
                                       levels = c("BRAIN_Yres:WING_hwi","BRAIN_Yres:WING_load","BRAIN_Yres:BEAK_cmsl","BRAIN_Yres:TRIBE"))

# plot categorical PDP first
p10<- ggplot(data = partial_factxy_all, aes(x = factorx, y = factory, fill = yhat))+
  themeKV+ theme(legend.key.height=unit(0.4,"cm"), legend.key.width=unit(0.32,"cm"), 
                 axis.text.x = element_text(size = 7, colour = "black", margin = unit(c(0.15,0,0,0), "cm"))) + 
  geom_tile(data = dplyr::filter(partial_factxy_all, mfactorsxy == "BRAIN_Yres:TRIBE"))+
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
  xlab("brain size") + ylab("hand wing index")

p8 <- ggplot(data = partial_factxy_all, aes(x = factorx, y = factory, fill = yhat))+
  themeKV+ theme(legend.position = "none") +
  geom_tile(data = filter(partial_factxy_all, mfactorsxy == "BRAIN_Yres:WING_load"))+
  scale_fill_gradientn(colours = cols, limits=lims)+
  #scale_x_continuous(breaks = seq(-2, 2),limits = c(-2, 2.3)) +
  xlab("brain size") + ylab("wing load (N m s-2)")

p9 <- ggplot(data = partial_factxy_all, aes(x = factorx, y = factory, fill = yhat))+
  themeKV+ theme(legend.position = "none") +
  geom_tile(data = filter(partial_factxy_all, mfactorsxy == "BRAIN_Yres:BEAK_cmsl"))+
  scale_fill_gradientn(colours = cols, limits=lims)+
  #scale_x_continuous(breaks = seq(-2, 2),limits = c(-2, 2.3)) +
  scale_y_continuous(breaks = seq(0, 14, by = 2)) +
  xlab("brain size") + ylab("bill lengths (cm)")


# plot variable importance ranks
p6 <- rf_imp %>% ggplot(aes(x = relIncMSE, y = fct_rev(fct_infreq(value, IncMSE)))) +
  themeKV + theme(axis.text.y = element_text(size = 6)) +
  geom_col(alpha =0.8, width=0.85,
           fill = c("#5e4fa2", "#3288bd", "#66c2a5", "#f46d43", "#9e0142")) +
  scale_x_continuous(breaks = seq(0, 100, by = 20)) +
  scale_y_discrete(expand = expand_scale(add = c(0.8, 0.8)))+
  ylab(NULL) + xlab("relavtive ΔMSE (%)")

#### patch them all together
layout <- "
AF
BG
CH
DI
EJ"
p1 + p2 + p3 + p4 + p5 + p6 + p7 + p9 + p8 + p10 +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'a')

stopCluster(c1) # end parallel processing


#### run sensitivity RF posthoc, 

head(loroRF)
# split loroDF by tribe
androDF <- loroRF %>% filter(TRIBE == "Androglossini")
ariniDF <- loroRF %>% filter(TRIBE == "Arini")
#check splits
nrow(androDF) # 10000 
nrow(ariniDF) # 14000

# run RF for each and check variable ranks
# run tuned RF model 

# first for Androglossini
set.seed(0819)
androRF <- randomForest(INDEX ~ BEAK_cmsl + WING_load + WING_hwi + BRAIN_Yres,
                   data=androDF, importance = T, 
                   trControl = tc,
                   ntree = 2000, mtry = 4)
androRF # MS resids = 1040, Rsquare = 0.9673

androRF_imp <- importance(androRF, type=1) # calculate variable importance 
androRF_imp <- as.data.frame(androRF_imp) # make into df
androRF_imp <- tibble::rownames_to_column(androRF_imp, "value") # convert row name to col
names(androRF_imp)[names(androRF_imp) == "%IncMSE"] <- "IncMSE" # rename col header
androRF_imp <- androRF_imp[order(androRF_imp$IncMSE, decreasing = T),] # sort df by rank order to retain colors fr pair-wise plots
androRF_imp <- androRF_imp %>% mutate(relIncMSE = 100*(IncMSE/max(IncMSE))) # add relative IncMSE col
androRF_imp # brain size is 94% rel IncMSE


# then for Arini
set.seed(0819)
ariniRF <- randomForest(INDEX ~ BEAK_cmsl + WING_load + WING_hwi + BRAIN_Yres,
                        data=ariniDF, importance = T, 
                        trControl = tc,
                        ntree = 2000, mtry = 4)
ariniRF # MS resids = 812, Rsquare = 0.9448

ariniRF_imp <- importance(ariniRF, type=1) # calculate variable importance 
ariniRF_imp <- as.data.frame(ariniRF_imp) # make into df
ariniRF_imp <- tibble::rownames_to_column(ariniRF_imp, "value") # convert row name to col
names(ariniRF_imp)[names(ariniRF_imp) == "%IncMSE"] <- "IncMSE" # rename col header
ariniRF_imp <- ariniRF_imp[order(ariniRF_imp$IncMSE, decreasing = T),] # sort df by rank order to retain colors fr pair-wise plots
ariniRF_imp <- ariniRF_imp %>% mutate(relIncMSE = 100*(IncMSE/max(IncMSE))) # add rel
ariniRF_imp # brain size is runaway dominant predictor, 100% rel IncMSE


#### EL FÍN