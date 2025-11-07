## --------------------------------------------------------------#
## Script name: Script0-4_Results
##
## Purpose of script: 
##    
## Various descriptive stats and plots regarding
## Hamilton Harbour Goldfish. Main results script for
## HH GF pop dems/ages/bar spacing MS.
## 
##
## Author: 
## Cole MacLeod
## 
##
## Original Date Created: 20 Oct 2025
## Version: 2025-11-07
## ---------------------------------------------------------------#

###Load packages
library(tidyverse)
library(ggplot2)
library(patchwork)

### Load data
#----------------------------#
data<-read.csv("01 - Data/2025-10-29_DFO_SN.csv")
data1<-read.csv("01 - Data/2025-11-05_Batch_DFO_SN.csv")


### Specify objects and parameters
#----------------------------#
theme_set(theme_classic())
options(scip=99)
param_widths <- data.frame(width_mm=c(50, 40, 30)) #Target widths for model predictions
df_testresults <- data.frame() #Home for test results
plots <- list() #home for plots


#Description of morphological variables ---------------
#uses batch data - want to be size structure representative

#TL
#----------------------------#
range(data1$TL_mm)
mean(data1$TL_mm)
sd(data1$TL_mm)

#visualize TL distrib
plots$TLhist<-ggplot(data1, aes(TL_mm))+
 geom_histogram(binwidth=50, fill="lightgrey", colour="black")

#Width
#----------------------------#
range(data1$width_mm, na.rm=TRUE)
mean(data1$width_mm, na.rm=TRUE)
sd(data1$width_mm, na.rm=TRUE)
#visualize width distrib
plots$widthhist<-ggplot(data1, aes(width_mm))+
 geom_histogram(binwidth=10, fill="lightgrey", colour="black", na.rm=TRUE)

#mass
#----------------------------#
range(data1$weight_g, na.rm=TRUE)
mean(data1$weight_g, na.rm=TRUE)
sd(data1$weight_g, na.rm=TRUE)
#visualize mass distrib
plots$masshist<-ggplot(data1, aes(weight_g))+
 geom_histogram(binwidth=100, fill="lightgrey", colour="black", na.rm=TRUE)

#combine morph plots
#----------------------------#
plots$morphhists <-
 with(plots,
      TLhist/widthhist/masshist
 )
plots$morphhists

#M vs F size diff?
#----------------------------#
data %>% count(sex)
#only have 28 males.. typically need min. 30 to assume representative?

t.test(TL_mm~sex, data)
t.test(width_mm~sex, data)
t.test(weight_g~sex, data)

data %>% filter(!is.na(sex)) %>% group_by(sex) %>%
 summarize(meanTL=mean(TL_mm), sdTL=sd(TL_mm))

data %>% filter(!is.na(sex), !is.na(width_mm)) %>% group_by(sex) %>%
 summarize(meanWi=mean(width_mm), sdWi=sd(width_mm))

data %>% filter(!is.na(sex), !is.na(weight_g)) %>% group_by(sex) %>%
 summarize(meanW=mean(weight_g), sdW=sd(weight_g))
#females larger across all three, significantly so for width and weight


# Relating Morphological variables  -----------
#non-batch data - only want observed values, not predicted

#TL-FL
#----------------------------#
model_TL_FL <- lm(FL_mm~TL_mm, data)
summary(model_TL_FL)
coef(model_TL_FL)

plots$TL_FL<-ggplot(data, aes(y = FL_mm, x = TL_mm))+
 geom_point(size=3, alpha=0.5)+
 geom_smooth(method="lm", se=F)+
 ylim(0, 475)

#TL - SL
#----------------------------#
model_TL_SL <- lm(SL_mm~TL_mm, data)
summary(model_TL_SL)

plots$TL_SL<-ggplot(data, aes(y = SL_mm, x = TL_mm))+
 geom_point(size=3, alpha=0.5)+
 geom_smooth(method="lm", se=F)+
 ylim(0, 475)

plots$TL_SL

#TL - Height
#----------------------------#

lm_
log_TL_height<-lm(log(height_mm)~log(TL_mm), #unreasonable height measurements
                    data %>% filter(!ID=="52", !ID=="75"))
summary(model_TL_height)

data$logpredictions<-predict(log_TL_height, newdata=data)
data$logpredictions<-exp(data$logpredictions)

plots$TL_height<-ggplot(data %>% filter(!ID=="52", !ID=="75"), 
                        aes(y = height_mm, x = TL_mm))+
 geom_point(size=3, alpha=0.5)+
 geom_line(aes(y=logpredictions), colour="blue", size=1)

plots$TL_height

data <- subset(data, select = -logpredictions)

#TL - Width
#----------------------------#
#linear
lm_TL_width <- lm(width_mm~TL_mm, data)
summary(lm_TL_width)
coef(lm_TL_width)

#log
log_TL_width <- lm(log(width_mm)~log(TL_mm), data)
summary(log_TL_width)

#log wins
AIC(lm_TL_width, log_TL_width)

data$logpredictions<-predict(log_TL_width, newdata=data)
data$logpredictions<-exp(data$logpredictions)

#plot
plots$TL_width<-ggplot(data, mapping=aes(y = width_mm, x = TL_mm))+
 geom_point(size=3, alpha=0.5)+
 geom_line(aes(y=logpredictions), colour="blue", size=1)

plots$TL_width
data <- subset(data, select = -logpredictions)


#Width - TL
#----------------------------#
#Linear equation
lm_width_TL <- lm(TL_mm~width_mm, data) 
summary(lm_width_TL)

#Logistic equation (nls)
logistic_width_TL <- nls(TL_mm ~ K/(1 + exp(-b * (width_mm - x0))), 
                         data = data,
                         start = list(K = max(data$TL_mm, na.rm=T)+10,
                                      b = 0.1, x0 = median(data$width_mm, na.rm=T)))
summary(logistic_width_TL)
plot(logistic_width_TL)

#logistic wins
AIC(lm_width_TL, logistic_width_TL)

#plot
plots$width_TL<-ggplot(data, mapping=aes(x = width_mm, y = TL_mm))+
 geom_point(size=3, alpha=0.5)+
 geom_smooth(method="nls",formula="y~K/(1+exp(-b*(x-x0)))",
             method.args=list(start=list(K = max(data$TL_mm, na.rm=T)+10, 
                                         b = 0.1, x0 = median(data$width_mm, na.rm=T))),
             se=FALSE, colour="blue")

plots$width_TL

#TL - Mass
#----------------------------#
log_TL_mass <- lm(log(weight_g)~log(TL_mm), data %>% filter(!dataset=="SN"))
summary(log_TL_mass) #r squared w/ SN data = 0.92, w/o = 0.97 - seasonal diff in wts?

data$logpredictions<-predict(log_TL_mass, newdata=data)
data$logpredictions<-exp(data$logpredictions)

#plot
plots$TL_mass<-ggplot(data %>% filter(!dataset=="SN"), aes(y = weight_g, x = TL_mm))+
 geom_point(size=3, alpha=0.5)+
 geom_line(aes(y=logpredictions), colour="blue", size=1)

plots$TL_mass #appearance suggests some room for further improvement
#but probably not important enough to worry about refining a glm?
data <- subset(data, select = -logpredictions)

#combine plots
#----------------------------#
plots$TLFLSL <-
 with(plots,
      TL_FL+TL_SL)
plots$TLFLSL

plots$TLwidth<-
 with(plots,
      TL_width+width_TL)
plots$TLwidth

plots$TL_mass

#Ages ------------------------------------
mean(data$use.age, na.rm=T)
sd(data$use.age, na.rm=T)

data %>% group_by(sex) %>% summarize(mean=mean(use.age, na.rm=T), sd=sd(use.age, na.rm=T))
#no diff

#calculate morphometrics by age class
ageclasses<-data %>% filter(!is.na(use.age)) %>% mutate(class=case_when(
  use.age <= 4 ~ "1 to 4",
  use.age >= 5 & use.age <= 8 ~ "5 to 8",
  use.age >= 9 & use.age <= 12 ~ "9 to 12",
  use.age >= 13 & use.age <= 16 ~ "13 to 16",
  use.age >= 17 & use.age <= 20 ~ "17 to 20",
  use.age >= 21 & use.age <= 24 ~ "21 to 24",
  use.age >= 25 & use.age <= 28 ~ "25 to 28",
  use.age >= 29 & use.age <= 31 ~ "29 to 31"
  )) %>% group_by(class) %>% 
 mutate(n=n(), meanTL=mean(TL_mm, na.rm=T), sdTL=sd(TL_mm, na.rm=T),
        meanWidth=mean(width_mm, na.rm=T), sdWidth=sd(width_mm, na.rm=T),
        meanWeight=mean(weight_g, na.rm=T), sdWeight=sd(weight_g, na.rm=T),
        meanHeight=mean(height_mm, na.rm=T), sdHeight=sd(height_mm, na.rm=T)) %>% 
 ungroup() %>% 
 mutate(class=factor(class, levels=c(
   "1 to 4", "5 to 8", "9 to 12", "13 to 16", 
   "17 to 20", "21 to 24", "25 to 28", "29 to 31"))) %>% 
 arrange(class)

ageclasses

plots$TL_ageclass<-ggplot(ageclasses, aes(y=TL_mm, x=class, group=class))+
 geom_boxplot(outlier.shape="asterisk")+
 stat_summary(fun="mean", geom="point")
 #geom_errorbar(aes(ymin=meanTL-sdTL, ymax=meanTL+sdTL))

plots$width_ageclass<-ggplot(ageclasses, aes(y=width_mm, x=class, group=class))+
 geom_boxplot(outlier.shape="asterisk")+
 stat_summary(fun="mean", geom="point")

plots$ageclass<-with(plots,
                     TL_ageclass/width_ageclass)
plots$ageclass

#export for table
ageclasstable<-ageclasses %>% 
 select(class, n, meanTL, sdTL, meanWidth, sdWidth, 
        meanHeight, sdHeight, meanWeight, sdWeight) %>% distinct()

ageclasstable<-as.data.frame(lapply(ageclasstable, function(x)
                                  if(is.numeric(x)) round(x, digits = 0) else x))
ageclasstable<-ageclasstable %>% 
 unite("Total length (mm)", meanTL, sdTL, sep =" ± ") %>% 
 unite("Width (mm)", meanWidth, sdWidth, sep=" ± ") %>% 
 unite("Mass (g)", meanWeight, sdWeight, sep=" ± ") %>% 
 unite("Height (mm)", meanHeight, sdHeight, sep=" ± ")

write_csv(ageclasstable, "ageclasses.csv")


#visualize age distrib
#----------------------------#
agehist<-ggplot(data, aes(use.age))+
 geom_histogram(binwidth=2, fill="lightgrey", colour="black")
agehist


###Histograms
#----------------------------#

plots$HistObsAge <- ggplot(data, aes(x=use.age, y=stat(density)))+  #uses observed ages 
 geom_freqpoly(bins=31, fill="lightgrey", colour="black")+
 labs(x = "Observed Age")
plots$HistObsAge

plots$HistPredAge <- ggplot(data1, aes(x=pred.age, y=stat(density)))+  #uses crappy fitting VBGM
 geom_freqpoly(bins=31, fill='lightgrey', colour='black')+
 labs(x = "Predicted Age (incl. batch fish)")
plots$HistPredAge

plots$HistPredAge2 <- ggplot(data1 %>% filter(!dataset=="batch"), #predicted ages sans batch fish
                             aes(x=pred.age, y=stat(density)))+  #uses crappy fitting VBGM
 geom_freqpoly(bins=31, fill='lightgrey', colour='black')+
 labs(x = "Predicted Age (excl. batch fish)")
plots$HistPredAge2

#Combine histograms
plots$combined2 <-
 with(plots,
       HistObsAge/
       HistPredAge2/
       HistPredAge
 )
plots$combined2

#Fecundity-----------------------------------------


#TL-Fecundity (from existing equation)
#----------------------------#
plots$LengthFecundityBatch <-
 ggplot(data1, aes(x = TL_mm, y = pred.eggs, colour = pred.age))+
 geom_point(size=3, alpha=0.5)+
 #geom_rug(position="jitter", sides = "b")+
 #geom_vline(xintercept = c(71, 170, 243, 298, 338, 368, 390),
           # colour="dark grey")+
 scale_colour_viridis_c()

plots$LengthFecundityBatch

#Width-Fecundity
#----------------------------#
#model relationship
#trimming fish without width measurements as well as those that are not fecund
d1_WidthFecundity<-data1 %>% 
 filter(!is.na(width_mm), TL_mm>100)

#log model
model_eggs_log <- lm(log(pred.eggs) ~ log(width_mm), data=d1_WidthFecundity)
d1_WidthFecundity$logpredictions<-exp(predict(object = model_eggs_log))
summary(model_eggs_log)
coef(model_eggs_log)

#glm with log link
log_glm_eggs<-glm(pred.eggs~width_mm, family=gaussian(link="log"), data=d1_WidthFecundity)
d1_WidthFecundity$glmpredictions<-exp(predict(object = log_glm_eggs))

AIC(log_glm_eggs, model_eggs_log)

#log plot
plots$EggWidth <- 
 ggplot(d1_WidthFecundity, aes(x = width_mm, y = pred.eggs)) +
 geom_point(alpha = 0.5, size=1.25) +
 geom_line(aes(x = width_mm, y = logpredictions), size=1.25, colour="blue")+
 #geom_line(aes(x = width_mm, y = glmpredictions), size=1.25, colour="red")+
 #geom_vline(xintercept=c(30, 40, 50))+
 labs(x = "Width (mm)", y = "Predicted Number of Eggs")+
 scale_y_continuous(limits=c(0,22000))

plots$EggWidth

rm(d1_WidthFecundity)

#Fecundity descriptive stats/plots
#----------------------------#
range(data1$pred.eggs, na.rm=T)
mean(data1$pred.eggs, na.rm=T)
sd(data1$pred.eggs, na.rm=T)

plots$HistEggs <- ggplot(data1 %>% filter(!is.na(pred.eggs)), aes(x=pred.eggs))+
 geom_histogram(bins=10, colour="black", fill="lightgrey")+
     labs(x = "Predicted fecundity (number of eggs)", y= "Goldfish count")
plots$HistEggs

plots$HistEggs2 <- ggplot(data1 %>% filter(!is.na(pred.eggs)), aes(x=width_mm))+
 geom_histogram(aes(weight=pred.eggs), binwidth=10, boundary=50, colour="black", fill="lightgrey")+
 labs(x = "Width (mm)", y= "Egg count")
plots$HistEggs2

plots$HistEggs3 <- ggplot(data1 %>% filter(!is.na(pred.eggs)), aes(x=TL_mm))+
 geom_histogram(aes(weight=pred.eggs), binwidth=50, boundary=200, colour="black", fill="lightgrey")+
 labs(x = "Total length (mm)", y= "Egg count")
plots$HistEggs3

plots$combined3 <-
 with(plots,
      HistEggs/
       HistEggs2
 )
plots$combined3


#cumulative egg passage: see bottom of script0-3



#--------------------------------### END OF SCRIPT ### ------------------------------------------------------#



#### Random Plots/Scrap code etc. ####

## -- 4. Observed Age and predicted fecundity by length -- ##

plots$LengthFecundityAge <-
 ggplot(data1 %>% filter(!is.na(age)), aes(x = TL_mm, y = pred.eggs, colour = age))+
 geom_point(size=3, alpha=0.5)+
 geom_rug(position="jitter", sides = "b")+
 geom_vline(xintercept = c(71, 170, 243, 298, 338, 368, 390), #Numbers are age class years
            colour="dark grey")+
 scale_colour_viridis_c()

plots$LengthFecundityAge

## -- 5. Gonad mass and predicted fecundity by length -- ##

plots$LengthFecundityGonads <-
 ggplot(data1%>% filter(!is.na(gonads_g)), aes(x = TL_mm, y = pred.eggs, colour = gonads_g))+
 geom_point(size=3, alpha=0.5)+
 geom_rug(position="jitter", sides = "b")+
 geom_vline(xintercept = c(71, 170, 243, 298, 338, 368, 390), #Numbers are age class years
            colour="dark grey")+
 scale_colour_viridis_c()

plots$LengthFecundityGonads

