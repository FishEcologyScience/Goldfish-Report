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
library(nlstools)

### Load data
#----------------------------#
data<-read.csv("01 - Data/2025-12-12_DFO_SN.csv")
data1<-read.csv("01 - Data/2025-12-12_Batch_DFO_SN.csv")

#LAA values for other populations
laa<-read.csv("01 - Data/2025-12-19_litLAA.csv")

### Specify objects and parameters
#----------------------------#
theme_set(theme_classic())
options(scip=99)
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
 geom_histogram(binwidth=50, fill="lightgrey", colour="black", size=1)+
 theme(axis.line = element_line(linewidth=1),
       axis.text=element_text(size=14, colour="black"),
       axis.title=element_text(size=14, colour="black"))+
 xlab("Total Length (mm)")

plots$TLhist

#Width
#----------------------------#
range(data1$width_mm, na.rm=TRUE)
mean(data1$width_mm, na.rm=TRUE)
sd(data1$width_mm, na.rm=TRUE)
#visualize width distrib
plots$widthhist<-ggplot(data1, aes(width_mm))+
 geom_histogram(binwidth=10, fill="lightgrey", colour="black", size=1)+
 theme(axis.line = element_line(linewidth=1),
       axis.text=element_text(size=14, colour="black"),
       axis.title=element_text(size=14, colour="black"))+
 xlab("Width (mm)")

plots$widthhist

#mass
#----------------------------#
range(data1$weight_g, na.rm=TRUE)
mean(data1$weight_g, na.rm=TRUE)
sd(data1$weight_g, na.rm=TRUE)
#visualize mass distrib
plots$masshist<-ggplot(data1, aes(weight_g))+
 geom_histogram(binwidth=100, fill="lightgrey", colour="black", size=1)+
 theme(axis.line = element_line(linewidth=1),
       axis.text=element_text(size=14, colour="black"),
       axis.title=element_text(size=14, colour="black"))+
 xlab("Mass (g)")

plots$masshist

#height
#----------------------------#
range(data1$height_mm, na.rm=TRUE)
mean(data1$height_mm, na.rm=TRUE)
sd(data1$height_mm, na.rm=TRUE)
#visualize height distrib
plots$heighthist<-ggplot(data1, aes(height_mm))+
 geom_histogram(binwidth=10, fill="lightgrey", colour="black", size=1)+
 theme(axis.line = element_line(linewidth=1),
       axis.text=element_text(size=14, colour="black"),
       axis.title=element_text(size=14, colour="black"))+
 xlab("Height (mm)")

plots$heighthist

#combine morph plots
#----------------------------#
plots$morphhists <-
 with(plots,
      (TLhist+masshist)/(widthhist+heighthist)
 )
plots$morphhists

#M vs F size diff?
#----------------------------#
data %>% count(sex)
#only have 28 males.. typically need min. 30 to assume representative?

t.test(TL_mm~sex, data)
t.test(width_mm~sex, data)
t.test(weight_g~sex, data)
t.test(height_mm~sex, data)

data %>% filter(!is.na(sex)) %>% group_by(sex) %>%
 summarize(meanTL=mean(TL_mm), sdTL=sd(TL_mm), max=max(TL_mm), min=min(TL_mm))

data %>% filter(!is.na(sex)) %>% group_by(sex) %>%
 summarize(meanWi=mean(width_mm), sdWi=sd(width_mm), max=max(width_mm), min=min(width_mm))

data %>% filter(!is.na(sex)) %>% group_by(sex) %>%
 summarize(meanW=mean(weight_g), sdW=sd(weight_g), max=max(weight_g), min=min(weight_g))

data %>% filter(!is.na(sex)) %>% group_by(sex) %>%
 summarize(meanW=mean(height_mm), sdW=sd(height_mm), max=max(height_mm), min=min(height_mm))

plots$sexTL<-ggplot(data=data %>% filter(!is.na(sex)), aes(y=TL_mm, x=sex, fill=sex))+
 geom_boxplot(outlier.shape="asterisk", colour="black", size=0.75)+
 stat_summary(fun="mean", geom="point", size=2)+
 scale_fill_manual(values=c("lightgrey", "darkgrey"))+
 theme(axis.title.x = element_blank(),
       axis.line = element_line(linewidth=1),
       axis.text=element_text(size=14, colour="black"),
       axis.title.y=element_text(size=14, colour="black"),
       legend.position="none")+
 ylab("Total Length (mm)")

plots$sexWi<-ggplot(data=data %>% filter(!is.na(sex)), aes(y=width_mm, x=sex, fill=sex))+
 geom_boxplot(outlier.shape="asterisk", colour="black", size=0.75)+
 stat_summary(fun="mean", geom="point", size=2)+
 scale_fill_manual(values=c("lightgrey", "darkgrey"))+
 theme(axis.title.x = element_blank(),
       axis.line = element_line(linewidth=1),
       axis.text=element_text(size=14, colour="black"),
       axis.title.y=element_text(size=14, colour="black"),
       legend.position="none")+
 ylab("Width (mm)")

plots$sexW<-ggplot(data=data %>% filter(!is.na(sex)), aes(y=weight_g, x=sex, fill=sex))+
 geom_boxplot(outlier.shape="asterisk", colour="black", size=0.75)+
 stat_summary(fun="mean", geom="point", size=2)+
 scale_fill_manual(values=c("lightgrey", "darkgrey"))+
 theme(axis.title.x = element_blank(),
       axis.line = element_line(linewidth=1),
       axis.text=element_text(size=14, colour="black"),
       axis.title.y=element_text(size=14, colour="black"),
       legend.position="none")+
 ylab("Mass (g)")

plots$sexH<-ggplot(data=data %>% filter(!is.na(sex)), aes(y=height_mm, x=sex, fill=sex))+
 geom_boxplot(outlier.shape="asterisk", colour="black", size=0.75)+
 stat_summary(fun="mean", geom="point", size=2)+
 scale_fill_manual(values=c("lightgrey", "darkgrey"))+
 theme(axis.title.x = element_blank(),
       axis.line = element_line(linewidth=1),
       axis.text=element_text(size=14, colour="black"),
       axis.title.y=element_text(size=14, colour="black"),
       legend.position="none")+
 ylab("Height (mm)")


plots$sexboxplot <-
 with(plots,
      (sexTL+sexW)/(sexWi+sexH)
 )
plots$sexboxplot

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

#log model
log_TL_height<-lm(log(height_mm)~log(TL_mm), data)
summary(log_TL_height)

#predictions
data$logpredictions<-predict(log_TL_height, newdata=data)
data$logpredictions<-exp(data$logpredictions)

#plot data and predictions
plots$TL_height<-ggplot(data, aes(y = height_mm, x = TL_mm))+
 geom_point(size=2, alpha=0.75)+
 geom_line(aes(y=logpredictions), colour="blue", size=1.25)+
 theme(axis.line = element_line(linewidth=1),
       axis.text=element_text(size=14, colour="black"),
       axis.title=element_text(size=14, colour="black"))+
 labs(x="TL (mm)",
      y="Height (mm)")

plots$TL_height

#remove preds from df
data <- subset(data, select = -logpredictions)

#Height - TL
#----------------------------#
#Logistic equation (nls)

hTLstarts<-list(K = max(data$TL_mm, na.rm=T)+10,
                b = 0.1, x0 = median(data$height_mm, na.rm=T))

logistic_height_TL <- nls(TL_mm ~ K/(1 + exp(-b * (height_mm - x0))), 
                         data = data %>% filter(!ID==52), #NAs break bootstrapping
                         start = hTLstarts)
summary(logistic_height_TL)
overview(logistic_height_TL)

#CIs
boot_hTL<-nlsBoot(logistic_height_TL, niter=999)
confint(boot_hTL, plot=TRUE)

#plot
plots$height_TL<-ggplot(data, mapping=aes(x = height_mm, y = TL_mm))+
 geom_point(size=2, alpha=0.75)+
 geom_smooth(method="nls",formula="y~K/(1+exp(-b*(x-x0)))",
             method.args=list(start=list(K = max(data$TL_mm, na.rm=T)+10, 
                                         b = 0.1, x0 = median(data$height_mm, na.rm=T))),
             se=FALSE, colour="blue", linewidth=1.25)+
 theme(axis.line = element_line(linewidth=1),
       axis.text=element_text(size=14, colour="black"),
       axis.title=element_text(size=14, colour="black"))+
 labs(x="Total Length (mm)",
      y="Height (mm)")

plots$height_TL

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
 geom_point(size=2, colour="black", alpha=0.75)+
 geom_line(aes(y=logpredictions), colour="blue", size=1.25)+
 theme(axis.line = element_line(linewidth=1),
       axis.text=element_text(size=14, colour="black"),
       axis.title=element_text(size=14, colour="black"))+
 labs(x="TL (mm)",
      y="Width (mm)")

plots$TL_width
data <- subset(data, select = -logpredictions)


#Width - TL
#----------------------------#
#Linear equation
lm_width_TL <- lm(TL_mm~width_mm, data) 
summary(lm_width_TL)


#Logistic equation (nls)
wiTLstarts<-list(K = max(data$TL_mm, na.rm=T)+10,
                 b = 0.1, x0 = median(data$width_mm, na.rm=T))

logistic_width_TL <- nls(TL_mm ~ K/(1 + exp(-b * (width_mm - x0))), 
                         data = data,
                         start = wiTLstarts)
summary(logistic_width_TL)
plot(logistic_width_TL)

#logistic wins
AIC(lm_width_TL, logistic_width_TL)

#logistic model CIs
boot_wiTL<-nlsBoot(logistic_width_TL, niter=999)
confint(boot_wiTL, plot=TRUE)

#plot
plots$width_TL<-ggplot(data, mapping=aes(x = width_mm, y = TL_mm))+
 geom_point(size=2, alpha=0.75)+
 geom_smooth(method="nls",formula="y~K/(1+exp(-b*(x-x0)))",
             method.args=list(start=list(K = max(data$TL_mm, na.rm=T)+10, 
                                         b = 0.1, x0 = median(data$width_mm, na.rm=T))),
             se=FALSE, colour="blue", linewidth=1.25)+
 theme(axis.line = element_line(linewidth=1),
       axis.text=element_text(size=14, colour="black"),
       axis.title=element_text(size=14, colour="black"))+
 labs(x="Width (mm)",
      y="Total Length (mm)")

plots$width_TL

#TL - Mass
#----------------------------#
log_TL_mass <- lm(log(weight_g)~log(TL_mm), data=data)
summary(log_TL_mass) #r squared w/ SN data = 0.92, w/o = 0.97 - seasonal diff in wts?

data$logpredictions<-predict(log_TL_mass, newdata=data)
data$logpredictions<-exp(data$logpredictions)

#plot
plots$TL_mass<-ggplot(data %>% filter(!dataset=="SN"), aes(y = weight_g, x = TL_mm))+
 geom_point(size=2, alpha=0.75)+
 geom_line(aes(y=logpredictions), colour="blue", size=1.25)+
 theme(axis.line = element_line(linewidth=1),
       axis.text=element_text(size=14, colour="black"),
       axis.title=element_text(size=14, colour="black"))+
 labs(x="TL (mm)",
      y="Mass (g)")

plots$TL_mass #appearance suggests some room for further improvement
#but probably not important enough to worry about refining a glm?
data <- subset(data, select = -logpredictions)

#combine plots
#----------------------------#
plots$TLFLSL <-
 with(plots,
      TL_FL+TL_SL)
plots$TLFLSL

plots$TLwidthheight<-
 with(plots,
      (TL_width+width_TL)/(TL_height+height_TL))
plots$TLwidthheight

plots$TL_mass

#Ages ------------------------------------

#calculate morphometrics by age class
#ageclasses<-data %>% filter(!is.na(use.age)) %>% mutate(class=case_when(
#  use.age <= 4 ~ "1 to 4",
#  use.age >= 5 & use.age <= 8 ~ "5 to 8",
#  use.age >= 9 & use.age <= 12 ~ "9 to 12",
#  use.age >= 13 & use.age <= 16 ~ "13 to 16",
 # use.age >= 17 & use.age <= 20 ~ "17 to 20",
 # use.age >= 21 & use.age <= 24 ~ "21 to 24",
#  use.age >= 25 & use.age <= 28 ~ "25 to 28",
#  use.age >= 29 & use.age <= 31 ~ "29 to 31"
#  )) %>% group_by(class) %>% 
# mutate(n=n(), meanTL=mean(TL_mm, na.rm=T), sdTL=sd(TL_mm, na.rm=T),
 #       meanWidth=mean(width_mm, na.rm=T), sdWidth=sd(width_mm, na.rm=T),
#        meanWeight=mean(weight_g, na.rm=T), sdWeight=sd(weight_g, na.rm=T),
#        meanHeight=mean(height_mm, na.rm=T), sdHeight=sd(height_mm, na.rm=T)) %>% 
# ungroup() %>% 
# mutate(class=factor(class, levels=c(
 #  "1 to 4", "5 to 8", "9 to 12", "13 to 16", 
 #  "17 to 20", "21 to 24", "25 to 28", "29 to 31"))) %>% 
# arrange(class)

#export for table
#ageclasstable<-ageclasses %>% 
# select(class, n, meanTL, sdTL, meanWidth, sdWidth, 
#        meanHeight, sdHeight, meanWeight, sdWeight) %>% distinct()

#ageclasstable<-as.data.frame(lapply(ageclasstable, function(x)
#                                  if(is.numeric(x)) round(x, digits = 0) else x))
#ageclasstable<-ageclasstable %>% 
#unite("Total length (mm)", meanTL, sdTL, sep =" ± ") %>% 
# unite("Width (mm)", meanWidth, sdWidth, sep=" ± ") %>% 
# unite("Mass (g)", meanWeight, sdWeight, sep=" ± ") %>% 
#unite("Height (mm)", meanHeight, sdHeight, sep=" ± ")

#write_csv(ageclasstable, "ageclasses.csv")

#morphometrics by age
#----------------------------#
ages <- data %>%
 group_by(use.age) %>% mutate(
  n=n(), meanTL=mean(TL_mm, na.rm=T), sdTL=sd(TL_mm, na.rm=T), minTL=min(TL_mm, na.rm=T), maxTL=max(TL_mm, na.rm=T),
  meanWidth=mean(width_mm, na.rm=T), sdWidth=sd(width_mm, na.rm=T), minWidth=min(width_mm, na.rm=T), maxWidth=max(width_mm, na.rm=T),
  meanWeight=mean(weight_g, na.rm=T), sdWeight=sd(weight_g, na.rm=T), minWeight=min(weight_g, na.rm=T), maxWeight=max(weight_g, na.rm=T),
  meanHeight=mean(height_mm, na.rm=T), sdHeight=sd(height_mm, na.rm=T), minHeight=min(height_mm, na.rm=T), maxHeight=max(height_mm, na.rm=T)
 ) %>% filter(n>=3) %>% select(n, use.age, meanTL, sdTL, minTL, maxTL, meanWidth, sdWidth, minWidth, maxWidth,
meanHeight, sdHeight, minHeight, maxHeight, meanWeight, sdWeight, minWeight, maxWeight) %>% distinct()

agestable<-as.data.frame(lapply(ages, function(x)
 if(is.numeric(x)) round(x, digits = 0) else x))

agestable<-agestable %>% 
 unite("Total length (mm)", meanTL, sdTL, sep =" ± ") %>% 
 unite("TL range", minTL, maxTL, sep=" - ") %>% 
 unite("Width (mm)", meanWidth, sdWidth, sep=" ± ") %>% 
 unite("Width range", minWidth, maxWidth, sep=" - ") %>% 
 unite("Mass (g)", meanWeight, sdWeight, sep=" ± ") %>% 
 unite("Mass range", minWeight, maxWeight, sep=" - ") %>% 
 unite("Height (mm)", meanHeight, sdHeight, sep=" ± ") %>% 
unite("Height range", minHeight, maxHeight, sep=" - ") 

#morphometric at age plots
#----------------------------#

#TL - age with comparison to other studies
colours<-c("Lorenzoni et al. (2010)" = "red", "Morgan & Beatty (2007)" = "darkblue", "Tarkan et al. (2010)" = "green2",
           "Izci (2004)" = "darkgreen", "Munkittrick & Leatherland (1984)"="darkorchid4", "Mitchell (1979); site (a)" = "gold",
           "Mitchell (1979); site (b)" = "brown4", "Mitchell (1979); site (c)" = "magenta3", "Hamilton Harbour" = 'black')

plots$TL_at_age<-ggplot(ages %>% filter(!use.age %in% c(2, 7, 8, 13, 21, 22, #remove years with n< 3
                                                   24, 25, 26, 27, 29, 30, 31, NA)), 
                        aes(y=meanTL, x=use.age))+
 geom_point(mapping=aes(colour="Hamilton Harbour"), size=2)+
 geom_line(size=0.5)+
 geom_point(inherit.aes = FALSE, data=laa, mapping=aes(y=Lorenzoni_2010, x=age, colour="Lorenzoni et al. (2010)"), size=3, shape=17)+
 geom_line(inherit.aes = FALSE, data=laa, mapping=aes(y=Lorenzoni_2010, x=age), colour="red")+
 geom_point(inherit.aes = FALSE, data=laa, mapping=aes(y=MorganBeatty_2007, x=age, colour="Morgan & Beatty (2007)"), size=3, shape=18)+
 geom_line(inherit.aes = FALSE, data=laa, mapping=aes(y=MorganBeatty_2007, x=age), colour="darkblue")+
 geom_point(inherit.aes = FALSE, data=laa, mapping=aes(y=Tarkan_2010, x=age, colour="Tarkan et al. (2010)"), size=3, shape=0)+
 geom_line(inherit.aes = FALSE, data=laa, mapping=aes(y=Tarkan_2010, x=age), colour="green2")+
 geom_point(inherit.aes = FALSE, data=laa, mapping=aes(y=Izci_2004, x=age, colour="Izci (2004)"), size=3, shape=8)+
 geom_line(inherit.aes = FALSE, data=laa, mapping=aes(y=Izci_2004, x=age), colour="darkgreen")+
 geom_point(inherit.aes = FALSE, data=laa, mapping=aes(y=MunkLeather_1984, x=age, colour="Munkittrick & Leatherland (1984)"), size=3, shape=6)+
 geom_line(inherit.aes = FALSE, data=laa, mapping=aes(y=MunkLeather_1984, x=age), colour="darkorchid4")+
 geom_point(inherit.aes = FALSE, data=laa, mapping=aes(y=Mitchell_1979a, x=age, colour="Mitchell (1979); site (a)"), size=3, shape=9)+
 geom_line(inherit.aes = FALSE, data=laa, mapping=aes(y=Mitchell_1979a, x=age), colour="gold")+
 geom_point(inherit.aes = FALSE, data=laa, mapping=aes(y=Mitchell_1979b, x=age, colour="Mitchell (1979); site (b)"), size=3, shape=10)+
 geom_line(inherit.aes = FALSE, data=laa, mapping=aes(y=Mitchell_1979b, x=age), colour="brown4")+
 geom_point(inherit.aes = FALSE, data=laa, mapping=aes(y=Mitchell_1979c, x=age, colour="Mitchell (1979); site (c)"), size=3, shape=12)+
 geom_line(inherit.aes = FALSE, data=laa, mapping=aes(y=Mitchell_1979c, x=age), colour="magenta3")+
 scale_colour_manual(values=colours)+
  theme(axis.line = element_line(linewidth=1),
       axis.text=element_text(size=14, colour="black"),
       axis.title=element_text(size=14, colour="black"),
       legend.text=element_text(size=12, colour="black"),
       legend.title=element_text(size=14, colour="black"),
       legend.position=c(0.8, 0.3))+
 labs(x="Age",
      y="Total Length (mm)",
      colour="Source")

plots$TL_at_age



#visualize age distrib
#----------------------------#
agehist<-ggplot(data, aes(use.age))+
 geom_histogram(binwidth=1, fill="lightgrey", colour="black")+
 theme(axis.line = element_line(linewidth=1),
       axis.text=element_text(size=14, colour="black"),
       axis.title=element_text(size=14, colour="black"))+
 labs(x="Age")
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

#Gonad mass - Fecundity
#----------------------------#
plots$gmFecundityBatch <-
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

#Fecundity descriptive stats
#----------------------------#
range(data1$pred.eggs, na.rm=T)
mean(data1$pred.eggs, na.rm=T)
sd(data1$pred.eggs, na.rm=T)


#cumulative egg passage: see bottom of script0-3



#--------------------------------### END OF SCRIPT ### ------------------------------------------------------#


