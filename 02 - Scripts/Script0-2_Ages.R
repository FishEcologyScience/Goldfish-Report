## --------------------------------------------------------------#
## Script name: Script0-2_Ages
##
## Purpose of script: 
##    
## Construct a von Bertalanffy curve for Hamilton Harbour Goldfish.
## to estimate local Goldfish ages from lengths, 
## and to validate applicability of fecundity estimates from Lorenzoni 2010. 
## Ultimately inform bar spacing to exclude reproductively viable Goldfish.
## 
##
## Author: 
## Cole MacLeod, 
##
## Original Date Created: 04 Sep 2025
## Version: 2025-09-04
## ---------------------------------------------------------------#

##### Setup ---------------

# Load packages
#---------------#
library(tidyverse)
library(ggplot2)
library(patchwork)
library(plotly)
library(FSA)
library(nlstools)

# Set parameters
#---------------#
theme_set(theme_classic())
options(scip=99)

# Import data
#---------------#
data <- read.csv("01 - Data/2025-09-03_Ages.csv")

# Specify objects and parameters
#--------------------------------#
#param_widths <- data.frame(width_mm=c(50, 40, 30)) #Target widths for model predictions
#df_testresults <- data.frame() #Home for test results
plots <- list()

##### Prep data ---------------
data1<-data %>% select(otolith.vial, obs, ID, FL_mm, TL_mm, weight_g, Sex, use.age) %>% 
 filter(!is.na(TL_mm), !is.na(use.age)) %>% 
 filter(!use.age>10 | !TL_mm<200) #illogical

dataF<-data %>% select(otolith.vial, obs, ID, FL_mm, TL_mm, weight_g, Sex, use.age) %>% 
 filter(!is.na(TL_mm), !is.na(use.age), Sex == "F")

dataM<-data %>% select(otolith.vial, obs, ID, FL_mm, TL_mm, weight_g, Sex, use.age) %>% 
 filter(!is.na(TL_mm), !is.na(use.age), Sex == "M") %>% 
 filter(!use.age>10 | !TL_mm<200)

#plots for vb update 1
plots$dataF<-ggplot(dataF,aes(x=use.age, y=TL_mm)) +
 geom_point()+
 labs(title="Females")

plots$dataM<-ggplot(dataM,aes(x=use.age,y=TL_mm)) +
 geom_point()+
 labs(title="Males")

plots$data1<-ggplot(data1,aes(x=use.age,y=TL_mm)) +
 geom_point()+
 labs(title="All fish")


plots$FM1<-
 with(plots,
      data1/(dataF+dataM))
plots$FM1

#### Build vB objects and fit model -----------------

#starting values for nls()
startvals1<-findGrowthStarts(TL_mm~use.age,data=data1)
startvalsF<-findGrowthStarts(TL_mm~use.age,data=dataF)

startvalsM<-findGrowthStarts(TL_mm~use.age,data=dataM)
#start value suggests theoretical age at length 0 is -13 y.o.

###growth model expression
vb<-TL_mm~Linf*(1-exp(-K*(use.age-t0)))

####fit nonlinear model
vb.nls1<-nls(vb, data=data1, start=startvals1)
overview(vb.nls1)
coef1<-coef(vb.nls1)

vb.nlsF<-nls(vb, data=dataF, start=startvalsF)
overview(vb.nlsF)
coefF<-coef(vb.nlsF)

vb.nlsM<-nls(vb, data=dataM, start=startvalsM)
overview(vb.nlsM)
coefM<-coef(vb.nlsM)

###get CIs for model parameters (model object uses normal distrib. theory to
 #estimate CIs - not ideal for nls)

boot1<-nlsBoot(vb.nls1)
confint(boot1,plot=TRUE)

bootF<-nlsBoot(vb.nlsF)
confint(bootF,plot=TRUE)

###visualize ------------------------
#does not use model objects

#female fish
plots$plotF<-ggplot(dataF,aes(x=use.age,y=TL_mm)) +
 geom_smooth(method="nls",formula="y~Linf*(1-exp(-K*(x-t0)))",
             method.args=list(start=startvalsF),se=FALSE) +
 geom_point()+
 labs(title="Females")

#all fish
plots$plot1<-ggplot(data1,aes(x=use.age,y=TL_mm)) +
 geom_smooth(method="nls",formula="y~Linf*(1-exp(-K*(x-t0)))",
             method.args=list(start=startvals1),se=FALSE) +
 geom_point()+
 labs(title="All Goldfish")

plots$vb.1F<-
 with(plots,
      plot1+plotF
      )
plots$vb.1F

#calculate index of growth for each subset --------------------
#permits comparisons among populations

grindex1<-log(coef1[2])+2*log(coef1[1])
names(grindex1)<-NULL

grindexF<-log(coefF[2])+2*log(coefF[1])
names(grindexF)<-NULL

grindexM<-log(coefM[2])+2*log(coefM[1])
names(grindexM)<-NULL


###Render summary markdown
#----------------------------#
rmarkdown::render("02 - Scripts/03 - Reports/vb_notes.Rmd")
