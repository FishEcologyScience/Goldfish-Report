## --------------------------------------------------------------#
## Script name: Script0-2_VBGM
##
## Purpose of script: 
##    
## Construct a von Bertalanffy curve for Hamilton Harbour Goldfish,
## to relate local Goldfish ages and lengths.
## Ultimately inform bar spacing to exclude reproductively viable Goldfish.
## 
##
## Author: 
## Cole MacLeod
##
## Original Date Created: 04 Sep 2025
## Version: 2025-11-07
## ---------------------------------------------------------------#

##### Setup ---------------

# Load packages
#---------------#
library(tidyverse)
library(ggplot2)
library(FSA)
library(nlstools)

# Set parameters
#---------------#
theme_set(theme_classic())
options(scip=99)


### Load/manipulate data
#----------------------------#
data<-read.csv("01 - Data/2025-10-29_DFO_SN.csv")
data$use.age<-as.numeric(data$use.age)

#nlsboot() breaks if I don't remove unused rows
ages<-data %>% filter(!is.na(use.age), !is.na(TL_mm))                                                              

#### Build objects and fit model -----------------

#VBGM expression
vb<-TL_mm~Linf*(1-exp(-K*(use.age-t0))) 

#starting values for nls()
startvals1<-findGrowthStarts(TL_mm~use.age, data=ages, type = "von Bertalanffy", plot=TRUE)
###fit VBGM
vb.nls1<-nls(vb, data=ages, start=startvals1)

overview(vb.nls1)
coef(vb.nls1)

###get CIs for model parameters (model object uses normal distrib. theory to
 #estimate CIs - not ideal for nls)

boot1<-nlsBoot(vb.nls1, niter=999)
confint(boot1,plot=TRUE)


###plot ------------------------


plot1<-ggplot(ages,aes(x=use.age,y=TL_mm)) +
 geom_smooth(method="nls",formula="y~Linf*(1-exp(-K*(x-t0)))",
             method.args=list(start=startvals1),se=FALSE) +
 geom_point()

plot1
