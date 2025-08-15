## --------------------------------------------------------------#
## Script name: GF_Morphology
##
## Purpose of script: 
##    
## Predict the age and fecundity of a goldfish of a given width.
## Inform bar spacing to exclude reproductively viable goldfish.
## 
##
## Author: 
## Cole MacLeod, adapted from Simon Fernandes and Nicole Turner 
## SHORTENED VERSION for troubleshooting by Paul Bzonek
##
## Original Date Created: 4 Feb 2025
## Version: 6 May 2025
## ---------------------------------------------------------------#

###Load packages
#----------------------------#
library(tidyverse)
library(ggplot2)

###Import/transform data, functions ---------------

#only new 2025 data
WL_GF <- read.csv("WL_GF_6May25.csv")

#age estimate function
# from equation in Lorenzoni et al. 2010
L10_aal <- function(L) {
  0.162 - log(1 - (L / 430.19)) / 0.272}

#convert TL to SL then estimate fecundity (n eggs)
#Lorenzoni et al. 2010

SLeggs2 <- function(TL) {
  SL = ((TL/10)/1.215) - 0.067 #Convert TL to SL
  eggs = 0.0041*SL^4.368 #Calculate fecundity from SL as per Lorenzoni 2010
  
  return(eggs)
}
  
#estimate ages and eggs
  #don't calculate egg estimates for fish < 109 mm TL because they aren't potentially mature
  WL_GF <- WL_GF %>% 
    mutate(age.estimate = L10_aal(TL_mm)) %>% filter(TL_mm>109) %>% 
    mutate(eggs=SLeggs(TL_mm),
           eggs2 = SLeggs2(TL_mm))
  #convert to integer for poisson model below
  WL_GF$eggs<-as.integer(WL_GF$eggs)

#Analyses -----------
#Overall goal: estimate fecundity at width 

  #widths to predict
  w.tests<-data.frame(width_mm=c(50, 40, 30))
  
#1 EASY: by predicting TL from width (HH data) and directly calculating eggs from TL (L10 equation)
  
tl.w<-lm(TL_mm~width_mm, WL_GF)
summary(tl.w) #R^2 0.79
plot(WL_GF$width_mm, WL_GF$TL_mm) # technically sigmoidal

SLeggs(predict(tl.w, w.tests)) 

#2 IDEAL AND MORE DIFFICULT: various models to directly predict egg estimates from width
  #currently, our egg estimates are a direct function of length - width does not factor in
  #so it is reasonable to assume that if we obtain local egg data,
  #then width will become a stronger predictor 

  #getting egg counts now feels more important than otolith ages.. 
  #focus of paper has slowly shifted away from original coop student draft. something to discuss

#linear 
  eggs.lm<-lm(eggs~width_mm, WL_GF)

  WL_GF %>% mutate(lm_eggs = predict(eggs.lm)) %>% ggplot() +
    geom_point(aes(width_mm, eggs))+
    geom_line(aes(width_mm, lm_eggs))+
    geom_vline(xintercept=c(30, 40, 50))
  #predictions
    predict(eggs.lm, w.tests, interval='confidence')
  # model is obviously bad, but 'next to none at 30 mm' is not a bad practical takeaway 
  #if we aren't worried about fish with <250 eggs 
    
#log
  eggs.log<-lm(log(eggs)~width_mm, WL_GF)

  WL_GF %>% mutate(log_eggs = exp(predict(eggs.log))) %>% ggplot() +
    geom_point(aes(width_mm, eggs))+
    geom_line(aes(width_mm, log_eggs))+
    geom_vline(xintercept=c(30, 40, 50))+
    ylim(0,20000)
  #good fit through the data but any extrapolation would overestimate like crazy
  #predictions
 exp(predict(eggs.log, w.tests, interval='confidence'))

# poisson + log link
 #eggs are (estimates of) count data and variance generally increases with,
 # but is definitely not equal to, the mean. this doesn't appear to fit well, however.
 #possibly because egg estimates are themselves calculated from a basic exponential equation?
  eggs.glm<-glm(eggs~width_mm, data=WL_GF, family=poisson(link = 'log'))
    summary(eggs.glm)
  
  WL_GF %>% mutate(glm_eggs = exp(predict(eggs.glm))) %>% ggplot() +
    geom_point(aes(width_mm, eggs))+
    geom_line(aes(width_mm, glm_eggs))+
    geom_vline(xintercept=c(30, 40, 50))+
    ylim(0,20000)
  #predictions
  exp(predict(eggs.glm, w.tests))

#nls
  
  eggs.nls<-nls(eggs~a*width_mm^b, data=WL_GF, start=list(a=5, b=0.06))
  #used coefficients from eggs.log for starting values a and b, nls algo figured out c on its on
  #there are various other ways to toy with this

  #just looped all the away around back to linear again lol
  WL_GF %>% mutate(nls_eggs = (predict(eggs.nls))) %>% ggplot() +
    geom_point(aes(width_mm, eggs))+
    geom_line(aes(width_mm, nls_eggs))+
    geom_vline(xintercept=c(30, 40, 50))
    #ylim(0,20000)
  predict(eggs.nls, w.tests)
  
#END-------------------------------------------------------------------------------------
  
  
#old work ------------
plot(SLeggs~TL_mm, WL_GF)
#200 mm looks good

#calculate eggs at 200 mm
#TL to SL
(20/1.215)-0.067
#SL to eggs
0.0041*16.39391^4.368
#829 eggs

#predict eggs at 200 mm
eggs_TL<-lm(log(SLeggs)~TL_mm, WL_GF)
length200<-data.frame(TL_mm=200)
predict(eggs_TL, newdata=length200, interval='confidence')
exp(6.355542)
#575 eggs ; 518 to 633

#predict width at 200 mm TL
W_TL<-(lm(width_mm~TL_mm, WL_GF))
predict(W_TL, newdata=length200, interval='confidence')
#37 mm ; 34.6 to 40.3

#predict eggs at 37 mm W
eggs_W<-lm(log(SLeggs)~width_mm, WL_GF)
width37<-data.frame(width_mm=37)
predict(eggs_W, newdata=width37, interval='confidence')
exp(7.115644)
#1231 eggs; 970 to 1562

#try 34.6? low end of CI
width34<-data.frame(width_mm=34.6)
predict(eggs_W, newdata=width34, interval='confidence')
exp(6.95476)
exp(6.699158)
exp(7.210361)
#1048 ; 811 to 1353
#so, a bit of overlap with the source equation (which estimates 829 eggs at 200 mm)
#if you take the low egg prediction from the low width prediction from 200 mm