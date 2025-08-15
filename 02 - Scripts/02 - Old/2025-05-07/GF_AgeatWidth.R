## --------------------------------------------------------------#
## Script name: GF_AgeatWidth 
##
## Purpose of script: 
##    
## Predict the age of a goldfish of a given width.
## Inform bar spacing to exclude reproductively viable goldfish.
##
## Author: Cole MacLeod, adapted from Simon Fernandes and Nicole Turner 
##
## Date Created: 4 Feb 2025
##
## ---------------------------------------------------------------#

###Load packages 
#----------------------------#
library(tidyverse)
library(ggplot2)

#Lorenzoni 2010 von bert
L10_aal <- function(L) {
  0.162 - log(1 - (L / 430.19)) / 0.272}

L10_laa <- function(t) {
  430.19 * (1 - exp(-0.272 * (t - 0.162)))}
### data ---------------

#2025 GF Sampling
WL_GF <- read.csv("new_WL_GF_22Apr.csv")

#transformations and analyses --------------
#Generate age estimates

WL_GF <- WL_GF %>% 
  mutate(age.estimate = L10_aal(TL_mm))

#sqrt width~age model
age1<-data.frame(age.estimate=1)
widthage_sqrt<-lm(width_mm~sqrt(age.estimate), data=WL_GF)  
#predict width at age 1
predict(widthage_sqrt, newdata=age1, interval='confidence')

#fecundity
#convert to SL in cm then estimate eggs (both equations from Lorenzoni 2010)
#don't calculate estimates for fish < 109 mm TL because they aren't potentially mature
WL_GF<-WL_GF %>% filter(TL_mm>109) %>% 
  mutate(SL=((TL_mm/10)/1.215) - 0.067) %>% mutate(SLeggs=0.0041*SL^4.368)

#egg~TL curve
plot(SLeggs~TL_mm, WL_GF)

#various models
eggs.lm<-lm(SLeggs~width_mm, WL_GF)
summary(eggs.lm)

eggs_TL<-lm(log(SLeggs)~TL_mm, WL_GF)

eggs.log<-lm(log(SLeggs)~width_mm, WL_GF)

summary(eggs.log)

#eggs=79.4*1.07^width - back calc'd exp equation from log model

#fecundity at widths
length109<-data.frame(TL_mm=109)
width<-data.frame(width_mm=50)

W_TL<-lm(width_mm~TL_mm, WL_GF)
TL_W<-lm(TL_mm~width_mm, WL_GF)

predict(W_TL, newdata=length109, interval='confidence')
predict(TL_W, newdata=width25, interval='confidence')

spacing50<-data.frame(width_mm=50)
spacing30<-data.frame(width_mm=37)

spacing
predict(eggs.lm, newdata=width, interval="confidence")

#formatted plots --------------------------------

#width at age 
Agewidthplot<-ggplot(WL_GF, aes(x=age.estimate, y=width_mm), na.rm=TRUE)+
  geom_point(size=2)+
  geom_line(y=50, colour='black', size=1.5, linetype="dashed")+
  geom_smooth(method='lm', se=FALSE, size=1.25, formula = y~sqrt(x))+
scale_x_continuous(n.breaks=6)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position=c(0.25,0.85),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12),
        axis.title=element_text(size=18),
        axis.text=element_text(size=14, color="black"),
        axis.ticks=element_blank(),
        axis.line = element_line(colour = "black", size=0.75),
        panel.border=element_blank())+
  ylab("Width (mm)")+
  xlab("Age")

Agewidthplot

ggsave("Agewidthplot_widthline.png", width = 20, height=15, units = "cm")


#lengthwidth
#predict length at 56 mm

TL_W<-(lm(width_mm~TL_mm, WL_GF))
predict(W_TL, newdata=age56, interval="confidence")

W_TL<-(lm(TL_mm~width_mm, WL_GF))
#hhdata <- WL_GF %>% filter(Location=='HH') 
summary(lm(width_mm~TL_mm, WL_GF))

lengthwidthplot<-ggplot(WL_GF, mapping = aes(x=TL_mm, width_mm), na.rm=TRUE)+
  geom_line(y=55, colour='black', size=1.5, linetype="dashed")+
  geom_point(size=2)+
  geom_smooth(method='lm', se=FALSE, size=1.25)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.text=element_text(size=12),
        axis.title=element_text(size=18),
        axis.text=element_text(size=14, color="black"),
        axis.ticks=element_blank(),
        axis.line = element_line(colour = "black", size=0.75),
        panel.border=element_blank())+
  ylab("Width (mm)")+
  xlab("Total Length (mm)")

  #scale_y_continuous(limits = c(25, 200), breaks=seq(0,200, by=25))
  #scale_x_continuous(limits = c(0, 400), breaks=seq(0,400, by=50))

lengthwidthplot

ggsave("lengthwidthplot.png", width = 20, height=15, units = "cm")

#fecundity width
eggwidthplot<-ggplot(WL_GF %>% filter(TL_mm>109), mapping = aes(x=width_mm, y=SLeggs), na.rm=TRUE)+
  geom_point(size=2)+
  geom_smooth(method='lm', formula = y~x, size=1.25, se=F)+
  #geom_line(inherit.aes=F, mapping= aes(x=width_mm, y=79.4*1.07^width_mm), colour='blue', size=1.5)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.text=element_text(size=12),
        axis.title=element_text(size=18),
        axis.text=element_text(size=14, color="black"),
        axis.ticks=element_blank(),
        axis.line = element_line(colour = "black", size=0.75),
        panel.border=element_blank())+
  ylab("Number of eggs")+
  xlab("Width (mm)")+
  ylim(0, 21000)
eggwidthplot

ggsave("eggwidthplot.png", width = 20, height=15, units = "cm")
