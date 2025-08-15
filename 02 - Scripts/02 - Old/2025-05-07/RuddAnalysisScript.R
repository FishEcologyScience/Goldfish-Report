## --------------------------------------------------------------#
## Script name: RuddAnalysisScript 
##
## Purpose of script: 
##    
##    
##
## Author: Simon Fernandesssss, Nicole Turner 
##
## Date Created: 2024-12-03
##
## --------------------------------------------------------------#  
## Modification Notes:  
##   
## --------------------------------------------------------------#

###Load packages
#----------------------------#
theme_set(theme_classic())
library(tidyverse)
library(ggplot2)


###Import data
#----------------------------#
data_raw <- read.csv("HH_Rudd_GF_LengthWidths_2024.csv")

### Process Nicole's functions
#----------------------------#
length_at_age <- function(t) {
  374.26 * (1 - exp(-0.651 * (t - 0.0163)))
}

age_at_length<-function(L) {
  0.0163 - log(1 - (L / 374.26)) / 0.651
}

 

#####Format data##################################################----
#-------------------------------------------------------------#
#Calculate ages and lengths according to input data
data1 <- data_raw %>% 
  mutate(age.estimate = age_at_length(ForkLength_mm))

data1$logMass <- log10(data1$Mass_g+1)

data1$logLength <- log10 (data1$ForkLength_mm+1)

data1$logWidth <- log10 (data1$Width_mm+1)
#Export table
write.csv(data1, "2024-12-13_SF_Results_HH_Rudd.csv")

#####Plot data##################################################----
#-------------------------------------------------------------#

#####Plots for Rudd Data########

#Fitting a Linear Model and extracting 
lm_model <- lm(logWidth ~ logMass, data = data1)
model_summary <- summary (lm_model)
model_summary$r.squared
slope <- coef(lm_model)[2]

rr_squared <- round(r_squared, 2)
intercept <- coef(lm_model)[1]
r_squared <- summary(lm_model)$r.sqaured


#Base plot with Jon - 
model1 <- lm(logMass~logLength, data1)
summary(model1)

plot(logMass~logLength, data1)
abline(model1)

#Plot Adjusted Fork Length by Adjusted Mass

ggplot(data1, aes(x=logMass, y=logLength))+
  geom_point(shape=21, size=4)+
  geom_smooth(method="lm")
  

#Plot Adjusted Width by Adjusted Mass

ggplot(data1, aes(x=logMass, y=logWidth))+
  geom_point(shape=21, size=4)+
  geom_smooth(method="lm")
  annotate ("text", x = min(data1$logMass, y = max(data1$logWidth),
                            label = paste("y =", round(slope, 2 ), "x +", round(intercept, 2),"R^2 =", round(r_squared, 2)),
                            hjust = 0, vjust = 1, size = 5, color = "blue"))
  
  

#####Plots for Goldfish Data########

#Plot Age estimate by Mass
ggplot(data1, aes(x=Mass_g, y=age.estimate, fill=Mass_g))+
  geom_point(shape=21, size=4)+
  geom_smooth(method="lm")+
  scale_fill_viridis_c()

#Plot Age estimate by Length
ggplot(data1, aes(x=ForkLength_mm, y=age.estimate, fill=Mass_g))+
  geom_point(shape=21, size=4)+
  geom_smooth(method="lm", formula = y~poly(x,2))+
  scale_fill_viridis_c()

#Plot Width by Age estimate 
ggplot(data1, aes(y=Width_mm,x=age.estimate, fill=Mass_g))+
  geom_point(shape=21, size=4)+
  geom_smooth(method="lm", formula = y~poly(x,2))+
  scale_fill_viridis_c()







