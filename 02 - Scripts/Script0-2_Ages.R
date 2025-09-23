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
#library(plotly)
library(FSA)
library(nlstools)

# Set parameters
#---------------#
theme_set(theme_classic())
options(scip=99)

# Import data
#---------------#
data <- read.csv("01 - Data/2025-09-03_Ages.csv", na.strings=c("NA", "", "unk", "M?", "Frozen")) 
                                              #ignoring the M? entries in Sex for now. only n=4 
                                              #and not so worried about the male subset anyways
                                              #frozen fish are NA for colour

supp_data <- read.csv("01 - Data/2025-09-15_Sully.csv", na.strings=c("NA", ""))
                                                               
# Specify objects and parameters
#--------------------------------#
plots <- list()

##### Prep data ---------------

# organize fish colours as usable factors

data$colour<- data$colour %>% 
 str_replace_all(c("orange$|orange-black$|gold-white$|yellow-black$" = 
  "Ornamental or Multicolour", "brown$|gold$" = "Naturalized")) %>% 
as.factor() # categorizations subject to change - 
#not sure if these should all be considered ornamental

# convert TL to mm for sully data

supp_data<-supp_data %>% mutate(TL_mm = TL_cm*10)

### prep to merge dataframes

#trim columns main data
data1<-data %>% select(ID, FL_mm, TL_mm, weight_g, Sex, use.age, colour, oto) %>% 
 filter(!is.na(TL_mm), !is.na(use.age)) #%>% 
 #filter(use.age<10 | TL_mm>200) #might be removing too many points here but
    #definitely need to remove a few 
    #e.g. 8 cm fish at 12 years old with naturalized colours??
data1$ID<-as.character(data1$ID) #needs to match sully ID str

#trim columns sully data
supp_data1<-supp_data %>% select(ID, TL_mm, weight_g, Sex, use.age)

#merge
data1<-bind_rows(data1, supp_data1) 

#testing
data1<-data1 %>% mutate(TL_cm=TL_mm/10)

#female-only subset
dataF<-data1 %>% filter(Sex=="F")

#### Build vB objects and fit model -----------------

#starting values for nls()
startvals1<-findGrowthStarts(TL_cm~use.age,data=data1)
startvalsF<-findGrowthStarts(TL_cm~use.age,data=dataF)

#startvalsM<-findGrowthStarts(TL_mm~use.age,data=dataM)
#start value suggests theoretical age at length 0 is -13 y.o.

###growth model expression
vb<-TL_cm~Linf*(1-exp(-K*(use.age-t0)))

###fit nonlinear model
vb.nls1<-nls(vb, data=data1, start=startvals1)
overview(vb.nls1)
coef1<-coef(vb.nls1) #isolate coefficients to calculate growth index

vb.nlsF<-nls(vb, data=dataF, start=startvalsF)
overview(vb.nlsF)
coefF<-coef(vb.nlsF)

# male subset is no good
#vb.nlsM<-nls(vb, data=dataM, start=startvalsM)
#overview(vb.nlsM)
#coefM<-coef(vb.nlsM)

###get CIs for model parameters (model object uses normal distrib. theory to
 #estimate CIs - not ideal for nls)

boot1<-nlsBoot(vb.nls1)
confint(boot1,plot=TRUE)

bootF<-nlsBoot(vb.nlsF)
confint(bootF,plot=TRUE)

###visualize ------------------------
#does not use model objects

#lorenzoni equation
l10<-function(x) {43.019*(1-exp(-0.272*(x-0.162)))}

#female fish
plots$plotF<-ggplot(dataF,aes(x=use.age,y=TL_cm)) +
geom_function(fun=l10)+ 
 geom_smooth(method="nls",formula="y~Linf*(1-exp(-K*(x-t0)))",
             method.args=list(start=startvalsF),se=FALSE) +
 
 geom_point()+
 labs(title="Females")

plots$plotF

#all fish by sex and phenotype
#x and y are swapped
plots$plot1<-ggplot(data1,aes(x=use.age,y=TL_mm)) +
 geom_smooth(method="nls",formula="y~Linf*(1-exp(-K*(x-t0)))",
             method.args=list(start=startvals1),se=FALSE) +
 geom_point()+
 labs(title="All Goldfish")

plots$plot1

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


###Render summary markdown
#----------------------------#
rmarkdown::render("02 - Scripts/03 - Reports/vb_notes.Rmd")
