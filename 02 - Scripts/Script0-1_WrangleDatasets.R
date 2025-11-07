## --------------------------------------------------------------#
## Script name: Script0-1_WrangleDatasets
##
## Purpose:
## Transform and merge 4 raw sets of Goldfish data
## Produce clean dataframes for further visualization and analyses
##
## First in a series of scripts focused on:
## Describing Hamilton Harbour Goldfish population demographics
## Informing selective barrier design on the basis of body width and fecundity
##
##
## Author: 
## Cole MacLeod, adapted from Paul Bzonek, Simon Fernandes and Nicole Turner 
## 
##
## Original Date Created: 24 Oct 2025
## Version: 2025-11-07
## ---------------------------------------------------------------#
###Load packages
library(tidyverse)

#### Individual Fish ----------------------------------------

### Load data
#----------------------------#

#DFO field sampling
data_field <- read.csv("01 - Data/2025-10-24_WL.csv", na.strings=c("NA", "")) # WL_GF_6May25.csv; "new_WL_GF_22Apr.csv" ; "WL_GF_6May25.csv"
data_field$ID<-as.character(data_field$ID) 
data_field$weight_g<-as.numeric(data_field$weight_g)

#DFO ages and dissection (lab)
data_ages <- read.csv("01 - Data/2025-10-24_Ages.csv", na.strings=c("NA", "", "unk", "M?"))
data_ages$ID<-as.character(data_ages$ID) 

#Load supplementary (small fish) data from Sultan Nessa thesis
data_SN <- read.csv("01 - Data/2025-09-15_Sully.csv", na.strings=c("NA", ""))
data_SN<-data_SN %>% mutate(TL_mm=TL_cm*10, #need lengths as mm
                                  SL_mm=SL_cm*10)

### Merge dataframes
#----------------------------#

#Combine field and lab data (join new columns)
data_DFO<-data_field %>% left_join(data_ages, 
                              by="ID", unmatched="error", relationship="one-to-one") %>%
#some fish in the age dataset (9000 series IDs) were not in the field dataset,
#so created 'blank' placeholder IDs in field file.
#But their morphological data is held with the ages,
#so need to combine (coalesce) morphology columns
mutate(TL_mm=coalesce(TL_mm.x, TL_mm.y), 
        FL_mm=coalesce(FL_mm.x, FL_mm.y), 
        width_mm=coalesce(width_mm.x, width_mm.y), 
        height_mm=coalesce(height_mm.x, height_mm.y),
        weight_g=coalesce(weight_g.x, weight_g.y))

#Combine DFO and SN data
data<-bind_rows(list(DFO=data_DFO, SN=data_SN), .id="dataset")

#select and organize relevant columns
data<-data %>%  
  select(dataset, ID, TL_mm, FL_mm, SL_mm, width_mm, height_mm, weight_g,
        sex=Sex, eggs=Eggs, gonads_g = Gonad.Mass.g, bodywall_mm = Bodywally.Thickness..mm., use.age, age.diff=AgeDiff, oto)

### Save cleaned csv
#----------------------------#
#write_excel_csv(data, "01 - Data/2025-10-29_DFO_SN.csv")
 

 
#### Batch Fish ----------------------------------------
 
### Load data
#----------------------------#
data_batch <- read.csv("01 - Data/2025-05-06_Batch.csv") # GF_batch_6May25.csv
data_batch <- data_batch %>% #clean things up
 mutate(key = row_number()) %>% 
 select(key, class = Size.class, count = n, weight_g)

### Convert to individual observations
#----------------------------#

#Specify size classes
data_batch<- data_batch %>% 
 mutate(TL_min = case_when(class == 1 ~ 1,
                           class == 2 ~ 51,
                           class == 3 ~ 101,
                           class == 4 ~ 151,
                           class == 5 ~ 201,
                           class == 6 ~ 251,
                           class == 7 ~ 301,
                           class == 8 ~ 326,
                           class == 9 ~ 351),
        TL_max = case_when(class == 1 ~ 50,
                           class == 2 ~ 100,
                           class == 3 ~ 150,
                           class == 4 ~ 200,
                           class == 5 ~ 250,
                           class == 6 ~ 300,
                           class == 7 ~ 325,
                           class == 8 ~ 350,
                           class == 9 ~ 400)
 )

#Expand rows and estimate TL, weight, width
#----------------------------#

#Depends on:

#TL to width function 
source("02 - Scripts/01 - Functions/Func2-1_Equations_Misc.R")
#TL to width model
lm_TL_width <- lm(width_mm~TL_mm, data)

#Expand, TL, weight
set.seed(1) #set seed for random TL estimates
df_batch_expanded <- data_batch %>%
 uncount(weights = count, .remove=FALSE, .id="key_expanded") %>%  # repeat each row 'n' times
 mutate(TL_mm = runif(n(), TL_min, TL_max),  # randomize TLs within size class
        weight_g = weight_g/count, #Divide total weight by number of fish
        dataset = "batch", #Log fish as a batch
        ID = paste(key, class, count, key_expanded, sep = "")) #give each batch fish a unique ID

#Width
set.seed(1) #do it again for width (unsure if necessary)
df_batch_expanded<-df_batch_expanded %>% mutate(
width_mm = rnorm(444, mean=predict(lm_TL_width, newdata=df_batch_expanded), 
                 sd=summary(lm_TL_width)$sigma)) #Predict width with variation

### Merge batch and individual dataframes
#----------------------------#

#Depends on:

#TL to age function (from HH GF)
source("02 - Scripts/01 - Functions/Func2-1_Equations_Misc.R")

#TL to egg function (from Lorenzoni eq.) 
source("02 - Scripts/01 - Functions/Func1-1_Lorenzoni_Equations.R")

data1 <- bind_rows(df_batch_expanded, data) %>% 
 mutate(key2 = row_number(), #new, consistently formatted within-df ID
        pred.age = func_HH_TL_Age(TL_mm), #Predict age with function
        pred.age = case_when(pred.age < 0 ~ 0, #Remove negative age estimates
                             TRUE ~ pred.age),
        pred.eggs = func_TL_to_Eggs(TL_mm), #Predict eggs with function
        pred.eggs = case_when(TL_mm < 100 ~ NA, #Remove predictions for immature fish
                              TRUE ~ pred.eggs))%>% 
 select(key=key2, ID, dataset, TL_mm, width_mm, weight_g, 
        gonads_g, pred.eggs, pred.age, age=use.age, class)#pred.age,

### Save cleaned csv
#----------------------------#
#write_excel_csv(data1, "01 - Data/2025-11-05_Batch_DFO_SN.csv")

