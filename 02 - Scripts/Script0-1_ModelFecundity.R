## --------------------------------------------------------------#
## Script name: Script0-1_ModelFecundity
##
## Purpose of script: 
##    
## Predict the age and fecundity of a goldfish of a given width.
## Inform bar spacing to exclude reproductively viable goldfish.
## 
##
## Author: 
## Cole MacLeod, adapted from Simon Fernandes and Nicole Turner 
## 
## Modifications
## Rewritten by Paul Bzonek for clarity and future-proofing
##
## Original Date Created: 4 Feb 2025
## Version: 2025-05-07
## ---------------------------------------------------------------#

 

#####Import data #################################################----
#-------------------------------------------------------------# 

###Load packages
#----------------------------#
library(tidyverse)
library(ggplot2)
library(patchwork)
library(plotly)

#Set parameters
theme_set(theme_classic())
options(scip=99)

###Source functions
#----------------------------#
source("02 - Scripts/01 - Functions/Func1-1_Lorenzoni_Equations.R")
source("02 - Scripts/01 - Functions/Func2-1_Equations_Misc.R")


###Import data
#----------------------------#
data_raw <- read.csv("01 - Data/2025-05-28_WL.csv") # WL_GF_6May25.csv; "new_WL_GF_22Apr.csv" ; "WL_GF_6May25.csv"
data_batch_raw <- read.csv("01 - Data/2025-05-06_Batch.csv") # GF_batch_6May25.csv

### Specify objects and parameters
#----------------------------#
param_widths <- data.frame(width_mm=c(50, 40, 30)) #Target widths for model predictions
df_testresults <- data.frame() #Home for test results
plots <- list() #home for plots

 

#####Prep dataset ################################################----
#-------------------------------------------------------------# 
### Build dataset with predicted age and fecundity 
#----------------------------#
data1 <- data_raw %>% 
 mutate(key = row_number(),
        weight_g = as.numeric(weight_g),
        pred_age = func_TL_to_Age(TL_mm), #Predict age with function
        pred_eggs = func_TL_to_Eggs(TL_mm), #Predict eggs with function
        pred_eggs = case_when(TL_mm < 100 ~ NA, #Remove predictions for small fish that shouldnt be fecund
                                         TRUE ~ pred_eggs)
       ) 

#Plot predicted age and fecundity by length
plots$LengthFecundity <-
 ggplot(data1, aes(x = TL_mm, y = pred_eggs, colour = pred_age))+
 geom_point(size=3, alpha=0.25)+
 geom_rug(position="jitter", sides = "b")+
 geom_vline(xintercept = c(86, 170, 230, 280, 315, 343, 363, 380, 392, 400), #Numbers are age class years
            colour="dark grey")+
 scale_colour_viridis_c()

plots$LengthFecundity

### Model GF length with Width 
#----------------------------#
plots$LengthWidth <-
 ggplot(data1, aes(x = width_mm, y = TL_mm, colour = pred_age))+
 geom_point(size=3, alpha=0.5)+
 geom_smooth()+
 geom_rug(position="jitter", sides = "b")+
 scale_colour_viridis_c()

plots$LengthWidth

model_TL <- lm(TL_mm~width_mm, data1) #Linear equation
summary(model_TL)

model_TL_logistic <- nls(TL_mm ~ A / (1 + exp(-k * (width_mm - x0))), #Logistic equation
                         data = data1,
                         start = list(A = max(data1$TL_mm)+10, k = 0.1, x0 = median(data1$width_mm))
                         )
summary(model_TL_logistic)


### Model GF Width with Length 
#----------------------------#
ggplot(data1, aes(y = width_mm, x = TL_mm, colour = pred_age))+
 geom_point(size=3, alpha=0.5)+
 geom_smooth()+
 geom_rug(position="jitter", sides = "b")+
 scale_colour_viridis_c()

model_TL2 <- lm(width_mm~TL_mm, data1) #Linear equation
#Okay peformance for now. Could look to improve.
summary(model_TL2)
plot(model_TL2)


##### Model fecundity ################################################----
#-------------------------------------------------------------# 
#Prep dataset
data_model <- data1 %>% 
 filter(TL_mm > 100) %>% #Trim fish that aren't fecund
 select(key, width_mm, TL_mm, pred_eggs)


###Linear model
#----------------------------#
model_eggs_lm <- lm(pred_eggs ~ width_mm, data=data_model)
summary(model_eggs_lm)
#plot(model_eggs_log)
data_model$results_eggs_lm <- predict(object = model_eggs_lm) #Predict slope
#Predict confidence intervals and add to dataframe
tempA <- param_widths %>% 
 cbind(model= "lm", 
       predict(model_eggs_lm, newdata=param_widths, interval='confidence')) %>% 
 as.data.frame() %>%  
 pivot_longer(cols = fit:upr, names_to = "interval", values_to = "yhat")
df_testresults <- rbind(df_testresults, tempA)


###Log model
#----------------------------#
model_eggs_log <- lm(pred_eggs ~ log(width_mm), data=data_model)
summary(model_eggs_log)
#plot(model_eggs_log)
data_model$results_eggs_log <- predict(object = model_eggs_log) #Predict slope
#Predict confidence intervals and add to dataframe
tempA <- param_widths %>% 
         cbind(model= "log", 
               predict(model_eggs_log, newdata=param_widths, interval='confidence')) %>% 
      as.data.frame() %>%  
      pivot_longer(cols = fit:upr, names_to = "interval", values_to = "yhat")
df_testresults <- rbind(df_testresults, tempA)


###Sqrt model
#----------------------------#
model_eggs_sqrt <- lm(pred_eggs ~ sqrt(width_mm), data=data_model)
summary(model_eggs_sqrt)
#plot(model_eggs_sqrt)
data_model$results_eggs_sqrt <- predict(object = model_eggs_sqrt, data = data_model$width_mm)
tempA <- param_widths %>% 
         cbind(model= "sqrt", 
               predict(model_eggs_sqrt, newdata=param_widths, interval='confidence')) %>% 
      as.data.frame() %>%  
      pivot_longer(cols = fit:upr, names_to = "interval", values_to = "yhat")
df_testresults <- rbind(df_testresults, tempA)



##### Plot the data models #######################################----
#-------------------------------------------------------------# 

###Plot the curves
#----------------------------#
#Mutate data for plotting
temp_df_plot <- data_model %>%
 pivot_longer(cols = c(results_eggs_lm, results_eggs_log, results_eggs_sqrt, #results_eggs_nls
                       ),
              names_to = "Model",
              values_to = "Prediction"
              ) %>%
 mutate(
  Label = case_when(
   Model == "results_eggs_lm" ~ "Linear Model",
   Model == "results_eggs_log" ~ "Log Model",
   Model == "results_eggs_sqrt" ~ "Sqrt Model",
  ),
  Color = case_when(
   Model == "results_eggs_lm" ~ "#c699e7",
   Model == "results_eggs_log" ~ "#2b4206",
   Model == "results_eggs_sqrt" ~ "#013ba5",
  )
 )

#Build plot
plots$EggWidth <- 
ggplot(data1, aes(x = width_mm, y = pred_eggs)) +
 geom_point(alpha = 0.5) +
 geom_line(data = temp_df_plot,
           aes(x = width_mm, y = Prediction, color = Label),
           size = 2) +
 geom_vline(xintercept=c(30, 40, 50))+
 scale_color_manual(values = setNames(temp_df_plot$Color, temp_df_plot$Label) %>% unique()) +
 labs(title = "1. Predicted egg-width relationship by model",
      x = "Width (mm)", y = "Predicted Eggs", color = "Model Type")
plots$EggWidth



###Plot r2
#----------------------------#
plots$ModelPerf <-
tibble(model = c("lm", "log", "sqrt"),
       yhat = c(summary(model_eggs_lm)$r.squared,
                summary(model_eggs_log)$r.squared,
                summary(model_eggs_sqrt)$r.squared
               )
       ) %>% 
ggplot(aes(x=fct_rev(model), y=yhat, fill=model))+
 geom_segment(aes(x=model, xend=model, 
                  y=yhat, yend=0))+
 geom_point(col="black", size=3.5, pch=21)+
 geom_hline(yintercept = 0, linetype=2)+
 scale_fill_manual(values = c("#2b4206","#013ba5", "#df9c73")) +
 coord_flip()+
 theme_bw()+
 labs(title = "2. Model performance",
      y = "Predicted fecundity", x = "Model Type", fill = "Model Type")
plots$ModelPerf


###Plot predicted confidence intervals
#----------------------------#
plots$ModelPreds <- 
 ggplot(df_testresults, aes(x=width_mm, y=yhat, fill = model))+
 geom_point(size=3.5, pch=21)+
 geom_hline(yintercept = 0, linetype=2)+
 coord_flip()+
 facet_wrap(~as.factor(model), ncol=1)+
 theme_bw()+
 scale_fill_manual(values = c("#2b4206","#013ba5", #"#c699e7", 
                              "#df9c73")) +
 labs(title = "3. Predicted fecundity at target widths",
      y = "Predicted fecundity", x = "Model Type", fill = "Model Type")+
 theme_minimal()
plots$ModelPreds

###Combine plots
#----------------------------#
plots$combined1 <-
 with(plots,
     EggWidth/
     (ModelPerf + ModelPreds)
     )
plots$combined1



##### Add population age structure ###############################----
#-------------------------------------------------------------# 
data_batch1 <- data_batch_raw %>% #Clean up data
 mutate(key = row_number()) %>% 
 select(key, class = Size.class, count = n, weight_g)

plots$HistBatch <- ggplot(data_batch1, aes(x=class))+
 geom_histogram()+
 labs(title = "Histogram of goldfish per batch size",
      x = "Size class")
plots$HistBatch

#Specify size classes
data_batch1<- data_batch1 %>% 
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

# Expand rows and generate random values
df_batch_expanded <- data_batch1 %>%
 uncount(weights = count, .remove=FALSE, .id="key_expanded") %>%  # repeat each row 'n' times
 mutate(TL_mm = runif(n(), TL_min, TL_max),  # generate random values
        width_mm = func_TL_to_Width(TL_mm), #Predict width with function
        weight_g = weight_g/count, #Divide total weight by number of fish
        pred_age = func_TL_to_Age(TL_mm), #Predict age with function
        pred_eggs = func_TL_to_Eggs(TL_mm), #Predict eggs with function
        pred_eggs = case_when(TL_mm < 100 ~ NA, #Remove predictions for small fish that shouldnt be fecund
                              TRUE ~ pred_eggs),
        batch = TRUE) #Log fish as a batch
 
data_batch2 <- bind_rows(df_batch_expanded, data1) %>% #Combine batch and individual fish
 mutate(key2 = row_number(),
        batch = case_when(is.na(batch) ~ FALSE, #Update batch column
                          TRUE ~ batch)
        ) %>% 
 select(key2, batch, class, weight_g, TL_mm, width_mm, pred_age, pred_eggs)


 

###Plot the combined data
#----------------------------#
###Histograms
plots$HistLength <- ggplot(data_batch2, aes(x=TL_mm))+
 geom_histogram()+
 labs(title = "Histogram of goldfish by length",
      x = "Length")
plots$HistLength

plots$HistAge <- ggplot(data_batch2, aes(x=pred_age))+
 geom_histogram()+
 labs(title = "Histogram of goldfish by age",
      x = "Predicted Age")
plots$HistAge

plots$HistFecundity <- ggplot(data_batch2, aes(x=pred_eggs))+
 geom_histogram()+
 labs(title = "Histogram of goldfish by fecundity",
      x = "Predicted Eggs")
plots$HistFecundity

#Combine historgrams
plots$combined2 <-
with(plots,
     HistBatch/
     HistLength/
     HistAge/
     HistFecundity
)
plots$combined2


#Plot predicted age and fecundity by length
plots$LengthFecundityBatch <-
 ggplot(data_batch2, aes(x = TL_mm, y = pred_eggs, colour = pred_age))+
 geom_point(size=3, alpha=0.1)+
 geom_rug(position="jitter", sides = "b")+
 geom_vline(xintercept = c(86, 170, 230, 280, 315, 343, 363, 380, 392, 400),
            colour="dark grey")+
 scale_colour_viridis_c()

plots$LengthFecundityBatch

 

##### Incorporate cumulative egg passage #########################----
#-------------------------------------------------------------# 
data_batch_cumulative <- data_batch2 %>%
 arrange(width_mm) %>% #Arrange by ascending width
 mutate(pred_eggs = replace_na(pred_eggs, 0),
        pred_eggs_remaining = cumsum(pred_eggs), # cumulative sum below width threshold
        prop_remaining = pred_eggs_remaining / max(pred_eggs_remaining, na.rm=TRUE)
        )  

#Predict ageclass TL into ageclass widths
func_TL_to_Width(c(86, 170, 230, 280, 315, 343, 363, 380, 392, 400))


#Plot
plots$CumulativeEggCurve <-
 ggplot(data_batch_cumulative, aes(x = width_mm, y = prop_remaining)) +
 geom_line(color = "firebrick", size = 1) +
 geom_vline(xintercept = c(10, 28, 40, 51, 58, 64, 68, 72, 74, 76), #Widths by age class
            colour="dark grey")+
 geom_vline(xintercept=c(30, 40, 50))+ #Widths by cutoffs
 labs(x = "Barrier Width (mm) Spacing", y = "Proportion of Eggs Passing Barrier",
      title = "Cumulative Remaining Eggs vs Width Threshold") 

plots$CumulativeEggCurve

###Render the summary markdown
#----------------------------#
rmarkdown::render("Summary1.Rmd")
