## --------------------------------------------------------------#
## Script name: Script0-3_ModelFecundity
##
## Purpose of script: 
##    
## Predict the fecundity of a goldfish of a given width.
## Inform bar spacing to exclude reproductively viable goldfish.
## 
##
## Author: 
## Cole MacLeod, adapted from Simon Fernandes and Nicole Turner 
## 
## Modifications
## Rewritten by Paul Bzonek for clarity and future-proofing 2025-05-07
## Significant changes by Cole MacLeod beginning 2025-10-01
##
## Original Date Created: 4 Feb 2025
## Version: 2025-11-07
## ---------------------------------------------------------------#


###Load packages
library(tidyverse)
library(ggplot2)
library(patchwork)

### Load data
#----------------------------#
data<-read.csv("01 - Data/2025-12-12_DFO_SN.csv")
data1<-read.csv("01 - Data/2025-12-12_Batch_DFO_SN.csv")


### Specify objects and parameters
#----------------------------#
theme_set(theme_classic())
options(scip=99)
param_widths <- data.frame(width_mm=c(50, 40, 30)) #Target widths for model predictions
df_testresults <- data.frame() #Home for test results
plots <- list() #home for plots


##### Model fecundity ################################################----
#-------------------------------------------------------------# 

#Prep dataset
data_model <- data1 %>% #Trim fish without width measurements
 filter(TL_mm > 100, !is.na(width_mm)) %>% #and fish that aren't fecund
 select(key, width_mm, height_mm, TL_mm, pred.eggs)   
 

###Linear model
#----------------------------#
model_eggs_lm <- lm(pred.eggs ~ width_mm, data=data_model)
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
model_eggs_log <- lm(log(pred.eggs) ~ log(width_mm), data=data_model)
summary(model_eggs_log)
#plot(model_eggs_log)
data_model$results_eggs_log <- exp(predict(object = model_eggs_log)) #Predict slope
#Predict confidence intervals and add to dataframe
tempA <- param_widths %>% 
         cbind(model= "log", 
               predict(model_eggs_log, newdata=param_widths, interval='confidence')) %>% 
      as.data.frame() %>%  
      pivot_longer(cols = fit:upr, names_to = "interval", values_to = "yhat") %>% 
      mutate(yhat= exp(yhat))
df_testresults <- rbind(df_testresults, tempA)

AIC(model_eggs_lm, model_eggs_log)


##### Compare models #######################################----
#-------------------------------------------------------------# 

###Plot the curves
#----------------------------#
#Mutate data for plotting
temp_df_plot <- data_model %>%
 pivot_longer(cols = c(results_eggs_lm, results_eggs_log, #results_eggs_sqrt, #results_eggs_nls
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
ggplot(data1, aes(x = width_mm, y = pred.eggs)) +
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
tibble(model = c("lm", "log"),
       yhat = c(summary(model_eggs_lm)$r.squared,
                summary(model_eggs_log)$r.squared)
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

#log wins

##### Incorporate batch data to estimate cumulative egg passage #########################----
#-------------------------------------------------------------# 
data_batch_cumulativeW <- data1 %>%
 arrange(width_mm) %>% #Arrange by ascending width
 mutate(pred.eggs = replace_na(pred.eggs, 0),
        pred.eggs.remaining = cumsum(pred.eggs), # cumulative sum below width threshold
        prop.remaining = pred.eggs.remaining / max(pred.eggs.remaining, na.rm=TRUE)
        )  

data_batch_cumulativeH <- data1 %>%
 arrange(height_mm) %>% #Arrange by ascending width
 mutate(pred.eggs = replace_na(pred.eggs, 0),
        pred.eggs.remaining = cumsum(pred.eggs), # cumulative sum below width threshold
        prop.remaining = pred.eggs.remaining / max(pred.eggs.remaining, na.rm=TRUE)
 )  
#Predict ageclass TL into ageclass widths (0, 5, 10, 15, 20, 25, 30)
#func_TL_to_Width(c(71, 170, 243, 298, 338, 368, 390))


#Plot
#age gives an idea of the number of spawning events an individual could get off before being excluded
#but predicted ages are not very accurate so did not include in rough draft fig
plots$CumulativeEggCurveW <-
 ggplot(data_batch_cumulativeW) +
 geom_line(mapping=aes(x = width_mm, y = prop.remaining), color = "firebrick", size = 1.25)+
geom_vline(xintercept=c(35, 44, 48), size=1, linetype="dashed")+
 theme(axis.line = element_line(linewidth=1),
       axis.text=element_text(size=14, colour="black"),
       axis.title=element_text(size=14, colour="black"))+
 labs(x = "Body Width (mm)", y = "Proportion of Eggs in Population") 

plots$CumulativeEggCurveH <-
 ggplot(data_batch_cumulativeH) +
 geom_line(mapping=aes(x = height_mm, y = prop.remaining), color = "firebrick", size = 1.25)+
 geom_vline(xintercept=c(70, 89, 95), size=1, linetype="dashed")+
 theme(axis.line = element_line(linewidth=1),
       axis.text=element_text(size=14, colour="black"),
       axis.title=element_text(size=14, colour="black"))+
 labs(x = "Height (mm)", y = "") 

plots$CumulativeEggCurve<-with(plots,
                               CumulativeEggCurveW+CumulativeEggCurveH)
plots$CumulativeEggCurve

##------------------------------END------------------------------##
