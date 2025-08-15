## --------------------------------------------------------------#
## Script name: Script0-1-1_ModelFecundity_OldData
##
## Purpose of script: 
##    
## Produce a plot output with the old dataset. 
## This script is no longer active
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


###Import data
#----------------------------#
data_raw <- read.csv("new_WL_GF_22Apr.csv") #"new_WL_GF_22Apr.csv" ; "WL_GF_6May25.csv"
data_batch_raw <- read.csv("GF_batch_6May25.csv")

### Specify objects and parameters
#----------------------------#
param_widths <- data.frame(width_mm=c(50, 40, 30)) #Target widths for model predictions
df_testresults <- data.frame() #Home for test results
oldplots <- list() #home for oldplots


#####Parameterize functions from Lorenzoni et al. 2010 ###########----
#-------------------------------------------------------------# 

#' Estimate age from Tail length
#' @name func_TL_to_Age
#' @param TL Tail length of goldfish in mm. 
#'              The function will fail if length exceeds 430 mm
#' @return   Predicted age in years
#' @author   Nicole Turner
#' @notes    From von Bertalanffy growth function
#'              Age = t0 - log(1 - (TL / Linf)) / k
func_TL_to_Age <- function(TL) {
 Age = 0.162 - log(1 - (TL / 430.19)) / 0.272
 
 return(Age)
}

#' Estimate fecundity from Tail length
#' @name func_TL_to_Eggs
#' @param TL Tail length of goldfish in mm. 
#' @return   Predicted count of eggs
#' @author   Cole Mac leod
#' @notes    Combines TL to SL conversion with SL to fecundity function from Lorenzoni et al.

func_TL_to_Eggs <- function(TL) {
 SL = ((TL/10)/1.215) - 0.067
 Eggs = 0.0041*SL^4.368
 Eggs <- as.integer(Eggs)
 return(Eggs)
}



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
oldplots$LengthFecundity <-
 ggplot(data1, aes(x = TL_mm, y = pred_eggs, colour = pred_age))+
 geom_point(size=3, alpha=0.25)+
 geom_rug(position="jitter", sides = "b")+
 geom_vline(xintercept = c(86, 170, 230, 280, 315, 343, 363, 380, 392, 400),
            colour="dark grey")+
 scale_colour_viridis_c()

oldplots$LengthFecundity

### Model GF length with Width 
#----------------------------#
oldplots$LengthWidth <-
 ggplot(data1, aes(x = width_mm, y = TL_mm, colour = pred_age))+
 geom_point(size=3, alpha=0.5)+
 geom_smooth()+
 geom_rug(position="jitter", sides = "b")+
 scale_colour_viridis_c()

oldplots$LengthWidth

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

#' Estimate width from Tail length
#' @name func_TL_to_Width
#' @param TL Tail length of goldfish in mm. 
#' @return   Predicted width in mm
#' @author   Paul Bzonek
#' @notes    Relationship is from rough linear model model_TL2
#'             The relationship is not truly linear and should be revisited

func_TL_to_Width <- function(TL) {
 tempdata = data.frame(TL_mm = TL) #Format TL param to be read by predict())
 Width = predict(object = model_TL2, newdata = tempdata) #use predict() function
 return(Width)
}


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


###Exponential nls model
#----------------------------#
model_eggs_nls <- nls(pred_eggs ~ a * exp(b * width_mm), data = data_model,
                      start = list(a = 100, b = 0.01))  # Provide starting values!
summary(model_eggs_nls)
#plot(model_eggs_sqrt)
data_model$results_eggs_nls <- predict(object = model_eggs_nls, data = data_model$width_mm)
tempA <- param_widths %>% 
 cbind(model= "nls", interval = "fit",
       yhat = predict(model_eggs_nls, param_widths, interval='confidence')) %>% 
 as.data.frame() 
df_testresults <- rbind(df_testresults, tempA)

#pseudoR2
temp_rss <- with(data_model, sum((pred_eggs - results_eggs_nls)^2))  # Residual sum of squares
temp_tss <- with(data_model, sum((pred_eggs - mean(pred_eggs))^2))  # Total sum of squares
model_eggs_nls$pseudoRsquared <- 1 - temp_rss / temp_tss

rm(list = paste(ls(pattern="temp"))) #Remove environment objects with 'temp' in name



##### Plot the data models #######################################----
#-------------------------------------------------------------# 

###Plot the curves
#----------------------------#
#Mutate data for plotting
temp_df_plot <- data_model %>%
 pivot_longer(cols = c(results_eggs_lm, results_eggs_log, results_eggs_sqrt, results_eggs_nls),
              names_to = "Model",
              values_to = "Prediction"
 ) %>%
 mutate(
  Label = case_when(
   Model == "results_eggs_lm" ~ "Linear Model",
   Model == "results_eggs_log" ~ "Log Model",
   Model == "results_eggs_sqrt" ~ "Sqrt Model",
   Model == "results_eggs_nls" ~ "Exponential Model"
  ),
  Color = case_when(
   Model == "results_eggs_lm" ~ "#c699e7",
   Model == "results_eggs_log" ~ "#2b4206",
   Model == "results_eggs_sqrt" ~ "#013ba5",
   Model == "results_eggs_nls" ~ "#df9c73"
  )
 )

#Build plot
oldplots$EggWidth <- 
 ggplot(data1, aes(x = width_mm, y = pred_eggs)) +
 geom_point(alpha = 0.5) +
 geom_line(data = temp_df_plot,
           aes(x = width_mm, y = Prediction, color = Label),
           size = 2) +
 geom_vline(xintercept=c(30, 40, 50))+
 scale_color_manual(values = setNames(temp_df_plot$Color, temp_df_plot$Label) %>% unique()) +
 labs(title = "1. Predicted egg-width relationship by model",
      x = "Width (mm)", y = "Predicted Eggs", color = "Model Type")
oldplots$EggWidth



###Plot r2
#----------------------------#
oldplots$ModelPerf <-
 tibble(model = c("lm", "log", "nls", "sqrt"),
        yhat = c(summary(model_eggs_lm)$r.squared,
                 summary(model_eggs_log)$r.squared,
                 model_eggs_nls$pseudoRsquared,
                 summary(model_eggs_sqrt)$r.squared
        )
 ) %>% 
 ggplot(aes(x=fct_rev(model), y=yhat, fill=model))+
 geom_segment(aes(x=model, xend=model, 
                  y=yhat, yend=0))+
 geom_point(col="black", size=3.5, pch=21)+
 geom_hline(yintercept = 0, linetype=2)+
 scale_fill_manual(values = c("#2b4206","#013ba5", "#c699e7", "#df9c73")) +
 coord_flip()+
 theme_bw()+
 labs(title = "2. Model performance",
      y = "Predicted fecundity", x = "Model Type", fill = "Model Type")
oldplots$ModelPerf


###Plot predicted confidence intervals
#----------------------------#
oldplots$ModelPreds <- 
 ggplot(df_testresults, aes(x=width_mm, y=yhat, fill = model))+
 geom_point(size=3.5, pch=21)+
 geom_hline(yintercept = 0, linetype=2)+
 coord_flip()+
 facet_wrap(~as.factor(model), ncol=1)+
 theme_bw()+
 scale_fill_manual(values = c("#2b4206","#013ba5", "#c699e7", "#df9c73")) +
 labs(title = "3. Predicted fecundity at target widths",
      y = "Predicted fecundity", x = "Model Type", fill = "Model Type")+
 theme_minimal()
oldplots$ModelPreds

###Combine oldplots
#----------------------------#
oldplots$combined1 <-
 with(oldplots,
      EggWidth/
       (ModelPerf + ModelPreds)
 )
oldplots$combined1


#Remove everything but the final plot
rm(list = setdiff(ls(), "oldplots"))

oldplots$combined1
