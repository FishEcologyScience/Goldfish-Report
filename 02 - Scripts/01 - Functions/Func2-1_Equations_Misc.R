## --------------------------------------------------------------#
## Script name: func2-1_Equations_Misc
##
## Purpose of script: 
##    
## build miscellaneous equation functions
## 
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
