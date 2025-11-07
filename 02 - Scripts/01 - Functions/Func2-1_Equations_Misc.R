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

#' Estimate age from Tail length - HH GF - Not a great fit
#' @name func_HH_TL_Age
#' @param TL Tail length of goldfish in mm. 
#'              The function will fail if length exceeds 588 mm - seems very unrealistic
#' @return   Predicted age in years
#' @author   Nicole Turner
#' @notes    From von Bertalanffy growth function
#'              Age = t0 - log(1 - (TL / Linf)) / k
func_HH_TL_Age <- function(TL) {
 Age = -4.98 - log(1 - (TL / 587.98)) / 0.0321
 
 return(Age)
}

#' Estimate width from Tail length
#' @name func_TL_to_Width
#' @param TL Tail length of goldfish in mm. 
#' @return   Predicted width in mm
#' @author   Paul Bzonek
#' @notes    Relationship is from rough linear model LM_TL_Width
#'             The relationship is not truly linear and should be revisited

func_TL_to_Width <- function(TL) {
 tempdata = data.frame(TL_mm = TL) #Format TL param to be read by predict())
 Width = predict(object = lm_TL_width, newdata = tempdata) #use predict() function
 return(Width)
}
