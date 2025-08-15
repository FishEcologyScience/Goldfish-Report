## --------------------------------------------------------------#
## Script name: func1-1_Lorenzoni_Equations
##
## Purpose of script: 
##    
## build Lorenzone et al. 2010 equations as functions
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
