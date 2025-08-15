#####
####
library(tidyverse)

#######################################################################
#goldfish age length von berlanffy equation 
#*FROM AUSTRALIA** Morgan & Beatty 2007

lenght_at_age <- function(t) {
  374.26 * (1 - exp(-0.651 * (t - 0.0163)))
}

#sub in here what age you want to find the length for
age1<-lenght_at_age(1)
age1


#sub in here what length you want to find the age for 
age_at_length<-function(L) {
  0.0163 - log(1 - (L / 374.26)) / 0.651
}

L176<-age_at_length(176)
L176


RuddAgeTable <- HH_Rudd_GF_LengthWidths_2024 %>% 
  mutate(age.estimate = 0.0163 - log(1 - (ForkLength_mm / 374.26)) / 0.651)


 


