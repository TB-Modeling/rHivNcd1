#
#  R HIVNCD 2022
#  Main Execution
#  
#####################################
# Rcpp for C++ integration
library(Rcpp)

#Initial Setup for random seed
set.seed(1)

##SOURCE R FILES##
source("person.R")
source("stats.R")
source("rCoreFunctions.R")
source("rHelperFunctions.R")
source("globalVariables.R")
source("hivInputs.R")


##COMPILE THE Rcpp##
cat("Compiling C++ ... ")
sourceCpp("cHelperFunctions.cpp")
sourceCpp("cCoreFunctions.cpp")
cat("Rcpp code compiled \n")

# Set global variables 
mc$YNOW=mc$INITIAL.YEAR
mc$ANNUAL.TIMESTEPS=12 #modeling monthly dynamics

#@MS: need to align variable names between HIV and NCD models (FEMALE vs female?) (number of HIV states: 4 or 5?)
#@redo the HIV output to only have 4 HIV states instead of 5
#########################################################################################
#Create initial population 
#########################################################################################
cat("Generating Population ... ")
pop<-create.initial.population(n = mc$POP.SIZE)

cat("initial states: ")
array(cReturnHivStates(pop),dimnames = list(mc$DIM.NAMES.HIV))
array(cReturnNcdStates(pop),dimnames = list(mc$DIM.NAMES.NCD))
array(unlist(cReturnHivNcdStates(pop)),dim = c(4,4),dimnames = list(
                                     mc$DIM.NAMES.NCD,
                                     mc$DIM.NAMES.HIV))
#'@MS: since you have more experience with array operations in R, 
#'can you explain to me how unlist and array work hand in hand to get the dim order correctly?
barplot(cReturnAgDist(pop),names.arg = mc$DIM.NAME.AGEGROUP,main=paste("Age distribution tnow=",mc$TNOW))

#######
# Set initial HIV status in 2015
#'@JP: this is super slow, how can we rewrite to be faster?
invisible(mapply(set.initial.hiv.status,c(1:length(pop)))) 
array(unlist(cReturnHivNcdStates(pop)),dim = c(4,4),dimnames = list(
  mc$DIM.NAMES.NCD,
  mc$DIM.NAMES.HIV))

#######
 

