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
# source("hivInputs.R")


##COMPILE THE Rcpp##
cat("Compiling C++ ... ")
sourceCpp("cHelperFunctions.cpp")
# sourceCpp("cCoreFunctions.cpp")
cat("Rcpp code compiled \n")

##LOAD DATA ##
# load hiv sim workspace with: (1) sim object, (2) data manager, (3) extracted data (hiv.output.for.ncd)
load('data/hiv_sim.RData')
# distribution of HIV states for each age/sex category
hivPrev2015 = hiv.output.for.ncd$population["2015",,,]
cat("HIV data loaded")

##########################################################################
# Set global variables 
{
  reset.gss()
  
  mc$TNOW=0
  mc$YNOW=1
  mc$CYNOW=mc$INITIAL.YEAR
  mc$ANNUAL.TIMESTEPS=12 #modeling monthly dynamics
  
  pop=NULL
  ##############
  #Create initial population 
  cat("Generating Population ... ")
  pop<-create.initial.population(n = mc$POP.SIZE)
  # pop<-create.initial.population(n = 100)
  
  cat("initial states: ")
  array(cReturnHivStates(pop),dimnames = list(mc$DIM.NAMES.HIV))
  array(cReturnNcdStates(pop),dimnames = list(mc$DIM.NAMES.NCD))
  array(unlist(cReturnHivNcdStates(pop)),dim = c(4,4),dimnames = list(
    mc$DIM.NAMES.NCD,
    mc$DIM.NAMES.HIV))
  barplot(cReturnAgDist(pop),names.arg = mc$DIM.NAMES.AGE,main=paste("Age distribution year=",mc$CYNOW))
  ##############
  # Set initial HIV status in 2015
  #'@JP: this is super slow, how can we rewrite to be faster?
  invisible(mapply(set.initial.hiv.status,c(1:length(pop)))) 
  array(unlist(cReturnHivNcdStates(pop)),dim = c(4,4),dimnames = list(
    mc$DIM.NAMES.NCD,
    mc$DIM.NAMES.HIV))
}
#######################################################
## RUN:
#initial sim obj
sim<-list(pop=pop,
          mc=mc,
          gss=gss)


for(i in c(mc$INITIAL.YEAR:mc$END.YEAR)){
  
  sim<-model.annual.dynamics(sim)

}



