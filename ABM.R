#
#  R HIVNCD 2022
#  Main Execution
#  
#####################################
# Rcpp for C++ integration
library(Rcpp)

##SOURCE R FILES##   #' @PK: add a wrapper to enclose all these later on

source("globalVariables.R")
source("person.R")
source("stats.R")
source("driver.R")
source("rCoreFunctions.R")
source("rHelperFunctions.R")
# source("plots.R") @MS: generates an error


##COMPILE THE Rcpp##
# cat("Compiling C++ ... ")
# sourceCpp("cHelperFunctions.cpp")
# # sourceCpp("cCoreFunctions.cpp")
# cat("Rcpp code compiled \n")

#run the model 
run.simulation(rep=1)

##########################################################################
# Set global variables 
{
  set.seed(1)
  
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
  
  cat("initial states: ")
  return.pop.distribution(var = "sex")
  return.pop.distribution(var = "agegroup")
  return.pop.distribution(var = "hiv")
  return.pop.distribution(var = "ncd")
  
  ##############
  # Set initial HIV status in 2015
  #'@JP: this is super slow, how can we rewrite to be faster?
  invisible(mapply(set.initial.hiv.status,c(1:length(pop)))) 
  return.pop.distribution(var = "hiv")
  return.pop.distribution(var = "hiv.ncd")

  #######################################################
## RUN:
#initial sim obj
sim<-list(pop=pop,
          mc=mc,
          gss=gss)

  # barplot(cReturnAgDist(pop),names.arg = mc$DIM.NAMES.AGE,main=paste("Age distribution year=",mc$CYNOW))
  # print(cReturnAgDist(pop))
  # browser()
  
for(i in c(mc$INITIAL.YEAR:mc$END.YEAR)){
  
  sim<-model.annual.dynamics(sim)

}



