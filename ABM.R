#
#  R HIVNCD 2022
#  Main Execution
#  
#####################################
# Rcpp for C++ integration
library(Rcpp)

#Initial Setup
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


#########################################################################################
#Create initial population 
#########################################################################################
cat("Generating Population ... ")
dim(mat.initial.pop)

pop<-create.initial.population(TICK)
N=length(pop)
cat(paste0("population created with"),N," persons")
cat("initial states: ")
returnHivStates(pop);
returnNcdStates(pop);
returnHivNcdStates(pop)
# barplot(returnAgeDist(pop),main="ageDist")
#######
# Set initial HIV status
invisible(mapply(set.initial.hiv.status,c(1:length(pop)),TICK))
# cat("new HIV states: ")
# returnHivStates(pop)/N;returnHivNcdStates(pop)
#######
# Set initial NCD status (randomly assigned on top of HIV)
    # invisible(mapply(set.initiatl.ncd.status,c(1:length(pop)),TICK)) - MS removing because we do this when we create initial
# cat("new NCD states: ")
# returnNcdStates(pop)/N;returnHivNcdStates(pop)
################################################

# Annual LOOP:
for (y in c(INITIAL.YEAR:END.YEAR)){
  #annual operations

  # bool modelBirths(mt19937 &rng);
  # bool modelHivDynamics(mt19937 &rng);
  # bool modelNcdDynamics(mt19937 &rng);
  # bool modelDeathsAging(mt19937 &rng);
  #aging
  invisible(lapply(pop,function(x){x$incAge}))
  barplot(returnAgeDist(pop),main="ageDist")

  # bool modelArtCoverage(mt19937 &rng);
  # bool removeDeaths();
  # bool modelIntervention(mt19937 &rng);


  TICK <<- TICK+1
  }


