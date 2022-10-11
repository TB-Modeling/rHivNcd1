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

#@MS: need to align variable names between HIV and NCD models (FEMALE vs female?) (number of HIV states: 4 or 5?)
#@redo the HIV output to only have 4 HIV states instead of 5
#########################################################################################
#Create initial population 
#########################################################################################
cat("Generating Population ... ")
pop<-create.initial.population(TICK,n = 10000)

cat("initial states: ")
array(cReturnHivStates(pop),dimnames = list(mc$DIM.NAMES.HIV))
array(cReturnNcdStates(pop),dimnames = list(mc$DIM.NAMES.NCD))
array(unlist(cReturnHivNcdStates(pop)),dim = c(4,4),dimnames = list(
                                     mc$DIM.NAMES.NCD,
                                     mc$DIM.NAMES.HIV))
#'@MS: since you have more experience with array operations in R, 
#'can you explain to me how unlist and array work hand in hand to get the dim order correctly?
# barplot(returnAgeDist(pop),main="ageDist")
#######
# Set initial HIV status in 2015
invisible(mapply(set.initial.hiv.status,c(1:length(pop)))) 
array(unlist(cReturnHivNcdStates(pop)),dim = c(4,4),dimnames = list(
  mc$DIM.NAMES.NCD,
  mc$DIM.NAMES.HIV))

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


