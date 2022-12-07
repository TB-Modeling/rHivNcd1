#
#  R HIVNCD 2022
#  Main Execution
#  
#####################################
# Rcpp for C++ integration
# library(Rcpp)

##SOURCE R FILES##   #' @PK: add a wrapper to enclose all these later on
rm(list=ls())


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
res<-run.simulation(rep=2)
sim1<-res[[1]]
sim2<-res[[2]]

sim1$gss$n.births==sim2$gss$n.births






#@JP: I wanted to make population accessible t all classes, so I made a null object in globalvariables and filled that when I created the population. 
#I'm not sure how that may work for the memory management though, specially if we want to rewrite that pop for a new replication.