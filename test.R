#
#  R HIVNCD 2022
#  Main Execution
#  

# Rcpp for C++ integration
library(Rcpp)

#Initial Setup
# set.seed(1)

# source("globalVariables.R")
# source("person.R")
# source("R/stats.R")

##COMPILE THE Rcpp##
cat("Compiling C++ ... ")
sourceCpp("cHelperFunctions.cpp")
sourceCpp("cCoreFunctions.cpp")
cat("Rcpp code compiled \n")



dim=c(2,17)
a<-c(1:(dim[1]*dim[2]))
cat("R 2D array is:")
A=array(a,dim);print(A)
cat(" compared to Rcpp array: ")
B=readArray2D_byCol(a,dim)
# print(B)
cat("done \n")

dim=c(2,17,2)
a<-c(1:(dim[1]*dim[2]*dim[3]))
cat("R 3D array is:")
array(a,dim)
cat(" compared to Rcpp array: ")
v=readArray3D_byCol(a,dim)
cat("done \n")
# 
# 
# 