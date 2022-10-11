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



#R code:
library(Rcpp)
library(RcppArmadillo)

cat("Compiling ... ")
sourceCpp("cTest.cpp")
cat("done\n")

##Create a 3D array
dimensions = c(3,4,5)
a = array(seq(1,60),dim=dimensions)
#Looks good
print(a)
dim(hiv.probs)
#Send it to C++
testCube(a)


hiv.probs
setInitialHivStates(pop,
                    hiv.probs,
                    1)
# The RcppArmadillo dependency must be imported in both the R and the C++ 
#   (in fact, it replaces  #include<Rcpp.h> in the C++), and
# // [[Rcpp::depends(RcppArmadillo)]]
#  is also required.  Then you can use the arma::cube object to accept any 3D array (as can
# be seen from the R code).  The dimensions of the object can be queried with the n_* series 
# of functions.  For each slice of the 3d object, we examine that slice like it's a matrix, 
# using the ( , ) operator (above I called the slice of the 3D object 'slice', so it's the call to 'slice(k,j)')
# # 