#
#  R HIVNCD 2022
#  R helper functions
#  
#####################################
print("Reading R Helper Functions... ")

# Wrapper functions around Rcpp helper functions to name the outputs outputs
returnHivStates<-function(pop){
  a<-cReturnHivStates(pop)
  names(a)<-DIM.NAMES.HIV
  return (a)
}
returnNcdStates<-function(pop){
  a<-cReturnNcdStates(pop)
  names(a)<-DIM.NAMES.NCD
  return (a)
}
returnHivNcdStates<-function(pop){
  a<-cReturnHivNcdStates(pop)
  names(a)<-DIM.NAMES.HIV
  return (a)
}
returnAgeDist<-function(pop){
  a<-cReturnAgeDist(pop)
  names(a)<-c(1:NUM_AGE_GROUPS)
  return (a)
}
#@MS: plotting functions 