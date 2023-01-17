#
#  R HIVNCD 2022
#  R helper functions
#  
#####################################
print("Reading R Helper Functions... ")

# a general function to return distributions
print("return.pop.distribution")
# return.pop.distribution<-function(var, # options: sex, agegroup, hiv, ncd
#                                   type="freq" #options: freq (default), prop 
# ){
#   
#   if(var=="sex"){
#     v=c(1:mc$DIM.SEX)
#     res<-unlist(lapply(pop,function(x) {return (x$sex)}))
#     res=lapply(v,function(x){return(length(res[res==v[x]]))})
#     names(res)=mc$DIM.NAMES.SEX
#   }
#   if(var=="agegroup"){
#     v=c(1:mc$DIM.AGE)
#     res<-unlist(lapply(pop,function(x) {return (x$agegroup)}))
#     res=lapply(v,function(x){return(length(res[res==v[x]]))})
#     names(res)=mc$DIM.NAMES.AGE
#   }
#   if(var=="hiv"){
#     v=c(1:mc$DIM.HIV)
#     res<-unlist(lapply(pop,function(x) {return (x$hivState)}))
#     res=lapply(v,function(x){return(length(res[res==v[x]]))})
#     names(res)=mc$DIM.NAMES.HIV
#   }
#   if(var=="ncd"){
#     v=c(1:mc$DIM.NCD)
#     res<-unlist(lapply(pop,function(x) {return (x$ncdState)}))
#     res=lapply(v,function(x){return(length(res[res==v[x]]))})
#     names(res)=mc$DIM.NAMES.NCD
#   }
#   if(var=="hiv.ncd"){
#     v=c(1: (mc$DIM.HIV*mc$DIM.NCD))
#     res<-unlist(lapply(pop,function(x) {return (c(x$hivState*x$ncdState))}))
#     res=lapply(v,function(x){return(length(res[res==v[x]]))})
#     names(res)=paste(unlist(lapply(mc$DIM.NAMES.HIV,rep,mc$DIM.NCD)) ,"/",mc$DIM.NAMES.NCD)
#   }
#   ##
#   if(type=="freq") {return (unlist(res))}
#   if(type=="prop") {return (unlist(res)/sum(unlist(res)) )}
# }


# Transform 1D data on HIV state sizes (hiv.pop) to proportion of people in different HIV states by age/sex
print("get.hiv.state.proportions")
get.hiv.state.proportions = function(hiv.pop){
  #transform 1D data to correct array dimensions
  ages = mc$DIM.NAMES.AGE
  sexes = mc$DIM.NAMES.SEX
  hiv.status = mc$DIM.NAMES.HIV
  
  hiv.dim.names = list(hiv.status = hiv.status,
                       age = ages,
                       sex = sexes)
  
  #compute proportions of HIV states in each age/sex subgroup
  hiv.probs = 
    sapply(1:length(hiv.dim.names$sex), function(sex){
      sapply(1:length(hiv.dim.names$age), function(age){
        hiv.pop[,age,sex]/sum(hiv.pop[,age,sex])
      })
    })
  
  dim(hiv.probs) = sapply(hiv.dim.names,length)
  dimnames(hiv.probs) = hiv.dim.names
  
  hiv.probs
  
}


#'@MS: to discuss:How to transform annual probability (AP) to monthly probability (MP):
#'This is applicable to prob of first CVD event (10-year to annual, and annual to monthly)
#case 1- if the first and subsequent events are independant: MP=AP/12
#case 2-if the probability indicate the chance to the first event occurance: then it follows a geometric distribution where prob(x)=(1-p)^(x-1)*p
# the cumulative prob for geom dist is 1-(1-p)^x
# AP= 1-(1-MP)^12 >> MP= 1-(1-AP)^(1/12)
return.monthly.prob<-function(annual.prob=1){
  return(1-(1-annual.prob)^(1/12))
}
AP=.5 
AP/12
return.monthly.prob(annual.prob = AP)
