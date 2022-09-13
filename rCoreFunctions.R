#
#  R HIVNCD 2022
#  R Core functions
#  
#####################################
print("Reading R Core Functions... ")
# always pass time via the global parameter TICK



create.initial.population.MS = function(TICK){
  sim.pop = read.csv("~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/ABM/ncd_analysis/simpop.csv")
  vIds = sim.pop$X
  vAges = sim.pop$age
  vSexes = sim.pop$sex

  pop = (mapply(Person$new, vIds,vSexes,vAges,TICK))
  pop = unlist(pop)

  pop
}

#creates the initial population
create.initial.population<-function(TICK){
pop<-lapply(c(1:DIM.SEX),function(i){
  lapply(c(1:DIM.AGE),function(j){
    #sex=i-1 ; agegroup=j    
    n=mat.initial.pop[i,j] #number of persons in this sex/agegroup
    vIds<-seq(lastID+1,lastID+n); lastID<<-lastID+n
    vAges<-floor(runif(n,(j-1)*5,j*5))
    #
    return(mapply(Person$new, vIds,i,vAges,TICK))
  })
})
pop<-unlist(pop)
return(pop)
}

#model initial HIV status
set.initiatl.hiv.status<-function(personID, #id of a selected population member
                                  TICK #current time (should pass via the global parameter)
){
  p<-pop[[personID]]
  r<-runif(1)
  mat<-cumsum(mat.hiv.initial.prevalence.ratio[p$sex,p$agegroup,])
  #
  # print(personID)
  if (r< mat[mc$HIV.NEG+1]) {
    p$hivState<-0
  }else{
    if (r<mat[mc$HIV.UNDIAG+1]){
      p$hivState=mc$HIV.UNDIAG
      p$tHivinc=TICK
    }else{
      if(r<mat[mc$HIV.DIAG_UNSUPP+1]){
        p$hivState=mc$HIV.DIAG_UNSUPP
        p$tHivinc=TICK
        p$tHivdiag=TICK
      }else{
        p$hivState=mc$HIV.SUPP
        p$tHivinc=TICK
        p$tHivdiag=TICK
        p$tHivsupp=TICK
      }}}
}

#model initial HIV status
set.initiatl.ncd.status<-function(personID,
                                  TICK 
){
  p<-pop[[personID]]
  r<-runif(1)
  mat<-cumsum(mat.ncd.initial.prevalence.ratio[p$sex,p$agegroup,])
  #
  # print(personID)
  if (r< mat[mc$NCD.NEG+1]) {
    p$ncdState<-0
  }else{
    if (r<mat[mc$NCD.DIAB+1]){
      p$ncdState=mc$NCD.DIAB
      p$tDiabinc=TICK
    }else{
      if(r<mat[mc$NCD.HYP+1]){
        p$ncdState=mc$NCD.HYP
        p$tHypinc=TICK
      }else{
        p$ncdState=mc$NCD.DIAB_HYP
        p$tHypinc=TICK
        p$tDiabinc=TICK
      }}}
}


computeCvdRisk<-function(personID){
  
}