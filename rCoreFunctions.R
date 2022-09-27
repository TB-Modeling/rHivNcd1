#
#  R HIVNCD 2022
#  R Core functions
#  
#####################################
print("Reading R Core Functions... ")
# always pass time via the global parameter TICK

# Get HIV status probabilities 
get.hiv.probabilities = function(){
  hiv.pop = read.csv("data/hivpop.csv")
  ages = c("0-4","5-9","10-14","15-19", "20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59",
           "60-64","65-69","70-74","75-79","80 and over")
  sexes = c("FEMALE","MALE")
  hiv.status = c("hiv_negative","undiagnosed","diagnosed_unengaged","engaged_unsuppressed","engaged_suppressed")
  hiv.dim.names.1 = list(age = ages,
                         sex = sexes,
                         hiv.status = hiv.status)
  hiv.pop = array(hiv.pop[,2],
                  dim = sapply(hiv.dim.names.1,length),
                  dimnames = hiv.dim.names.1)
  
  hiv.probs = sapply(1:length(hiv.dim.names.1$age), function(age){
    sapply(1:length(hiv.dim.names.1$sex), function(sex){
      hiv.pop[age,sex,]/sum(hiv.pop[age,sex,])
    })
  })
  
  hiv.dim.names.2 = list(hiv.status = hiv.status,
                         sex = sexes,
                         age = ages
                         
)
  dim(hiv.probs) = sapply(hiv.dim.names.2,length)
  dimnames(hiv.probs) = hiv.dim.names.2

  hiv.probs
}

# model initial HIV status 
set.initial.hiv.status = function(personID, #id of a selected population member
                                  TICK #current time (should pass via the global parameter)
){
  p<-pop[[personID]]
  
  hiv.probs = get.hiv.probabilities()
  hiv.states = unlist(dimnames(hiv.probs)[1])
  specific.hiv.probs = hiv.probs[,p$sex,p$agegroup]
  
  hiv.status = sample(x = hiv.states, size = 1, prob = specific.hiv.probs)
  
  if(hiv.status=="hiv_negative")
    p$hivState=0
  if(hiv.status=="undiagnosed"){
    p$hivState=1
    p$tHivInc=TICK
  }
  if(hiv.status=="diagnosed_unengaged"){
    p$hivState=2
    p$tHivInc=TICK
    tHivDiag=TICK
  }
  if(hiv.status=="engaged_unsuppressed" | hiv.status=="engaged_suppressed"){
    p$hivState=3
    p$tHivInc=TICK
    p$tHivDiag=TICK
    p$tHivEng=TICK # old version did tHivSupp instead
  }

}


#creates the initial population
create.initial.population = function(TICK){
  sim.pop = read.csv("data/simpop.csv")
  sim.pop$age[sim.pop$age==0] = 0.5
  sim.pop$age[sim.pop$age>85] = 85 # TEMPORARY FIX FOR NOW TO GET AGE GROUPS TO WORK 
  sim.pop$ncd[sim.pop$hypertension==0 & sim.pop$diabetes==0] = mc$NCD.NEG #0, neither 
  sim.pop$ncd[sim.pop$hypertension==0 & sim.pop$diabetes==1] = mc$NCD.DIAB #1, diabetic
  sim.pop$ncd[sim.pop$hypertension==1 & sim.pop$diabetes==0] = mc$NCD.HYP #2, hypertensive
  sim.pop$ncd[sim.pop$hypertension==1 & sim.pop$diabetes==1] = mc$NCD.DIAB_HYP #3, both

  sim.pop$tDiabinc=NULL
  sim.pop$tDiabInc[sim.pop$ncd==1 | sim.pop$ncd==3] = TICK
  
  sim.pop$tHypInc=NULL
  sim.pop$tHypInc[sim.pop$ncd==3 | sim.pop$ncd==3] = TICK
  
  vIds = sim.pop$X
  vAges = sim.pop$age
  vSexes = sim.pop$sex
  vncdState = sim.pop$ncd
  vtDiabInc = sim.pop$tDiabInc
  vtHypInc = sim.pop$tHypInc

  pop = (mapply(Person$new, vIds,vSexes,vAges,TICK,vncdState,vtDiabInc,vtHypInc))
  pop = unlist(pop)

  pop
}

## OLD VERSIONS BELOW - I THINK THESE CAN BE REMOVED ## 

## MS - I think I can remove this one now; but check other places for Supp (should now be Eng)
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


## MS - I think I can remove this one now that I've added incidence times to create initial 
#model initial NCD status
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