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
  sexes = c("female","male")
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

hiv.probs = get.hiv.probabilities()

#model initial HIV status 
set.initial.hiv.status.MS = function(personID, #id of a selected population member
                                      TICK, #current time (should pass via the global parameter)
                                     hiv.probs
){
  p<-pop[[personID]]
  
  #empty p object for purpose of writing - NEED SOME HELP WITH THIS 
  p = NULL
  p$sex = "female"
  p$agegroup = "10-14"
  
  hiv.states = unlist(dimnames(hiv.probs)[1])
  
  specific.hiv.probs = hiv.probs[,p$sex,p$agegroup]
  hiv.status = sample(x = hiv.states, size = 1, prob = specific.hiv.probs)
  
  # THESE DON'T QUITE MATCH WHAT'S IN THE GLOBAL VARIABLES
  if(hiv.status=="hiv_negative")
    p$hivState=0
  if(hiv.status=="undiagnosed")
    p$hivState=1
  if(hiv.status=="diagnosed_unengaged")
    p$hivState=2
  if(hiv.status=="engaged_unsuppressed" | hiv.status=="engaged_suppressed")
    p$hivState=3
}



create.initial.population.MS = function(TICK){
  sim.pop = read.csv("data/simpop.csv")
  sim.pop$ncd[sim.pop$hypertension==0 & sim.pop$diabetes==0] = 0 # neither
  sim.pop$ncd[sim.pop$hypertension==0 & sim.pop$diabetes==1] = 1 # diabetic
  sim.pop$ncd[sim.pop$hypertension==1 & sim.pop$diabetes==0] = 2 # hypertensive
  sim.pop$ncd[sim.pop$hypertension==1 & sim.pop$diabetes==1] = 3 # both

  vIds = sim.pop$X
  vAges = sim.pop$age
  vSexes = sim.pop$sex
  vncdState = sim.pop$ncd

  pop = (mapply(Person$new, vIds,vSexes,vAges,TICK,vncdState))
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