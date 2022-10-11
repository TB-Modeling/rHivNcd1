#
#  R HIVNCD 2022
#  R Core functions
#  
#####################################
print("Reading R Core Functions... ")
# always pass time via the global parameter TICK

#creates the initial population
create.initial.population = function(TICK,
                                     n=0){
  cat("Generating Population in",TICK,"\n")
  sim.pop = read.csv("data/stepSimPop2015.csv")
  
  sim.pop$age[sim.pop$age==0] = 0.5 #we need this so that the first agegroups is set to 1
  sim.pop$age[sim.pop$age>85] = 85 
  sim.pop$sex[sim.pop$sex=="male"] = mc$MALE #'@MS: I am trying to use these variables throughout
  sim.pop$sex[sim.pop$sex=="female"] = mc$FEMALE
  sim.pop$ncd[sim.pop$hypertension==0 & sim.pop$diabetes==0] = mc$NCD.NEG #0, neither 
  sim.pop$ncd[sim.pop$hypertension==0 & sim.pop$diabetes==1] = mc$NCD.DIAB #1, diabetic
  sim.pop$ncd[sim.pop$hypertension==1 & sim.pop$diabetes==0] = mc$NCD.HYP #2, hypertensive
  sim.pop$ncd[sim.pop$hypertension==1 & sim.pop$diabetes==1] = mc$NCD.DIAB_HYP #3, both
  
  #subset the first n row to create n persons
  if (n>0) sim.pop=sim.pop[1:n,]
  
  vIds = sim.pop$X
  vAges = sim.pop$age
  vSexes = as.numeric(sim.pop$sex)
  vncdState = sim.pop$ncd
  
  pop = (mapply(Person$new, vIds,vSexes,vAges,TICK,vncdState))
  pop = unlist(pop)
  
  cat(length(vIds),"persons generated.","\n")
  pop
}

#'@MS: I revised this to be a more generalizable function that can be used for each timestep as well
#'@MS: I defined these dimensions in the globalvariables so that they're accessible throughout the code
# Transform 1D data on HIV state sizes (hiv.pop) to proportion of people in different HIV states by age/sex
get.hiv.probabilities = function(hiv.pop){
  #transform 1D data to correct array dimensions
  ages = mc$DIM.NAME.AGEGROUP
  sexes = mc$DIM.NAMES.SEX
  hiv.status = mc$DIM.NAMES.HIV

 hiv.dim.names.1 = list(age = ages,
                         sex = sexes,
                         hiv.status = hiv.status)
  hiv.pop = array(hiv.pop[,2],
                  dim = sapply(hiv.dim.names.1,length),
                  dimnames = hiv.dim.names.1)
  #' @MS: how can we switch array dimensions, for example to hiv.status, age and sex?
  
  #compute proportions of HIV states in each age/sex subgroup
  hiv.probs = 
    sapply(1:length(hiv.dim.names.1$age), function(age){
      sapply(1:length(hiv.dim.names.1$sex), function(sex){
        hiv.pop[age,sex,]/sum(hiv.pop[age,sex,])
      })
    })
  #reorder the dimensions
  hiv.dim.names.2 = list(hiv.status = hiv.status,
                         sex = sexes,
                         age = ages
  )
  dim(hiv.probs) = sapply(hiv.dim.names.2,length)
  dimnames(hiv.probs) = hiv.dim.names.2
  
  hiv.probs

  }


# model initial HIV status 
set.initial.hiv.status = function(personID #id of a selected population member
){
  p<-pop[[personID]]
  
  #read 1D data from hiv outputs
  hiv.pop = read.csv("data/hivPrev2015.csv")
  hiv.probs = get.hiv.probabilities(hiv.pop)
  probs = hiv.probs[,p$sex,p$agegroup]
  
  #'@MS: add a sanity check here to break the code if this doesnt hold
  # sum(specific.hiv.probs) == 1
  
  rand.hiv.state = sample(x = c(1:length(probs)), size = 1, prob = probs)
  p$hivState=rand.hiv.state
}


