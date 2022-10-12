#
#  R HIVNCD 2022
#  R Core functions
#  
#####################################
print("Reading R Core Functions... ")

#creates the initial population
create.initial.population = function(n=0){
  cat("Generating Population in tnow=",mc$TNOW,"\n")
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
  
  pop = (mapply(Person$new, vIds,vSexes,vAges,mc$TNOW,vncdState))
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

# model one simulated year
model.annual.dynamics<-function(pop){
  #check the TNOW and break if it's not correctly set
  if ((mc$TNOW%%mc$ANNUAL.TIMESTEPS)!=0) break("TNOW is not set correctly")
  
  #Add one year
  mc$YNOW=mc$YNOW+1
  #Aging
  invisible(lapply(pop,function(x){x$incAge}))
  barplot(cReturnAgDist(pop),names.arg = mc$DIM.NAME.AGEGROUP,main=paste("Age distribution tnow=",mc$TNOW))
  
  
  
  #model each timestep within the year
  for(i in (1:mc$ANNUAL.TIMESTEPS)){
  mc$TNOW=mc$TNOW+1  
    
  }
  
  #DEATHS 
  #Aging out:
  #' @MS: Here is how I am modeling a specific event for all pop members and return 1 if that event happens to be able to count instances 
  n=invisible(lapply(pop,function(x){if(x$age>85){x$bMarkedDead=T; return (1)}}))
  sum(unlist(n))
  
  
  #Kill those dead
  n=invisible(lapply(pop,function(x){if(x$bMarkedDeat==T){
    XXX #'@JP: how do we kill agents?
    return (1)}}))
  
  #BIRTHS
  #compute number of newborns needed (assuming a fix pop size for now)
  n=mc$POP.size-length(pop)
  vIds = c((mc$lastID+1): (mc$lastID+n))
  mc$lastID=mc$lastID+n
  vSexes = sample(c(mc$MALE,mc$FEMALE),n,prob = c(.5,.5),replace = T)
  pop1 = (mapply(Person$new, vIds,vSexes,0,mc$TNOW,mc$NCD.NEG)) #'@JP: do we need to delete pop1 and open memory?
  pop<-rbind(pop,pop1)
  
  }




install.packages("pryr")
a=pop1[1]
pryr::address(a)

rm(a);gc()
pryr::address(pop[1])
# bool modelBirths(mt19937 &rng);
# bool modelHivDynamics(mt19937 &rng);
# bool modelNcdDynamics(mt19937 &rng);
# bool modelDeathsAging(mt19937 &rng);
# bool modelArtCoverage(mt19937 &rng);
# bool removeDeaths();
# bool modelIntervention(mt19937 &rng);

