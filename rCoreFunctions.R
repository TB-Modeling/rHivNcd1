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
  sim.pop$sex[sim.pop$sex=="male"] = mc$MALE 
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

# load hiv sim workspace with: (1) sim object, (2) data manager, (3) extracted data (hiv.output.for.ncd)
load('hiv_sim.RData')

# distribution of HIV states for each age/sex category
hivPrev2015 = hiv.output.for.ncd$population["2015",,,]


# Transform 1D data on HIV state sizes (hiv.pop) to proportion of people in different HIV states by age/sex
get.hiv.probabilities = function(hiv.pop){
  #transform 1D data to correct array dimensions
  ages = mc$DIM.NAME.AGEGROUP
  sexes = mc$DIM.NAMES.SEX
  hiv.status = mc$DIM.NAMES.HIV
  
  hiv.dim.names = list(hiv.status = hiv.status,
                       age = ages,
                       sex = sexes)
  
  #compute proportions of HIV states in each age/sex subgroup
  hiv.probs = 
    sapply(1:length(hiv.dim.names$age), function(age){
      sapply(1:length(hiv.dim.names$sex), function(sex){
        hiv.pop[,age,sex]/sum(hiv.pop[,age,sex])
      })
    })

  dim(hiv.probs) = sapply(hiv.dim.names,length)
  dimnames(hiv.probs) = hiv.dim.names
  
  hiv.probs
  
}

# model initial HIV status 
set.initial.hiv.status = function(personID #id of a selected population member
){
  
  p<-pop[[personID]]
  
  #read 1D data from hiv outputs
  hiv.pop = hivPrev2015
  hiv.probs = get.hiv.probabilities(hiv.pop)
  p.probs = hiv.probs[,p$agegroup,p$sex]

  # break to ensure that sum of p.probs==1
  if(round(sum(p.probs),0)!=1){
    stop(paste0("Error: p.probs for: age ",dimnames(hiv.probs)[[2]][p$agegroup],
                ", sex ",dimnames(hiv.probs)[[3]][p$sex], " does not sum to 1"))
  }

  rand.hiv.state = sample(x = c(1:length(p.probs)), size = 1, prob = p.probs)
  p$hivState=rand.hiv.state
}
################################################################
# model one simulated year
model.annual.dynamics<-function(sim){
  pop=sim$pop
  mc=sim$mc
  gss=sim$gss
  
  #check the TNOW and break if it's not correctly set
  if ((mc$TNOW%%mc$ANNUAL.TIMESTEPS)!=0) break("TNOW is not set correctly")
  
  #Add one year
  mc$YNOW<-mc$YNOW+1
  cat("It's now year =",mc$YNOW,"\n")
  
  #Aging
  invisible(lapply(pop,function(x){x$incAge}))
  barplot(cReturnAgDist(pop),names.arg = mc$DIM.NAME.AGEGROUP,main=paste("Age distribution tnow=",mc$TNOW))
  
  
  ################################################################
  # MODLING TIMESTEP DYNAMICS
  for(i in (1:mc$ANNUAL.TIMESTEPS)){
  mc$TNOW=mc$TNOW+1
  
  ### MODEL HIV DYNAMICS
  # calculate current HIV state sizes by age/sex to use below:
  n=length(pop)
  hiv.state.sizes<-array(0,dim=c(mc$NUM.SEXES,mc$NUM.AGE.GROUPS,mc$NUM.HIV.STATES),
                  dimnames = list(mc$DIM.NAMES.SEX,mc$DIM.NAME.AGEGROUP,mc$DIM.NAMES.HIV))
  invisible(lapply(c(1:n),function(x){
    hiv.state.sizes[ pop[[x]]$sex, pop[[x]]$agegroup, pop[[x]]$hivState]<<-hiv.state.sizes[ pop[[x]]$sex, pop[[x]]$agegroup, pop[[x]]$hivState]+1
  }))
  
  #'@MS: I completed the incidence as an example below. you can complete the other sections based on that
  #'@PK: I filled in the rest below
  #1- ENGAGEMENT
  {
    n.hiv.uneng<-hiv.state.sizes[,,"HIV.UNENG"]
    # projected engagement by the hiv model
    target.eng = hiv.output.for.ncd$engagement[as.character(mc$INITIAL.YEAR+mc$YNOW),,] # pull out current year; dimensions are year, age, sex
    target.eng = aperm(target.eng, c(2,1)) # reorders dimensions to be sex, age
    dimnames(target.eng) = list(mc$DIM.NAMES.SEX,mc$DIM.NAME.AGEGROUP)
    
    # calculate the probability of engagement for each subgroup
    prob.eng=target.eng/n.hiv.uneng
    prob.eng[prob.eng==Inf]<-0
    # model random events based on probabilities
    lapply(c(1:n),function(x){
      p=pop[[x]] 
      p.prob= prob.eng[p$sex, p$agegroup]
      if (runif(1)<p.prob){
        p$bMarkedHivEng=T
      }
    })
    }
  #2- DISENGAGEMENT
  {
    n.hiv.eng<-hiv.state.sizes[,,"HIV.ENG"]
    # projected disengagement by the hiv model
    target.diseng = hiv.output.for.ncd$disengagement[as.character(mc$INITIAL.YEAR+mc$YNOW),,] # pull out current year; dimensions are year, age, sex
    target.diseng = aperm(target.diseng, c(2,1)) # reorders dimensions to be sex, age
    dimnames(target.diseng) = list(mc$DIM.NAMES.SEX,mc$DIM.NAME.AGEGROUP)
    
    # calculate the probability of disengagement for each subgroup
    prob.diseng=target.diseng/n.hiv.eng
    prob.diseng[prob.diseng==Inf]<-0
    # model random events based on probabilities
    lapply(c(1:n),function(x){
      p=pop[[x]] 
      p.prob= prob.diseng[p$sex, p$agegroup]
      if (runif(1)<p.prob){
        p$bMarkedHivUneng=T
      }
    })
  }
  #3- DIAGNOSIS
  {
    n.hiv.undiag<-hiv.state.sizes[,,"HIV.UNDIAG"]
    # projected diagnosis by the hiv model
    target.diag = hiv.output.for.ncd$diagnosis[as.character(mc$INITIAL.YEAR+mc$YNOW),,] # pull out current year; dimensions are year, age, sex
    target.diag = aperm(target.diag, c(2,1)) # reorders dimensions to be sex, age
    dimnames(target.diag) = list(mc$DIM.NAMES.SEX,mc$DIM.NAME.AGEGROUP)
    
    # calculate the probability of diagnosis for each subgroup
    prob.diag=target.diag/n.hiv.undiag
    prob.diag[prob.diag==Inf]<-0
    # model random events based on probabilities
    lapply(c(1:n),function(x){
      p=pop[[x]] 
      p.prob= prob.diag[p$sex, p$agegroup]
      if (runif(1)<p.prob){
        p$bMarkedHivDiag=T
      }
    })
    }
  #' @MS: make sure this one works/makes sense before repeating for other transitions 
  #4- INCIDENCE
  {#number of HIV negative persons eligible for new incidence
  n.hiv.neg<-hiv.state.sizes[,,"HIV.NEG"] #'@MS: this should come from the HIV model, not this simulated population - fix for all above as well 
  n.hiv.neg = hiv.output.for.ncd$population[as.character(mc$INITIAL.YEAR+mc$YNOW),,,"hiv_negative"]
  # projected incidence by the hiv model
  target.inc = hiv.output.for.ncd$incidence[as.character(mc$INITIAL.YEAR+mc$YNOW),,] # pull out current year; dimensions are year, age, sex
  target.inc = aperm(target.inc, c(2,1)) # reorders dimensions to be sex, age
  dimnames(target.inc) = list(mc$DIM.NAMES.SEX,mc$DIM.NAME.AGEGROUP)

  # calculate the probability of incidence for each subgroup
  prob.inc=target.inc/n.hiv.neg
  prob.inc[prob.inc==Inf]<-0
  # model random events based on probabilities
  lapply(c(1:n),function(x){
    p=pop[[x]] 
    p.prob= prob.inc[p$sex, p$agegroup]
    if (runif(1)<p.prob){
      p$bMarkedHivInc=T
    }
  })
  }
  ### model hiv events that are marked:
  #'@MS:  if you want to keep track of these by sex/age, you can replace them with 2D arrays,
  #' and create a corresponding array in gss to save it there at the end
  n.inc=0
  n.diag=0
  n.eng=0
  n.uneng=0
  lapply(c(1:n),function(x){
    p=pop[[x]]
    #sanity check
    if (p$bMarkedHivInc+p$bMarkedHivDiag+p$bMarkedHivUneng+p$bMarkedHivEng >1) break("can not model more than one hiv transition")
    #
    if(p$bMarkedHivInc) {
      p$hiv.getInfected(mc$TNOW)
      n.inc=n.inc+1
    }
    if(p$bMarkedHivDiag) {
      p$hiv.getDiagnosed(mc$TNOW)
      n.diag=n.diag+1
    }
    if(p$bMarkedHivUneng) {
      p$hiv.getUnengage(mc$TNOW)
      n.uneng=n.uneng+1
    }
    if(p$bMarkedHivEng) {
      p$hiv.getEngaged(mc$TNOW)
      n.eng=n.eng+1
    }
  })
  
  
  ### MODEL NCD DYNAMICS
  
  ### MODEL CVD DYNAMICS
  
  ### MODEL DEATHS

  }
  ################################################################
  ### DEATHS 
  #Aging out:
  n.ageout=sum(unlist(invisible(lapply(pop,function(x){
    if(x$age>mc$MAX.AGE) {
      x$bMarkedDead=T;
      return(1)
    }}))))
  cat(n.ageout," persons are aging out","\n")
  
  #Kill those dead
  npop<-length(pop)
  death.status=unlist(invisible(lapply(c(1:npop),function(x){
    return(pop[[x]]$bMarkedDead)
  })))
  n.deaths<-sum(death.status)
  gss$n.deaths[mc$YNOW]=n.deaths
  cat(n.deaths," persons are marked dead","\n")
  #only keep those who are alive
  pop<-pop[!death.status] #'@JP: should we delete these dead people?
  
  cat("current pop size is ",length(pop),"\n")
  ################################################################
  ### BIRTHS
  #compute number of newborns needed (assuming a fix pop size for now)
  n.births=mc$POP.SIZE-length(pop)
  if(n.births>0){
    cat(n.births," newborns are added","\n")
    vIds = c((mc$lastID+1): (mc$lastID+n.births))
    mc$lastID=mc$lastID+n.births
    vSexes = sample(c(mc$MALE,mc$FEMALE),n.births,prob = c(.5,.5),replace = T)
    pop1 = (mapply(Person$new, vIds,vSexes,0,mc$TNOW,mc$NCD.NEG)) #'@JP: do we need to delete pop1 and open memory?
    pop<-c(pop,pop1)
  #
    gss$n.births[mc$YNOW]=n.births
    }
  
  #
  gss$pop.size[mc$YNOW]=length(pop)
  
  
  
  cat("Final pop size is ",length(pop),"\n")
  
  return(list(pop=pop,
              mc=mc,
              gss=gss))
}




# bool modelHivDynamics(mt19937 &rng);
# bool modelNcdDynamics(mt19937 &rng);
# bool modelDeathsAging(mt19937 &rng);
# bool modelArtCoverage(mt19937 &rng);
# bool removeDeaths();
# bool modelIntervention(mt19937 &rng);

