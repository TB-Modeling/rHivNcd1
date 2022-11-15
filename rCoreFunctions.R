#
#  R HIVNCD 2022
#  R Core functions
#  
#####################################
print("Reading R Core Functions... ")

#creates the initial population
print("create.initial.population")
create.initial.population = function(n=0){
  cat("Generating Population in year=",mc$CYNOW,"\n")
  sim.pop = read.csv("data/stepSimPop2015.csv")
  
  sim.pop$age[sim.pop$age==0] = 0.5 #we need this so that the first agegroups is set to 1
  sim.pop$age[sim.pop$age>85] = 85 
  sim.pop$sex[sim.pop$sex=="MALE"] = mc$MALE 
  sim.pop$sex[sim.pop$sex=="FEMALE"] = mc$FEMALE
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

# Transform 1D data on HIV state sizes (hiv.pop) to proportion of people in different HIV states by age/sex
print("get.hiv.probabilities")
get.hiv.probabilities = function(hiv.pop){
  #transform 1D data to correct array dimensions
  ages = mc$DIM.NAMES.AGE
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
print("set.initial.hiv.status")
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

# model one simulated year
cat("model.annual.dynamics")
model.annual.dynamics<-function(sim){
  pop=sim$pop
  mc=sim$mc
  gss=sim$gss
  
  #check the TNOW and break if it's not correctly set
  if ((mc$TNOW%%mc$ANNUAL.TIMESTEPS)!=0) break("TNOW is not set correctly")
  cat("Beginning the year ... ",mc$CYNOW,"\n")
  
  {
    ##Probability of engagement--------
    prob.eng=target.probabilities$prob.eng[as.character(mc$CYNOW),,]
    if(sum(prob.eng>1)>1)     
      stop(paste("Error: probability of engagement >1 in year ",mc$CYNOW))
    prob.eng[prob.eng==Inf]<-0
    prob.eng<-prob.eng/mc$ANNUAL.TIMESTEPS
    
    ##Probability of disengagement--------
    prob.diseng=target.probabilities$prob.diseng[as.character(mc$CYNOW),,]
    if(sum(prob.diseng>1)>1)     
      stop(paste("Error: probability of disengagement >1 in year ",mc$CYNOW))
    prob.diseng[prob.diseng==Inf]<-0
    prob.diseng<-prob.diseng/mc$ANNUAL.TIMESTEPS
    
    ##Probability of diagnosis--------
    prob.diag = target.probabilities$prob.diag[as.character(mc$CYNOW),,]
    if(sum(prob.diag>1)>1)     
      stop(paste("Error: probability of prob.diag >1 in year ",mc$CYNOW))
    prob.diag[prob.diag==Inf]<-0
    prob.diag<-prob.diag/mc$ANNUAL.TIMESTEPS
    
    ##Probability of incidence--------
    n.hiv.neg = hiv.output.for.ncd$population[as.character(mc$CYNOW-1),"hiv_negative",,]
    target.inc = hiv.output.for.ncd$incidence[as.character(mc$CYNOW),,] # pull out current year; dimensions are year, age, sex
    prob.inc=target.inc/n.hiv.neg
    if(sum(prob.inc>1)>1)     stop(paste("Error: probability of prob.inc >1 in year ",mc$CYNOW))
    prob.inc[prob.inc==Inf]<-0
    prob.inc<-prob.inc/mc$ANNUAL.TIMESTEPS
  }
  #----------------
  # MODLING TIMESTEP DYNAMICS -----
  for(i in (1:mc$ANNUAL.TIMESTEPS)){
    mc$TNOW=mc$TNOW+1
    n=length(pop)
    # Evaluate prob of events ----
    lapply(c(1:n),function(x){
      p=pop[[x]] 
      
      if (p$hivState == mc$HIV.UNENG) {
        p.prob= prob.eng[p$agegroup,p$sex]
        if (runif(1) < p.prob)
          p$bMarkedHivEng=T
      }else{
        #2- DISENGAGEMENT
        if (p$hivState== mc$HIV.ENG) {
          p.prob= prob.diseng[p$agegroup,p$sex]
          if (runif(1)<p.prob)
            p$bMarkedHivUneng=T
        }else{
          #3- DIAGNOSIS
          if (p$hivState== mc$HIV.UNDIAG) {
            p.prob= prob.diag[p$agegroup,p$sex]
            if (runif(1)<p.prob)
              p$bMarkedHivDiag=T
          }else{
            #4- INCIDENCE
            if (p$hivState== mc$HIV.NEG) {
              p.prob= prob.inc[p$agegroup,p$sex]
              if (runif(1)<p.prob)
                p$bMarkedHivInc=T
            }else         { #'@MS: if we arrive here, there is a problem
              browser()
              stop(paste("Error: Person ",x," hivState is ",p$hivState," and it didnt meet anuy criteria"))
            }}}}
    })
    print(paste("Timestep: ",mc$TNOW," ---"))
    # Model marked events and record statistics ----
    n.inc=0
    n.diag=0
    n.eng=0
    n.uneng=0
    lapply(c(1:n),function(x){
      p=pop[[x]] 
      if (p$bMarkedHivInc+p$bMarkedHivDiag+p$bMarkedHivUneng+p$bMarkedHivEng >1){
        browser()
        stop(paste("can not model more than one hiv transition for person ",x," at year",mc$YNOW," time ",mc$TNOW))
      }
      #
      if(p$bMarkedHivInc==TRUE) {
        # browser()
        p$hiv.getInfected(mc$TNOW)
        gss$n.hiv.inc[p$agegroup,p$sex,as.character(mc$CYNOW)] = gss$n.hiv.inc[p$agegroup,p$sex,as.character(mc$CYNOW)]+1
        n.inc<<-n.inc+1
      }
      if(p$bMarkedHivDiag==TRUE) {
        # browser()
        p$hiv.getDiagnosed(mc$TNOW)
        gss$n.hiv.diag[p$agegroup,p$sex,as.character(mc$CYNOW)] = gss$n.hiv.diag[p$agegroup,p$sex,as.character(mc$CYNOW)]+1
        n.diag<<-n.diag+1
      }
      if(p$bMarkedHivUneng==TRUE) {
        # browser()
        p$hiv.getUnengage(mc$TNOW)
        gss$n.hiv.uneng[p$agegroup,p$sex,as.character(mc$CYNOW)] = gss$n.hiv.uneng[p$agegroup,p$sex,as.character(mc$CYNOW)]+1
        n.eng<<-n.eng+1
      }
      if(p$bMarkedHivEng==TRUE) {
        # browser()
        p$hiv.getEngaged(mc$TNOW)
        gss$n.hiv.eng[p$agegroup,p$sex,as.character(mc$CYNOW)] = gss$n.hiv.eng[p$agegroup,p$sex,as.character(mc$CYNOW)]+1
        n.uneng<<-n.uneng+1
      }
    })
    print(paste("Total incidence= ",n.inc," diag= ",n.diag," eng= ",n.eng," uneng= ",n.uneng))
    
  }
  ################################################################
  # MODEL AGING --------
  # barplot(cReturnAgDist(pop),names.arg = mc$DIM.NAMES.AGE,main=paste("Age distribution year=",mc$CYNOW))
  # print(cReturnAgDist(pop))
  # browser()
  invisible(lapply(pop,function(x){x$incAge}))
  
  ## MODEL DEATHS --------
  ### Aging out:
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
  ##
  ## MODEL BIRTHS --------
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
  
  # Record annual statatistics --------
  gss$pop.size[mc$YNOW]=length(pop)
  gss$n.hiv.prev[,,,as.character(mc$CYNOW)]= return.gss.hiv.state.sizes(pop)
  
  # END OF YEAR----
  #'@MS: for now, we let the TNOW increase over time and use it to record the event times for agents
  mc$YNOW<-mc$YNOW+1
  mc$CYNOW<-mc$CYNOW+1
  
  cat("Final pop size is ",length(pop),"\n")
  print(paste("End of year: ",mc$CYNOW," ---------"))
  
  # barplot(cReturnAgDist(pop),names.arg = mc$DIM.NAMES.AGE,main=paste("Age distribution year=",mc$CYNOW))
  # print(cReturnAgDist(pop))
  # browser()
  invisible(list(pop=pop,
                 mc=mc,
                 gss=gss))
}




# bool modelHivDynamics(mt19937 &rng);
# bool modelNcdDynamics(mt19937 &rng);
# bool modelDeathsAging(mt19937 &rng);
# bool modelArtCoverage(mt19937 &rng);
# bool removeDeaths();
# bool modelIntervention(mt19937 &rng);
