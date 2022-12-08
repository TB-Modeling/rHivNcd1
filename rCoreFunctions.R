#
#  R HIVNCD 2022
#  rCoreFunctions.R class
#  
#####################################
print("Sourcing rCoreFunctions.R ... ")


# a general function to return distributions
print("return.pop.distribution")
return.pop.distribution<-function(var, # options: sex, agegroup, hiv, ncd
                                  type="freq" #options: freq (default), prop 
){
  
  if(var=="sex"){
    v=c(1:mc$DIM.SEX)
    res<-unlist(lapply(pop,function(x) {return (x$sex)}))
    res=lapply(v,function(x){return(length(res[res==v[x]]))})
    names(res)=mc$DIM.NAMES.SEX
  }
  if(var=="agegroup"){
    v=c(1:mc$DIM.AGE)
    res<-unlist(lapply(pop,function(x) {return (x$agegroup)}))
    res=lapply(v,function(x){return(length(res[res==v[x]]))})
    names(res)=mc$DIM.NAMES.AGE
  }
  if(var=="hiv"){
    v=c(1:mc$DIM.HIV)
    res<-unlist(lapply(pop,function(x) {return (x$hivState)}))
    res=lapply(v,function(x){return(length(res[res==v[x]]))})
    names(res)=mc$DIM.NAMES.HIV
  }
  if(var=="ncd"){
    v=c(1:mc$DIM.NCD)
    res<-unlist(lapply(pop,function(x) {return (x$ncdState)}))
    res=lapply(v,function(x){return(length(res[res==v[x]]))})
    names(res)=mc$DIM.NAMES.NCD
  }
  if(var=="hiv.ncd"){
    v=c(1: (mc$DIM.HIV*mc$DIM.NCD))
    res<-unlist(lapply(pop,function(x) {return (c(x$hivState*x$ncdState))}))
    res=lapply(v,function(x){return(length(res[res==v[x]]))})
    names(res)=paste(unlist(lapply(mc$DIM.NAMES.HIV,rep,mc$DIM.NCD)) ,"/",mc$DIM.NAMES.NCD)
  }
  ##
  if(type=="freq") {return (unlist(res))}
  if(type=="prop") {return (unlist(res)/sum(unlist(res)) )}
}


#creates the initial population
print("create.initial.population")
create.initial.population <- function( n=0){
  cat("Generating Population in year=",mc$CYNOW,"\n")
  sim.pop = read.csv("data/stepSimPop2015.csv")
  # sim.pop[1,]
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
  vNcdState = sim.pop$ncd
  vHivState = rep(mc$HIV.NEG,n)
  vtDiabInc= c(sim.pop$ncd%in%c(mc$NCD.DIAB,mc$NCD.DIAB_HYP))*-1
  vtHypInc= c(sim.pop$ncd%in%c(mc$NCD.HYP,mc$NCD.DIAB_HYP))*-1
  
  pop = (mapply(Person$new, vIds,vSexes,vAges,mc$TNOW,
                vHivState,
                vNcdState,
                vtDiabInc,
                vtHypInc
  ))
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
set.initial.hiv.status = function(){
  lapply(c(1:length(pop)),function(x){
    p<-pop[[x]]
    
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
  })
  cat("Initial HIV status set \n")
  pop
}

# model one simulated year
print("model.annual.dynamics")
model.annual.dynamics<-function(sim){
  pop=sim$pop
  mc=sim$mc
  gss=sim$gss
  
  #check the TNOW and break if it's not correctly set
  if ((mc$TNOW%%mc$ANNUAL.TIMESTEPS)!=0) break("TNOW is not set correctly")
  cat("Beginning the year ... ",mc$CYNOW,"\n")
  
  {
    ##Probability of HIV mortality (estimated via # events/ elig pop) --------
    n.hiv.pos = apply(hiv.output.for.ncd$population[as.character(mc$CYNOW-1),-1,,],c(2:3),sum) # extract all but hiv.negative, sum over hiv states
    target.hiv.mort = hiv.output.for.ncd$hiv.mortality[as.character(mc$CYNOW),,] # pull out current year; dimensions are year, age, sex
    prob.hiv.mort=target.hiv.mort/n.hiv.pos
    if(sum(prob.hiv.mort>1)>1)
      stop(paste("Error: probability of prob.hiv.mort >1 in year ",mc$CYNOW))
    prob.hiv.mort[prob.hiv.mort==Inf]<-0
    prob.hiv.mort<-prob.hiv.mort/mc$ANNUAL.TIMESTEPS
    
    ##Probability of general mortality (estimated via # events/ elig pop) --------
    n.pop = apply(hiv.output.for.ncd$population[as.character(mc$CYNOW-1),,,],c(2:3),sum) # extract all population, sum over hiv states
    target.general.mort = hiv.output.for.ncd$non.hiv.mortality[as.character(mc$CYNOW),,] # pull out current year; dimensions are year, age, sex
    prob.general.mort=target.general.mort/n.pop
    if(sum(prob.general.mort>1)>1)
      stop(paste("Error: probability of prob.general.mort >1 in year ",mc$CYNOW))
    prob.general.mort[prob.general.mort==Inf]<-0
    prob.general.mort<-prob.general.mort/mc$ANNUAL.TIMESTEPS
    
    ##Probability of incidence (estimated via # events/ elig pop) --------
    n.hiv.neg = hiv.output.for.ncd$population[as.character(mc$CYNOW-1),"hiv_negative",,]
    target.inc = hiv.output.for.ncd$incidence[as.character(mc$CYNOW),,] # pull out current year; dimensions are year, age, sex
    prob.inc=target.inc/n.hiv.neg
    if(sum(prob.inc>1)>1)
      stop(paste("Error: probability of prob.inc >1 in year ",mc$CYNOW))
    prob.inc[prob.inc==Inf]<-0
    prob.inc<-prob.inc/mc$ANNUAL.TIMESTEPS
    
    ##Probability of engagement (direct input to HIV model) --------
    prob.eng=target.parameters$prob.eng[as.character(mc$CYNOW),,]
    if(sum(prob.eng>1)>1)     
      stop(paste("Error: probability of engagement >1 in year ",mc$CYNOW))
    prob.eng[prob.eng==Inf]<-0
    prob.eng<-prob.eng/mc$ANNUAL.TIMESTEPS
    
    ##Probability of disengagement (direct input to HIV model) --------
    prob.diseng=target.parameters$prob.diseng[as.character(mc$CYNOW),,]
    if(sum(prob.diseng>1)>1)     
      stop(paste("Error: probability of disengagement >1 in year ",mc$CYNOW))
    prob.diseng[prob.diseng==Inf]<-0
    prob.diseng<-prob.diseng/mc$ANNUAL.TIMESTEPS
    
    ##Probability of diagnosis (direct input to HIV model) --------
    prob.diag = target.parameters$prob.diag[as.character(mc$CYNOW),,]
    if(sum(prob.diag>1)>1)     
      stop(paste("Error: probability of prob.diag >1 in year ",mc$CYNOW))
    prob.diag[prob.diag==Inf]<-0
    prob.diag<-prob.diag/mc$ANNUAL.TIMESTEPS
    
  }
  #----------------
  # MODLING TIMESTEP DYNAMICS -----
  for(i in (1:mc$ANNUAL.TIMESTEPS)){
     mc$TNOW=mc$TNOW+1
    n=length(pop)
    ######
    # Evaluate prob of events ----
    ######
    # Probability of events and mortality are evaluated simultaneously and a person can be marked for both
    invisible(lapply(c(1:n),function(x){
      p=pop[[x]] 
      #1-ENGAGEMENT
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
      
      # HIV MORTALITY
      if(p$hivState!=mc$HIV.NEG){ # all HIV positive
        p.prob = prob.hiv.mort[p$agegroup,p$sex]
        if(runif(1)<p.prob)
          p$bMarkedDead.hiv=T
      }
      
      # GENERAL MORTALITY 
      p.prob = prob.general.mort[p$agegroup,p$sex]
      if(runif(1)<p.prob)
        p$bMarkedDead.gen=T
      
    }))
    
    ######
    # modeling the events
    ######
    # here the order in which the events are modeled will matter for collecting statistics
    # assuming that HIV.cascade.events take place first, then mortality, aging and births 
    n.inc=0
    n.diag=0
    n.eng=0
    n.uneng=0
    
    {
      n=length(pop)
      # 1- Modeling Hiv cascade events  
      invisible(lapply(c(1:n),function(x){
        p=pop[[x]] 
        if (p$bMarkedHivInc+p$bMarkedHivDiag+p$bMarkedHivUneng+p$bMarkedHivEng >1){
          browser()
          stop(paste("can not model more than one hiv transition for person ",x," at year",mc$YNOW," time ",mc$TNOW))
        }
        #
        if(p$bMarkedHivInc==TRUE) {
          p$hiv.getInfected(mc$TNOW)
          gss$n.hiv.inc[p$agegroup,p$sex,as.character(mc$CYNOW)] = gss$n.hiv.inc[p$agegroup,p$sex,as.character(mc$CYNOW)]+1
          n.inc<<-n.inc+1
        }
        if(p$bMarkedHivDiag==TRUE) {
          p$hiv.getDiagnosed(mc$TNOW)
          gss$n.hiv.diag[p$agegroup,p$sex,as.character(mc$CYNOW)] = gss$n.hiv.diag[p$agegroup,p$sex,as.character(mc$CYNOW)]+1
          n.diag<<-n.diag+1
        }
        if(p$bMarkedHivUneng==TRUE) {
          p$hiv.getUnengage(mc$TNOW)
          gss$n.hiv.uneng[p$agegroup,p$sex,as.character(mc$CYNOW)] = gss$n.hiv.uneng[p$agegroup,p$sex,as.character(mc$CYNOW)]+1
          n.eng<<-n.eng+1
        }
        if(p$bMarkedHivEng==TRUE) {
          p$hiv.getEngaged(mc$TNOW)
          gss$n.hiv.eng[p$agegroup,p$sex,as.character(mc$CYNOW)] = gss$n.hiv.eng[p$agegroup,p$sex,as.character(mc$CYNOW)]+1
          n.uneng<<-n.uneng+1
        }
      }))
      
      # 2- Modeling Hiv and non-HIV deaths
      n=length(pop) 
      death.status=unlist(invisible(lapply(c(1:n),function(x){
        return(pop[[x]]$bMarkedDead.hiv)  })))
      n.deaths.hiv<-sum(death.status)
      gss$n.deaths.hiv[mc$YNOW]=n.deaths.hiv
      pop<-pop[!death.status] #only keep those who are alive
      
      n=length(pop)
      death.status=unlist(invisible(lapply(c(1:n),function(x){
        return(pop[[x]]$bMarkedDead.gen)  })))
      n.deaths.gen<-sum(death.status)
      gss$n.deaths.gen[mc$YNOW]=n.deaths.gen
      pop<-pop[!death.status] #only keep those who are alive
    }
    
    cat("End of timestep: ",mc$TNOW," ---")
    # cat("Total incidence= ",n.inc," diag= ",n.diag," eng= ",n.eng," uneng= ",n.uneng)
    # cat("Hiv.deaths=",n.deaths.hiv,"gen.deaths=",n.deaths.gen,"\n")
    cat("current pop size is ",length(pop),"\n")
  }
  ################################################################
  # MODEL AGING --------
  invisible(lapply(pop,function(x){x$incAge}))
  # Aging out:
  n.ageout=sum(unlist(invisible(lapply(pop,function(x){
    if(x$age>mc$MAX.AGE) {
      x$bMarkedDead.ageout=T;
      return(1)
    }}))))
  #Kill those dead
  n<-length(pop)
  death.status=unlist(invisible(lapply(c(1:n),function(x){
    return(pop[[x]]$bMarkedDead.ageout)
  })))
  n.deaths.ageout<-sum(death.status)
  gss$n.deaths.ageout[mc$YNOW]=n.deaths.ageout
  pop<-pop[!death.status]   #only keep those who are alive #'@JP: should we delete people directly?
  
  cat("End of year: ---")
  cat("current pop size is ",length(pop),"\n")
  
  ##
  ## MODEL BIRTHS --------
  # non-HIV births
  absolute.n.births.non.hiv=target.parameters$non.hiv.births[as.character(mc$CYNOW)] # total non HIV births from HIV model 
  non.hiv.births.scalar = absolute.n.births.non.hiv/sum(hiv.output.for.ncd$population[as.character(mc$CYNOW),,,]) # scaled to pop size from HIV model
  n.births.non.hiv = round(non.hiv.births.scalar*length(pop),0) # re-scaled to pop size from NCD model
  
  if(n.births.non.hiv>0){
    cat(n.births.non.hiv," non-HIV newborns are added","\n")
    vIds = c((mc$lastID+1): (mc$lastID+n.births.non.hiv))
    mc$lastID=mc$lastID+n.births.non.hiv
    vSexes = sample(c(mc$MALE,mc$FEMALE),n.births.non.hiv,prob = c(.5,.5),replace = T) # still 50/50 male/female
    pop1 = (mapply(Person$new, vIds,vSexes,0,mc$TNOW,mc$HIV.NEG,mc$NCD.NEG)) #'@JP: do we need to delete pop1 and open memory?
    pop<-c(pop,pop1)
    #
    gss$n.births.non.hiv[mc$YNOW]=n.births.non.hiv
  }
  
  # HIV births - putting into undiagnosed for now
  absolute.n.births.hiv=target.parameters$hiv.births[as.character(mc$CYNOW)] # total HIV births from HIV model
  hiv.births.scalar = absolute.n.births.hiv/sum(hiv.output.for.ncd$population[as.character(mc$CYNOW),,,]) # scaled to pop size from HIV model
  n.births.hiv = round(hiv.births.scalar*length(pop),0) # re-scaled to pop size from NCD mode
  
  if(n.births.hiv>0){
    cat(n.births.hiv," HIV newborns are added","\n")
    vIds = c((mc$lastID+1): (mc$lastID+n.births.hiv))
    mc$lastID=mc$lastID+n.births.hiv
    vSexes = sample(c(mc$MALE,mc$FEMALE),n.births.hiv,prob = c(.5,.5),replace = T)
    pop1 = (mapply(Person$new, vIds,vSexes,0,mc$TNOW,mc$HIV.UNDIAG,mc$NCD.NEG)) #'@JP: do we need to delete pop1 and open memory?
    pop<-c(pop,pop1)
    #
    gss$n.births.hiv[mc$YNOW]=n.births.hiv
  }
  
  gss$n.births[mc$YNOW]=n.births.non.hiv + n.births.hiv
  
  ##############################################
  # END OF YEAR----
  cat("Final pop size is ",length(pop),"\n")
  print(paste("End of year: ",mc$CYNOW," ---------------------------"))
  
  
  # Record annual statatistics --------
  gss$pop.size[mc$YNOW]=length(pop)
  gss$n.hiv.prev[,,,mc$YNOW]= return.gss.hiv.state.sizes(pop)
  
  #'@MS: for now, we let the TNOW increase over time and use it to record the event times for agents
  mc$YNOW<-mc$YNOW+1
  mc$CYNOW<-mc$CYNOW+1
  
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
