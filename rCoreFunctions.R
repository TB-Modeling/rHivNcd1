#
#  R HIVNCD 2022
#  R Core functions
#  
#####################################
print("Reading R Core Functions... ")

#creates the initial population
print("create.initial.population")
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
  
  #Add one year
  currentYear<-mc$INITIAL.YEAR+mc$YNOW
  cat("Beginning the year ",currentYear,"\n")
  
  # #Aging
  # invisible(lapply(pop,function(x){x$incAge}))
  # barplot(cReturnAgDist(pop),names.arg = mc$DIM.NAMES.AGE,main=paste("Age distribution tnow=",mc$TNOW))
  
  {
    ##Probability of engagement--------
    n.hiv.uneng = hiv.output.for.ncd$population[as.character(currentYear-1),"diagnosed_unengaged",,] #'@MS: shouldn't we use the last year's population for denom here?
    target.eng = hiv.output.for.ncd$engagement[as.character(currentYear),,] # pull out current year; dimensions are year, age, sex
    prob.eng=target.eng/n.hiv.uneng
    if(sum(prob.eng>1)>1)     stop(paste("probability of engagement >1 in year ",currentYear))
    prob.eng[prob.eng==Inf]<-0
    prob.eng<-prob.eng/mc$ANNUAL.TIMESTEPS #'@MS: should convert this to values per timestep (monthly or quarterly) 
    
    ##Probability of disengagement--------
    n.hiv.eng = hiv.output.for.ncd$population[as.character(currentYear-1),"engaged",,]
    target.diseng = hiv.output.for.ncd$disengagement[as.character(currentYear),,] # pull out current year; dimensions are year, age, sex
    prob.diseng=target.diseng/n.hiv.eng
    if(sum(prob.diseng>1)>1)     stop(paste("probability of disengagement >1 in year ",currentYear))
    prob.diseng[prob.diseng==Inf]<-0
    prob.diseng<-prob.diseng/mc$ANNUAL.TIMESTEPS
    
    ##Probability of diagnosis--------
    n.hiv.undiag = hiv.output.for.ncd$population[as.character(currentYear-1),"undiagnosed",,]
    target.diag = hiv.output.for.ncd$diagnosis[as.character(currentYear),,] # pull out current year; dimensions are year, age, sex
    prob.diag=target.diag/n.hiv.undiag
    if(sum(prob.diag>1)>1)     stop(paste("probability of prob.diag >1 in year ",currentYear))
    prob.diag[prob.diag==Inf]<-0
    prob.diag<-prob.diag/mc$ANNUAL.TIMESTEPS
    
    ##Probability of incidence--------
    n.hiv.neg = hiv.output.for.ncd$population[as.character(currentYear-1),"hiv_negative",,]
    target.inc = hiv.output.for.ncd$incidence[as.character(currentYear),,] # pull out current year; dimensions are year, age, sex
    prob.inc=target.inc/n.hiv.neg
    if(sum(prob.inc>1)>1)     stop(paste("probability of prob.inc >1 in year ",currentYear))
    prob.inc[prob.inc==Inf]<-0
    prob.inc<-prob.inc/mc$ANNUAL.TIMESTEPS
  }
  ################################################################
  # MODLING TIMESTEP DYNAMICS
  for(i in (1:mc$ANNUAL.TIMESTEPS)){
    mc$TNOW=mc$TNOW+1
    n=length(pop)
    #loop through the population
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
    print("1-----------------------")
    n.inc=0
    n.diag=0
    n.eng=0
    n.uneng=0
    ### model hiv events that are marked:   #loop through the population
    lapply(c(1:n),function(x){
      p=pop[[x]] 
      
      if (p$bMarkedHivInc+p$bMarkedHivDiag+p$bMarkedHivUneng+p$bMarkedHivEng >1){
        browser()
        stop(paste("can not model more than one hiv transition for person ",x," at time ",mc$TNOW))
      }
      #
      if(p$bMarkedHivInc==TRUE) {
        p$hiv.getInfected(mc$TNOW)
        gss$n.hiv.inc[p$sex,p$agegroup,as.character(mc$CYNOW)] = gss$n.hiv.inc[p$sex,p$agegroup,as.character(mc$CYNOW)]+1
        
      }
      if(p$bMarkedHivDiag==TRUE) {
        p$hiv.getDiagnosed(mc$TNOW)
        gss$n.hiv.diag[p$sex,p$agegroup,as.character(mc$CYNOW)] = gss$n.hiv.diag[p$sex,p$agegroup,as.character(mc$CYNOW)]+1
      }
      if(p$bMarkedHivUneng==TRUE) {
        p$hiv.getUnengage(mc$TNOW)
        gss$n.hiv.uneng[p$sex,p$agegroup,as.character(mc$CYNOW)] = gss$n.hiv.uneng[p$sex,p$agegroup,as.character(mc$CYNOW)]+1
      }
      if(p$bMarkedHivEng==TRUE) {
        p$hiv.getEngaged(mc$TNOW)
        gss$n.hiv.eng[p$sex,p$agegroup,as.character(mc$CYNOW)] = gss$n.hiv.eng[p$sex,p$agegroup,as.character(mc$CYNOW)]+1
      }
    })
    
    
    print(paste("End of timestep: ",mc$TNOW))
  }
  
  #end of year
  # ################################################################
  # ## DEATHS
  # # Aging out:
  # n.ageout=sum(unlist(invisible(lapply(pop,function(x){
  #   if(x$age>mc$MAX.AGE) {
  #     x$bMarkedDead=T;
  #     return(1)
  #   }}))))
  # cat(n.ageout," persons are aging out","\n")
  # 
  # #Kill those dead
  # npop<-length(pop)
  # death.status=unlist(invisible(lapply(c(1:npop),function(x){
  #   return(pop[[x]]$bMarkedDead)
  # })))
  # n.deaths<-sum(death.status)
  # gss$n.deaths[mc$YNOW]=n.deaths
  # cat(n.deaths," persons are marked dead","\n")
  # #only keep those who are alive
  # pop<-pop[!death.status] #'@JP: should we delete these dead people?
  # 
  # cat("current pop size is ",length(pop),"\n")
  # ################################################################
  # ### BIRTHS
  # #compute number of newborns needed (assuming a fix pop size for now)
  # n.births=mc$POP.SIZE-length(pop)
  # if(n.births>0){
  #   cat(n.births," newborns are added","\n")
  #   vIds = c((mc$lastID+1): (mc$lastID+n.births))
  #   mc$lastID=mc$lastID+n.births
  #   vSexes = sample(c(mc$MALE,mc$FEMALE),n.births,prob = c(.5,.5),replace = T)
  #   pop1 = (mapply(Person$new, vIds,vSexes,0,mc$TNOW,mc$NCD.NEG)) #'@JP: do we need to delete pop1 and open memory?
  #   pop<-c(pop,pop1)
  #   #
  #   gss$n.births[mc$YNOW]=n.births
  # }
  # 
  # #
  # gss$pop.size[mc$YNOW]=length(pop)

  # 
  mc$YNOW<-mc$YNOW+1
  mc$CYNOW<-mc$CYNOW+1
  cat("Final pop size is ",length(pop),"\n")
  
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

# hiv.state.sizes<-array(0,dim=c(mc$NUM.SEXES,mc$NUM.AGE.GROUPS,mc$NUM.HIV.STATES),
#                        dimnames = list(mc$DIM.NAMES.SEX,mc$DIM.NAME.AGEGROUP,mc$DIM.NAMES.HIV))
# invisible(lapply(c(1:n),function(x){hiv.state.sizes[ pop[[x]]$sex, pop[[x]]$agegroup, pop[[x]]$hivState]<<-hiv.state.sizes[ pop[[x]]$sex, pop[[x]]$agegroup, pop[[x]]$hivState]+1  }))
