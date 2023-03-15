#
#  R HIVNCD 2022
#  rCoreFunctions.R class
#  
#####################################
print("Sourcing rCoreFunctions.R ... ")

#boolean options to control printing messages throughout the code.
bPrint1=T; # printing year's start/end/pop size
bPrint2=F  #printing events: incidence, deaths, newborns

#creates the initial population
print("Loading function create.initial.pop.list")
create.initial.population <- function( id=0,
                                       n=0 # number of people if not specified as in mc
){
  # 1- create an empty population
  pop<-POPULATION$new(id = id,
                      members = list(),
                      params = generate.new.modelParameter(),
                      stats =  generate.new.stat())
  
  # 2- create member list of persons for this population 
  #subset the first n row to create n persons
  sim.pop=pop$params$step.dataset
  if (n>nrow(sim.pop)) stop ("Requested size of initial population is greater than simpop data in 2015")
  sim.pop=sim.pop[1:n,]
  #set up attributes for each row
  sim.pop$age[sim.pop$age==0] = 0.5 #we need this so that the first agegroups is set to 1
  sim.pop$age[sim.pop$age>85] = 85 
  sim.pop$sex[sim.pop$sex=="MALE"] = MALE 
  sim.pop$sex[sim.pop$sex=="FEMALE"] = FEMALE
  sim.pop$ncd[sim.pop$hypertension==0 & sim.pop$diabetes==0] = NCD.NEG #1, neither 
  sim.pop$ncd[sim.pop$hypertension==0 & sim.pop$diabetes==1] = NCD.DIAB #2, diabetic
  sim.pop$ncd[sim.pop$hypertension==1 & sim.pop$diabetes==0] = NCD.HYP #3, hypertensive
  sim.pop$ncd[sim.pop$hypertension==1 & sim.pop$diabetes==1] = NCD.DIAB_HYP #4, both
  #vector of attributes for agents
  vIds = c(1:n) #@MS: the person id is a unique number that we use to differentiate people. 
  # we need to start this from 0 and add it over time. 
  # otherwise, the future values may overlap with ones you're reading from the step dataset sim.pop$X
  vAges = sim.pop$age
  vSexes = as.numeric(sim.pop$sex)
  vNcdState = sim.pop$ncd
  vHivState = rep(HIV.NEG,n)
  vtDiabInc= c(sim.pop$ncd%in%c(NCD.DIAB))*0
  vtHypInc= c(sim.pop$ncd%in%c(NCD.HYP))*0
  vtDiabHypInc= c(sim.pop$ncd%in%c(NCD.DIAB_HYP))*0
  
  memberList = (mapply(PERSON$new, 
                       vIds,
                       vSexes,
                       vAges,
                       pop$params$TNOW,
                       vHivState,
                       vNcdState,
                       vtDiabInc,
                       vtHypInc,
                       vtDiabHypInc
  ))
  memberList = unlist(memberList)
  pop$members<-memberList
  
  pop$greet()
  
  pop
}


# sets the initial HIV status (based on HIV state prevalence in year 2015) 
print("Loading function set.initial.hiv.status")
set.initial.hiv.status = function(pop){
  #get hiv state proportions by age and sex
  hiv.probs = transform.hiv.data.1d.to.3d(pop$params$khm.hivPrev2015)
  # dim(hiv.probs)
  
  invisible(lapply(pop$members,function(p){
    p.probs = hiv.probs[,p$agegroup,p$sex]
    # break to ensure that sum of p.probs==1
    if(round(sum(p.probs),0)!=1){
      stop(paste0("Error: p.probs for: age ",dimnames(hiv.probs)[[2]][p$agegroup],
                  ", sex ",dimnames(hiv.probs)[[3]][p$sex], " does not sum to 1"))
    }
    #randomly assign HIV state based on probs
    rand.hiv.state = sample(x = c(1:length(p.probs)), size = 1, prob = p.probs)
    p$hivState=rand.hiv.state
  }))
  
  if(bPrint2) cat("Initial HIV status set \n")
  pop
}

# sets the monthly CVD risk 
print("Loading function set.cvd.risk")
set.cvd.risk = function(pop){
  invisible(lapply(pop$members,function(p){
    # for whatever age group they are in, access the 10-year risk for the previous age group 
    p.agegroup=pmax(1, p$agegroup-1 )# for the youngest age group, just make this 1, not 0
    p$monthlyCvdRisk= pop$params$monthly.cvd.risk.by.age.sex[p.agegroup,p$sex,p$ncdState]
  }))
  if (bPrint2) cat("Annual CVD risks are set \n")
  pop
}


# models the HIV transitions 
print("Loading function model.hiv.transitions")
model.hiv.transitions<-function(pop,
                                prob.inc,
                                prob.eng,
                                prob.diseng,
                                prob.diag){
  invisible(lapply(pop$members,function(p){
    #1-ENGAGEMENT
    if (p$hivState == HIV.UNENG) {
      if (runif(1) < prob.eng[p$agegroup,p$sex]){
        p$model.hiv.eng(pop$params$TNOW)
        pop$record.hiv.eng(p$agegroup,p$sex,p$hivState,p$ncdState)
      }
    }else{      #2- DISENGAGEMENT
      if (p$hivState== HIV.ENG) {
        if (runif(1)<prob.diseng[p$agegroup,p$sex]){
          p$model.hiv.uneng(pop$params$TNOW)
          pop$record.hiv.uneng(p$agegroup,p$sex,p$hivState,p$ncdState)
        }
      }else{        #3- DIAGNOSIS
        if (p$hivState== HIV.UNDIAG) {
          if (runif(1)<prob.diag[p$agegroup,p$sex]){
            p$model.hiv.diag(pop$params$TNOW)
            pop$record.hiv.diag(p$agegroup,p$sex,p$hivState,p$ncdState)
          }
        }else{          #4- INCIDENCE
          if (p$hivState== HIV.NEG) {
            if (runif(1)<prob.inc[p$agegroup,p$sex]){
              p$model.hiv.inc(pop$params$TNOW)
              pop$record.hiv.inc(p$agegroup,p$sex,p$hivState,p$ncdState)
            }
          }else {
            browser()
            stop(paste("Error: Person ",x," hivState is ",p$hivState," and it didnt meet any criteria"))
          }}}}}))
  pop
}

# models the CVD events
print("Loading function model.cvd.events")
model.cvd.events<-function(pop){
  invisible(lapply(pop$members,function(p){
    p.cvd.risk = p$return.cvd.risk(pop$params) # this function evaluates whether they have history of cvd events and returns appropriate risk 
    
    if(runif(1) < p.cvd.risk){ # evaluate if they have a cvd event 
      # evaluate whether this should be a stroke event or mi event (assign default male probability, change to female if sex is female)
      prob.mi=pop$params$prob.first.cvd.event.mi.male
      if(p$sex==FEMALE) prob.mi=pop$params$prob.first.cvd.event.mi.female
      
      if(runif(1) < prob.mi){ # mi event
        p$model.mi.event(pop$params$TNOW)
        pop$record.mi.inc(p$agegroup,p$sex,p$hivState,p$ncdState)
      } else{ # stroke event
        p$model.stroke.event(pop$params$TNOW)
        pop$record.stroke.inc(p$agegroup,p$sex,p$hivState,p$ncdState)
      }}}))
  pop
}

# models the CVD deaths
print("Loading function model.cvd.deaths")
model.cvd.deaths = function(pop){
  invisible(lapply(pop$members,function(p){
    if(runif(1) < p$return.cvd.mortality(pop$params))
      p$bMarkedDead.cvd=T
  }))
  pop
}

# removes the HIV & CVD deaths
print("Loading function remove.hiv.cvd.deaths")
remove.hiv.cvd.deaths<-function(pop,
                                prob.hiv.mort,
                                prob.non.hiv.mort){
  
  #evaluate prob of hiv and non.hiv mortality for everyone who is not already marked dead
  invisible(lapply(pop$members,function(p){
    if (p$bMarkedDead.cvd==FALSE){
      # HIV MORTALITY
      if(p$hivState!=HIV.NEG){ # all HIV positive
        if(runif(1)<prob.hiv.mort[p$agegroup,p$sex])
          p$bMarkedDead.hiv=T
      }
      # NON.HIV MORTALITY 
      if(runif(1)< prob.non.hiv.mort[p$agegroup,p$sex])
        p$bMarkedDead.non.hiv=T
    }}))
  
  # removing deaths & saving stats
  { death.ids=NULL
    n.cvd.deaths=0
    n.hiv.deaths=0
    n.nonHiv.deaths=0
    lapply(1:length(pop$members),function(x){
      p=pop$members[[x]]
      if (bMarkedDead.cvd==TRUE) {
        n.cvd.deaths<<-n.cvd.deaths+1
        death.ids<<-c(death.ids,x)
      }else{
        if (bMarkedDead.hiv ==TRUE) {
          n.hiv.deaths<<-n.hiv.deaths+1
          death.ids<<-c(death.ids,x)
        }else{
          if (bMarkedDead.non.hiv ==TRUE) {
            n.nonHiv.deaths<<-n.nonHiv.deaths+1
            death.ids<<-c(death.ids,x)
          }}}})
    #removing dead people:
    pop$members<-pop$members[-death.ids]
    #stats
    pop$stats$n.deaths.cvd[pop$params$YNOW]=n.cvd.deaths
    pop$stats$n.deaths.hiv[pop$params$YNOW]=n.hiv.deaths
    pop$stats$n.deaths.non.hiv[pop$params$YNOW]=n.nonHiv.deaths
    
    if (bPrint2) cat("modeled n.deaths.cvd=",n.cvd.deaths," n.deaths.hiv=",n.hiv.deaths," n.deaths.non.hiv=",n.nonHiv.deaths,"\n")
  }
  pop
}

# update NCD state after aging
print("Loading function update.ncd.states")
update.ncd.states<-function(pop){ 
  # for each age/sex subgroup:
  #   compute current prp of ncd states, compare to baseline proportions, and estimate the difference
  # if diference > 0, assign individuals to new ncd states with corresponding probabilities 
  # states: S, D, H, DH
  # only allowing one NCD incidence at a time (H>DH, D>DH and not S>>DH)
  # 1- modeling moves from H >> DH or D>> DH
  # 2- modeling moves from S >> D
  # 3- modeling moves from S >>H
  
  # TARGET ncNCDd state sizes and proportions 
  # target.ncd.sizes
  # target.ncd.props
  
  # DIAB HYP #######################################
  # CURRENT NCD state sizes & prop
  current.ncd.states = filter.5D.stats.by.field(pop$return.state.size.distribution(),
                                                years = as.character(pop$params$CYNOW),
                                                keep.dimensions = c('age','sex','ncd.status','year'))
  current.ncd.states=current.ncd.states[,,,1] #to remove year dimension
  # dimnames(current.ncd.states)
  current.ncd.props<-return.prop.sex.age(vFreq = current.ncd.states)
  
  # DIFFERENCE in prevalence of NCDs
  # dimnames(pop$params$target.ncd.props)
  diff.props =  pop$params$target.ncd.props-current.ncd.props
  
  # ADDITIONAL Transitions required to reach the target proportions in our current population:
  trans.freq=diff.props
  invisible(lapply(1:DIM.AGE, function(ag){
    lapply(1:DIM.SEX, function(sex){
      lapply(1:DIM.NCD, function(ncd){
        trans.freq[ag,sex,ncd]<<-diff.props[ag,sex,ncd]*sum(current.ncd.states[ag,sex,]) # the required number of new transitions
      })})}))
  trans.freq[trans.freq<0]<-0
  
  #PROBABILITY Of transition to DH for those in D or H state
  trans.prob.diab.hyp=
    trans.freq[,,"NCD.DIAB_HYP"]/(current.ncd.states[,,"NCD.DIAB"]+current.ncd.states[,,"NCD.HYP"])
  
  # TRANSITION to DH from D or H 
  invisible(lapply(pop$members,function(p){
    if(p$ncdState==NCD.DIAB || p$ncdState==NCD.HYP){ 
      if(runif(1) < trans.prob.diab.hyp[p$agegroup,p$sex]){
        p$model.diab.hyp.inc(pop$params$TNOW)
        pop$record.diab.hyp.inc(p$agegroup,p$sex,p$hivState,p$ncdState)
      }}}))
  
  # DIAB #######################################
  current.ncd.states = filter.5D.stats.by.field(pop$return.state.size.distribution(),
                                                years = as.character(pop$params$CYNOW),
                                                keep.dimensions = c('age','sex','ncd.status','year'))
  current.ncd.states=current.ncd.states[,,,1] #to remove year dimension
  # dimnames(current.ncd.states)
  current.ncd.props<-return.prop.sex.age(current.ncd.states)
  
  # DIFFERENCE in prevalence of NCDs
  diff.props =  pop$params$target.ncd.props-current.ncd.props
  
  # ADDITIONAL Transitions required to reach the target proportions:
  trans.freq=diff.props
  invisible(lapply(1:DIM.AGE, function(ag){
    lapply(1:DIM.SEX, function(sex){
      lapply(1:DIM.NCD, function(ncd){
        trans.freq[ag,sex,ncd]<<-diff.props[ag,sex,ncd]*sum(current.ncd.states[ag,sex,]) # the required number of new transitions
      })})}))
  trans.freq[trans.freq<0]<-0
  
  #PROBABILITY Of transition to DIAB for those in NCD.NEG
  trans.prob.diab= trans.freq[,,"NCD.DIAB"]/(current.ncd.states[,,"NCD.NEG"])
  
  # TRANSITION to D from neg
  invisible(lapply(c(1:length(pop$members)),function(x){
    p=pop$members[[x]] 
    if(p$ncdState==NCD.NEG){
      if(runif(1)<trans.prob.diab[p$agegroup,p$sex]){
        p$model.diab.inc(pop$params$TNOW)
        pop$record.diab.inc(p$agegroup,p$sex,p$hivState,p$ncdState)
        }}}))
  
  # HYP #############################################################################
  current.ncd.states = filter.5D.stats.by.field(pop$return.state.size.distribution(),
                                                years = as.character(pop$params$CYNOW),
                                                keep.dimensions = c('age','sex','ncd.status','year'))
  current.ncd.states=current.ncd.states[,,,1] #to remove year dimension
  # dimnames(current.ncd.states)
  current.ncd.props<-return.prop.sex.age(current.ncd.states)
  
  # DIFFERENCE in prevalence of NCDs
  diff.props =  pop$params$target.ncd.props-current.ncd.props
  
  # ADDITIONAL Transitions required to reach the target proportions:
  trans.freq=diff.props
  invisible(lapply(1:DIM.AGE, function(ag){
    lapply(1:DIM.SEX, function(sex){
      lapply(1:DIM.NCD, function(ncd){
        trans.freq[ag,sex,ncd]<<-diff.props[ag,sex,ncd]*sum(current.ncd.states[ag,sex,]) # the required number of new transitions
      })})}))
  trans.freq[trans.freq<0]<-0
  
  #PROBABILITY Of transition to HYP for those in NCD.NEG
  trans.prob.hyp= trans.freq[,,"NCD.HYP"]/(current.ncd.states[,,"NCD.NEG"])
  
  # TRANSITION to H from neg
  invisible(lapply(c(1:length(pop$members)),function(x){
    p=pop$members[[x]] 
    if(p$ncdState==NCD.NEG){
      if(runif(1)<trans.prob.diab[p$agegroup,p$sex]){
      p$model.hyp.inc(pop$params$TNOW)
      pop$record.hyp.inc(p$agegroup,p$sex,p$hivState,p$ncdState)
      }}}))
  pop
}

# model one simulated year
print("Loading function model.annual.dynamics")
run.one.year<-function(pop){
  # current age at year's beginning:
  # Jan 1st to Dec 31st:
  
  # each months:
  ## changes in HIV states from khm
  ## new cvd events based on current risks
  ## deaths from khm (HIV and age-specific deaths) > these include some CVD deaths too
  ## deaths from CVD events
  
  # end of year:
  # deaths due to aging
  # remaining deaths to balance with khm population
  # new births 
  
  # aging >>  
  # changes in NCD states
  # updating cvd annual risks based on new age and ncd state
  
  #check the TNOW and break if it's not correctly set
  if(bPrint2) cat("Beginning the year ... ",pop$params$CYNOW,"\n")
  
  #### AT YEAR's BEGINNING:
  { #computing event probabilities from KHM
    ##Probability of HIV mortality (estimated via # events/ elig pop) --------
    khm=pop$params$khm
    n.hiv.pos = apply(khm$population[as.character(pop$params$CYNOW-1),-1,,],c(2:3),sum) # extract all but hiv.negative, sum over hiv states
    target.hiv.mort = khm$hiv.mortality[as.character(pop$params$CYNOW),,] # pull out current year; dimensions are year, age, sex
    prob.hiv.mort=target.hiv.mort/n.hiv.pos
    if(sum(prob.hiv.mort>1)>1)
      stop(paste("Error: probability of prob.hiv.mort >1 in year ",pop$params$CYNOW))
    prob.hiv.mort[prob.hiv.mort==Inf]<-0
    khm.prob.hiv.mort<-prob.hiv.mort/ANNUAL.TIMESTEPS
    
    ##Probability of non.HIV mortality (estimated via # events/ elig pop) --------
    n.pop = apply(khm$population[as.character(pop$params$CYNOW-1),,,],c(2:3),sum) # extract all population, sum over hiv states
    target.non.hiv.mort = khm$non.hiv.mortality[as.character(pop$params$CYNOW),,] # pull out current year; dimensions are year, age, sex
    prob.non.hiv.mort=target.non.hiv.mort/n.pop
    if(sum(prob.non.hiv.mort>1)>1)
      stop(paste("Error: probability of prob.non.hiv.mort >1 in year ",pop$params$CYNOW))
    prob.non.hiv.mort[prob.non.hiv.mort==Inf]<-0
    khm.prob.non.hiv.mort<-prob.non.hiv.mort/ANNUAL.TIMESTEPS
    
    ##Probability of incidence (estimated via # events/ elig pop) --------
    n.hiv.neg = khm$population[as.character(pop$params$CYNOW-1),"HIV.NEG",,]
    target.inc = khm$incidence[as.character(pop$params$CYNOW),,] # pull out current year; dimensions are year, age, sex
    prob.inc=target.inc/n.hiv.neg
    if(sum(prob.inc>1)>1)
      stop(paste("Error: probability of prob.inc >1 in year ",pop$params$CYNOW))
    prob.inc[prob.inc==Inf]<-0
    khm.prob.hiv.inc<-prob.inc/ANNUAL.TIMESTEPS
    
    ##Probability of engagement (direct input to HIV model) --------
    prob.eng=khm$target.parameters$prob.eng[as.character(pop$params$CYNOW),,]
    if(sum(prob.eng>1)>1)     
      stop(paste("Error: probability of engagement >1 in year ",pop$params$CYNOW))
    prob.eng[prob.eng==Inf]<-0
    khm.prob.hiv.eng<-prob.eng/ANNUAL.TIMESTEPS
    
    ##Probability of disengagement (direct input to HIV model) --------
    prob.diseng=khm$target.parameters$prob.diseng[as.character(pop$params$CYNOW),,]
    if(sum(prob.diseng>1)>1)     
      stop(paste("Error: probability of disengagement >1 in year ",pop$params$CYNOW))
    prob.diseng[prob.diseng==Inf]<-0
    khm.prob.hiv.diseng<-prob.diseng/ANNUAL.TIMESTEPS
    
    ##Probability of diagnosis (direct input to HIV model) --------
    prob.diag = khm$target.parameters$prob.diag[as.character(pop$params$CYNOW),,]
    if(sum(prob.diag>1)>1)     
      stop(paste("Error: probability of prob.diag >1 in year ",pop$params$CYNOW))
    prob.diag[prob.diag==Inf]<-0
    khm.prob.hiv.diag<-prob.diag/ANNUAL.TIMESTEPS
    
  }
  
  ##### AT EACH TIMESTEP WITHIN THE YEAR:
  for(i in (1:ANNUAL.TIMESTEPS)){
    
    if (bPrint1) cat("Running month ",pop$params$TNOW,"\n")
    # CVD events and HIV transitions are independent, so the order doesn't matter    # we model HIV deaths based on new HIV states after new transitions are modeled
    
    # 1- modeling cvd events based on current cvd annual risk    # counting new events: marking those who will die after the event
    pop<-model.cvd.events(pop)
    
    # 2- modeling HIV transitions based on khm outputs
    pop<-model.hiv.transitions(pop,
                               khm.prob.hiv.inc, 
                               khm.prob.hiv.eng, 
                               khm.prob.hiv.diseng, 
                               khm.prob.hiv.diag)
    # filter.stateSizes.by.field(pop$return.state.size.distribution(), years = "2015",keep.dimensions = c("year","hiv.status") )
    
    # 3- modeling HIV and CVD deaths
    pop<-model.cvd.deaths(pop)
    pop<-remove.hiv.cvd.deaths( pop,
                                khm.prob.hiv.mort,
                                khm.prob.non.hiv.mort)
    
    pop$increaseMonth()
    
  }
  #### AT YEAR's END:
  ## 1-MODEL Deaths due to aging out --------
  n.ageout=sum(unlist(invisible(lapply(pop$members,function(x){
    if(x$age>=MAX.AGE) {
      x$bMarkedDead.ageout=T;
      return(1)
    }}))))
  # modeling deaths
  n.deaths.ageout=pop$remove.dead.ageout()
  pop$stats$n.deaths.ageout[pop$params$YNOW]=n.deaths.ageout
  
  ## 2-MODEL BIRTHS --------
  { # non-HIV births
    absolute.n.births.non.hiv=khm$target.parameters$non.hiv.births[as.character(pop$params$CYNOW)] # total non HIV births from HIV model 
    non.hiv.births.scalar = absolute.n.births.non.hiv/sum(khm$population[as.character(pop$params$CYNOW),,,]) # scaled to pop size from HIV model
    n.births.non.hiv = round(non.hiv.births.scalar*length(pop$members),0) # re-scaled to pop size from NCD model
    
    if(n.births.non.hiv>0){
      if (bPrint2) cat(n.births.non.hiv," non-HIV newborns are added","\n")
      vIds = c((pop$params$LAST.PERSON.ID+1): (pop$params$LAST.PERSON.ID+n.births.non.hiv))
      pop$params$LAST.PERSON.ID=pop$params$LAST.PERSON.ID+n.births.non.hiv
      vSexes = sample(c(MALE,FEMALE),n.births.non.hiv,prob = c(.5,.5),replace = T) # still 50/50 male/female
      memberListNew = (mapply(PERSON$new, vIds,vSexes,0,pop$params$TNOW,HIV.NEG,NCD.NEG)) #'@JP: do we need to delete pop1 and open memory?
      pop$addMembers(unlist(memberListNew))
      # record stats:
      pop$stats$n.births.non.hiv[pop$params$YNOW]=n.births.non.hiv
    }
    
    # HIV births - putting into undiagnosed for now
    absolute.n.births.hiv=khm$target.parameters$hiv.births[as.character(pop$params$CYNOW)] # total HIV births from HIV model
    hiv.births.scalar = absolute.n.births.hiv/sum(khm$population[as.character(pop$params$CYNOW),,,]) # scaled to pop size from HIV model
    n.births.hiv = round(hiv.births.scalar*length(pop$members),0) # re-scaled to pop size from NCD mode
    
    if(n.births.hiv>0){
      if(bPrint2) cat(n.births.hiv," HIV newborns are added","\n")
      vIds = c((pop$params$LAST.PERSON.ID+1): (pop$params$LAST.PERSON.ID+n.births.hiv))
      pop$params$LAST.PERSON.ID=pop$params$LAST.PERSON.ID+n.births.hiv
      vSexes = sample(c(MALE,FEMALE),n.births.hiv,prob = c(.5,.5),replace = T)
      memberListNew = (mapply(PERSON$new, vIds,vSexes,0,pop$params$TNOW,HIV.UNDIAG,NCD.NEG)) #'@JP: do we need to delete pop1 and open memory?
      pop$addMembers(memberListNew)
      # record stats:
      pop$stats$n.births.hiv[pop$params$YNOW]=n.births.hiv
      nMaleNewborns=sum(vSexes==MALE)
      pop$stats$n.hiv.inc["0-4","MALE","HIV.NEG", "NCD.NEG",pop$params$YNOW]=pop$stats$n.hiv.inc["0-4","MALE","HIV.NEG","NCD.NEG",pop$params$YNOW] + nMaleNewborns 
      pop$stats$n.hiv.inc["0-4","FEMALE","HIV.NEG","NCD.NEG",pop$params$YNOW]=pop$stats$n.hiv.inc["0-4","FEMALE","HIV.NEG","NCD.NEG",pop$params$YNOW] + (n.births.hiv-nMaleNewborns)
    }
    #record stats:
    pop$stats$n.births[pop$params$YNOW]=n.births.non.hiv + n.births.hiv
    if(bPrint2) cat("n.births.non.hiv= ",n.births.non.hiv, " n.births.hiv= ",n.births.hiv, " modeled \n")
  }
  
  ## 3- MODEL AGING --------
  # invisible(lapply(pop$members,function(x){x$incAge}))
  pop$modelAging()
  
  ## 4- UPDATE NCD STATES & CVD RISKS FOR NEXT YEAR --------
  pop<-update.ncd.states(pop)
  pop<-invisible(set.cvd.risk(pop))
  
  
  ##############################################
  # END OF YEAR----
  if (bPrint1) cat("Final pop size is ",length(pop$members),"\n")
  if (bPrint1) cat("End of year: ",pop$params$CYNOW," --------------------------- \n")
  
  # Record annual statatistics --------
  pop$record.annual.stats()
  
  #Increment the clock
  pop$increaseYear()
  
  
  
  pop
}





