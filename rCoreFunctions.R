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
initialize.simulation <- function( id=0,
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
  sim.pop$age=sim.pop$age * 12 +sample((0:11),size = n,replace = T)#monthly age values
  sim.pop$age[sim.pop$age>MAX.AGE] = MAX.AGE
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
  tborn=pop$params$TNOW
  memberList = (mapply(PERSON$new,
                       tborn,
                       vIds,
                       vSexes,
                       vAges,
                       vHivState,
                       vNcdState,
                       vtDiabInc,
                       vtHypInc,
                       vtDiabHypInc
  ))
  memberList = unlist(memberList)
  pop$members<-memberList
  pop$stats$ncd.id<-id
  pop$greet()
  #
  pop<-invisible(set.initial.hiv.status(pop ))
  pop<-invisible(set.cvd.risk(pop))
  pop$record.annual.stats()
  pop$increaseYear() 
  #
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
        pop$record.hiv.eng(p$agegroup,p$sex,p$hivState,p$ncdState)
        p$model.hiv.eng(pop$params$TNOW)
      }
    }else{      #2- DISENGAGEMENT
      if (p$hivState== HIV.ENG) {
        if (runif(1)<prob.diseng[p$agegroup,p$sex]){
          pop$record.hiv.uneng(p$agegroup,p$sex,p$hivState,p$ncdState)
          p$model.hiv.uneng(pop$params$TNOW)
        }
      }else{        #3- DIAGNOSIS
        if (p$hivState== HIV.UNDIAG) {
          if (runif(1)<prob.diag[p$agegroup,p$sex]){
            pop$record.hiv.diag(p$agegroup,p$sex,p$hivState,p$ncdState)
            p$model.hiv.diag(pop$params$TNOW)
          }
        }else{          #4- INCIDENCE
          if (p$hivState== HIV.NEG) {
            if (runif(1)<prob.inc[p$agegroup,p$sex]){
              pop$record.hiv.inc(p$agegroup,p$sex,p$hivState,p$ncdState)
              p$model.hiv.inc(pop$params$TNOW)
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
        pop$record.mi.inc(p$agegroup,p$sex,p$hivState,p$ncdState)
        p$model.mi.event(pop$params$TNOW)
      } else{ # stroke event
        pop$record.stroke.inc(p$agegroup,p$sex,p$hivState,p$ncdState)
        p$model.stroke.event(pop$params$TNOW)
      }}}))
  pop
}

# removes the HIV & CVD deaths
print("Loading function remove.hiv.cvd.deaths")
model.hiv.cvd.deaths<-function(pop,
                               prob.hiv.mort,
                               prob.non.hiv.mort){
  
  invisible(lapply(pop$members,function(p){
    #model CVD death
    if(runif(1) < p$return.cvd.mortality(pop$params))
      p$bMarkedDead.cvd=T
    
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
  { 
    death.ids=NULL
    n.cvd.deaths=0
    n.hiv.deaths=0
    n.nonHiv.deaths=0
    lapply(1:length(pop$members),function(x){
      p=pop$members[[x]]
      if (p$bMarkedDead.cvd==TRUE) {
        n.cvd.deaths<<-n.cvd.deaths+1
        death.ids<<-c(death.ids,x)
        
        pop$record.deaths.cvd(p$agegroup,p$sex,p$hivState,p$ncdState)
      }else{
        if (p$bMarkedDead.hiv ==TRUE) {
          n.hiv.deaths<<-n.hiv.deaths+1
          death.ids<<-c(death.ids,x)
          pop$record.deaths.hiv(p$agegroup,p$sex,p$hivState,p$ncdState)
        }else{
          if (p$bMarkedDead.non.hiv ==TRUE) {
            n.nonHiv.deaths<<-n.nonHiv.deaths+1
            death.ids<<-c(death.ids,x)
            pop$record.deaths.non.hiv(p$agegroup,p$sex,p$hivState,p$ncdState)
            
          }}}})
    
    #removing dead people:
    if(!is.null(death.ids))
      pop$members<-pop$members[-death.ids]
    
    
    if (bPrint2) cat("modeled n.deaths.cvd=",n.cvd.deaths," n.deaths.hiv=",n.hiv.deaths," n.deaths.non.hiv=",n.nonHiv.deaths,"\n")
  }
  pop
}


# update NCD state (based on hiv and annual population growth)
print("Loading function update.ncd.states")
update.ncd.states<-function(pop){ 
  #'MS: the aim is to incorporate these two coefficients for sensitivity analysis
  #'we need to include HIV dimention in our calculation
  
  #new parameters to incorporate:
  #1-additional risk of ncds by hiv status (can be a single value or different based on engagement)
  # MP$relative.ncd.risk.by.hiv=1 
  #2-annual growth in age/sex-specific prev of ncds
  # MP$annaul.growth.ncd.prev=1
  
  
  # TARGET PREV OF NCDs BASED ON 2015, HIV STATUS AND ANNUAL GROWTH
  q=array(rep(pop$params$target.ncd.props,4),c(dim(pop$params$target.ncd.props),DIM.HIV))
  dimnames(q)<-list(DIM.NAMES.AGE,DIM.NAMES.SEX,DIM.NAMES.NCD,DIM.NAMES.HIV)
  q<-aperm(q,c(1,2,4,3)) #reorder dimensions
   #'@MS: should incorporate this in params
  target.ncd.props=q
  
  #1-multiply by HIV coef
  target.ncd.props[,,c("HIV.UNDIAG", "HIV.UNENG" , "HIV.ENG"),]=pop$params$relative.ncd.risk.by.hiv* target.ncd.props[,,c("HIV.UNDIAG", "HIV.UNENG" , "HIV.ENG"),]
  
  #2-multiply by rhe annual growth factor:
  target.ncd.props= pop$params$annaul.growth.ncd.prev^(pop$params$CYNOW-INITIAL.YEAR)  * target.ncd.props
  
  # DIAB HYP #######################################
  # CURRENT NCD state sizes & prop
  current.ncd.states = filter.5D.stats.by.field(pop$return.state.size.distribution(),
                                                years = as.character(pop$params$CYNOW),
                                                keep.dimensions = c('age','sex','hiv.status','ncd.status','year'))[,,,,1]#to remove year dimension
  current.ncd.props<-return.prop.sex.age(vFreq = current.ncd.states)
  # dimnames(current.ncd.props);dimnames(current.ncd.states)
  
  diff.props =  target.ncd.props-current.ncd.props
  
  # ADDITIONAL Transitions required to reach the target proportions in our current population:
  trans.freq=diff.props
  invisible(lapply(1:DIM.AGE, function(ag){
    lapply(1:DIM.SEX, function(sex){
      lapply(1:DIM.HIV, function(hiv){
        lapply(1:DIM.NCD, function(ncd){
          trans.freq[ag,sex,hiv,ncd]<<-diff.props[ag,sex,hiv,ncd]*sum(current.ncd.states[ag,sex,hiv,]) # the required number of new transitions
        })})})}))
  trans.freq[trans.freq<0]<-0
  
  #PROBABILITY Of transition to DH for those in D or H state
  trans.prob.diab.hyp=
    trans.freq[,,,"NCD.DIAB_HYP"]/(current.ncd.states[,,,"NCD.DIAB"]+current.ncd.states[,,,"NCD.HYP"])
  trans.prob.diab.hyp[is.na(trans.prob.diab.hyp) | trans.prob.diab.hyp==Inf]<-0
  
  # TRANSITION to DH from D or H
  invisible(lapply(pop$members,function(p){
    if(p$ncdState==NCD.DIAB || p$ncdState==NCD.HYP){
      if(runif(1) < trans.prob.diab.hyp[p$agegroup,p$sex,p$hivState]){
        pop$record.diab.hyp.inc(p$agegroup,p$sex,p$hivState,p$ncdState)
        p$model.diab.hyp.inc(pop$params$TNOW)
      }}}))
  
  # DIAB #######################################
  current.ncd.states = filter.5D.stats.by.field(pop$return.state.size.distribution(),
                                                years = as.character(pop$params$CYNOW),
                                                keep.dimensions = c('age','sex','hiv.status','ncd.status','year'))[,,,,1]#to remove year dimension
  current.ncd.props<-return.prop.sex.age(vFreq = current.ncd.states)
  
  diff.props =  target.ncd.props-current.ncd.props
  
  # ADDITIONAL Transitions required to reach the target proportions in our current population:
  trans.freq=diff.props
  invisible(lapply(1:DIM.AGE, function(ag){
    lapply(1:DIM.SEX, function(sex){
      lapply(1:DIM.HIV, function(hiv){
        lapply(1:DIM.NCD, function(ncd){
          trans.freq[ag,sex,hiv,ncd]<<-diff.props[ag,sex,hiv,ncd]*sum(current.ncd.states[ag,sex,hiv,]) # the required number of new transitions
        })})})}))
  trans.freq[trans.freq<0]<-0
  
  #PROBABILITY Of transition to DIAB for those in NCD.NEG
  trans.prob.diab= trans.freq[,,,"NCD.DIAB"]/(current.ncd.states[,,,"NCD.NEG"])
  trans.prob.diab[is.na(trans.prob.diab) | trans.prob.diab==Inf]<-0
  
  # TRANSITION to D from neg
  invisible(lapply(c(1:length(pop$members)),function(x){
    p=pop$members[[x]]
    if(p$ncdState==NCD.NEG){
      if(runif(1)<trans.prob.diab[p$agegroup,p$sex,p$hivState]){
        pop$record.diab.inc(p$agegroup,p$sex,p$hivState,p$ncdState)
        p$model.diab.inc(pop$params$TNOW)
      }}}))
  
  # HYP #############################################################################
  current.ncd.states = filter.5D.stats.by.field(pop$return.state.size.distribution(),
                                                years = as.character(pop$params$CYNOW),
                                                keep.dimensions = c('age','sex','hiv.status','ncd.status','year'))[,,,,1]#to remove year dimension
  current.ncd.props<-return.prop.sex.age(vFreq = current.ncd.states)
  # dimnames(current.ncd.props);dimnames(current.ncd.states)
  
  diff.props =  target.ncd.props-current.ncd.props
  
  # ADDITIONAL Transitions required to reach the target proportions in our current population:
  trans.freq=diff.props
  invisible(lapply(1:DIM.AGE, function(ag){
    lapply(1:DIM.SEX, function(sex){
      lapply(1:DIM.HIV, function(hiv){
        lapply(1:DIM.NCD, function(ncd){
          trans.freq[ag,sex,hiv,ncd]<<-diff.props[ag,sex,hiv,ncd]*sum(current.ncd.states[ag,sex,hiv,]) # the required number of new transitions
        })})})}))
  trans.freq[trans.freq<0]<-0
  
  #PROBABILITY Of transition to HYP for those in NCD.NEG
  trans.prob.hyp= trans.freq[,,,"NCD.HYP"]/(current.ncd.states[,,,"NCD.NEG"])
  trans.prob.hyp[is.na(trans.prob.hyp) | trans.prob.hyp==Inf]<-0
  
  # TRANSITION to H from neg
  invisible(lapply(c(1:length(pop$members)),function(x){
    p=pop$members[[x]]
    if(p$ncdState==NCD.NEG) {
      if(runif(1)< trans.prob.hyp[p$agegroup,p$sex,p$hivState]){
        pop$record.hyp.inc(p$agegroup,p$sex,p$hivState,p$ncdState)
        p$model.hyp.inc(pop$params$TNOW)
      }}}))
  
  pop
}


# model one simulated year (from Jan 1st to Dec 31st)
print("Loading function model.annual.dynamics")
run.one.year<-function(pop){ 
  #check the TNOW and break if it's not correctly set
  if(bPrint2) cat("Beginning the year ... ",pop$params$CYNOW,"\n")
  
  #### AT YEAR's BEGINNING:computing event probabilities from KHM
  khm=pop$params$khm
  ###computing prob of death
  { 
    ##Probability of HIV mortality (estimated via # events/ elig pop) --------
    n.hiv.pos = apply(khm$population[as.character(pop$params$CYNOW-1),-1,,],c(2:3),sum) # extract all but hiv.negative, sum over hiv states
    target.hiv.mort = khm$hiv.mortality[as.character(pop$params$CYNOW),,] # pull out current year; dimensions are year, age, sex
    n.hiv.pos.now = apply(khm$population[as.character(pop$params$CYNOW),-1,,],c(2:3),sum)
    #we are using average to approximate the integral
    # prob.hiv.mort=target.hiv.mort/((n.hiv.pos + n.hiv.pos.now)/2) 
    prob.hiv.mort=target.hiv.mort/(n.hiv.pos.now+target.hiv.mort)
    if(sum(prob.hiv.mort>1)>1)
      stop(paste("Error: probability of prob.hiv.mort >1 in year ",pop$params$CYNOW))
    prob.hiv.mort[prob.hiv.mort==Inf]<-0
    khm.annual.hiv.mort=prob.hiv.mort
    khm.monthly.hiv.mort=(1-(1-prob.hiv.mort)^(1/12))
    
    ##Probability of non.HIV mortality (estimated via # events/ elig pop) --------
    n.pop = apply(khm$population[as.character(pop$params$CYNOW-1),,,],c(2:3),sum) # extract all population, sum over hiv states
    target.non.hiv.mort = khm$non.hiv.mortality[as.character(pop$params$CYNOW),,] # pull out current year; dimensions are year, age, sex
    n.pop.now = apply(khm$population[as.character(pop$params$CYNOW),,,],c(2:3),sum)
    #we are using average to approximate the integral
    # prob.non.hiv.mort=target.non.hiv.mort/((n.pop + n.pop.now)/2) # denominator is now population as well as aging in - aging out 
    prob.non.hiv.mort=target.non.hiv.mort/( n.pop.now+target.non.hiv.mort)
    if(sum(prob.non.hiv.mort>1)>1){
      browser()
      stop(paste("Error: probability of prob.non.hiv.mort >1 in year ",pop$params$CYNOW))
    }
    prob.non.hiv.mort[prob.non.hiv.mort==Inf]<-0
    khm.annual.non.hiv.mort=prob.non.hiv.mort
    khm.monthly.non.hiv.mort=(1-(1-prob.non.hiv.mort)^(1/12))
    
  }
  ###computing number of births (and distributing them over 12 months)
  {
    # non-HIV births
    absolute.n.births.non.hiv=khm$target.parameters$non.hiv.births[as.character(pop$params$CYNOW)] # total non HIV births from HIV model
    non.hiv.births.scalar = absolute.n.births.non.hiv/sum(khm$population[as.character(pop$params$CYNOW),,,]) # scaled to pop size from HIV model
    n.births.non.hiv = round(non.hiv.births.scalar*length(pop$members),0) # re-scaled to pop size from NCD model
    #distribute births over months
    n.births.non.hiv.per.month=rep(0,ANNUAL.TIMESTEPS)
    if(n.births.non.hiv>0)
      n.births.non.hiv.per.month=as.vector(tabulate(sample(size = n.births.non.hiv,x = c(1:ANNUAL.TIMESTEPS),replace = T),nbins = 12))
    
    # HIV births - putting into undiagnosed for now
    absolute.n.births.hiv=khm$target.parameters$hiv.births[as.character(pop$params$CYNOW)] # total HIV births from HIV model
    hiv.births.scalar = absolute.n.births.hiv/sum(khm$population[as.character(pop$params$CYNOW),,,]) # scaled to pop size from HIV model
    n.births.hiv = round(hiv.births.scalar*length(pop$members),0) # re-scaled to pop size from NCD mode
    #distribute births over months
    n.births.hiv.per.month=rep(0,ANNUAL.TIMESTEPS)
    if(n.births.hiv>0)
      n.births.hiv.per.month=as.vector(tabulate(sample(size = n.births.hiv,x = c(1:ANNUAL.TIMESTEPS),replace = T),nbins = 12))
  }
  ###computing care cascade prob of events
  {
    ##Probability of incidence (estimated via # events/ elig pop) --------
    n.hiv.neg = khm$population[as.character(pop$params$CYNOW-1),"HIV.NEG",,]
    target.inc = khm$incidence[as.character(pop$params$CYNOW),,] # pull out current year; dimensions are year, age, sex
    n.hiv.neg.now = khm$population[as.character(pop$params$CYNOW),"HIV.NEG",,]
    delta = n.hiv.neg.now - n.hiv.neg # change in hiv negative population (aging in - aging out - deaths - hiv incidence)
    delta = delta + target.inc
    #'@MS: maybe change later?
    prob.inc=target.inc/(n.hiv.neg + delta)
    if(sum(prob.inc>1)>1)
      stop(paste("Error: probability of prob.inc >1 in year ",pop$params$CYNOW))
    prob.inc[prob.inc==Inf]<-0
    khm.prob.hiv.inc=(1-(1-prob.inc)^(1/12))
    
    ##Probability of engagement (direct input to HIV model) --------
    prob.eng=khm$target.parameters$prob.eng[as.character(pop$params$CYNOW),,]
    if(sum(prob.eng>1)>1)
      stop(paste("Error: probability of engagement >1 in year ",pop$params$CYNOW))
    prob.eng[prob.eng==Inf]<-0
    khm.prob.hiv.eng=(1-(1-prob.eng)^(1/12))
    
    ##Probability of disengagement (direct input to HIV model) --------
    prob.diseng=khm$target.parameters$prob.diseng[as.character(pop$params$CYNOW),,]
    if(sum(prob.diseng>1)>1)
      stop(paste("Error: probability of disengagement >1 in year ",pop$params$CYNOW))
    prob.diseng[prob.diseng==Inf]<-0
    khm.prob.hiv.diseng=(1-(1-prob.diseng)^(1/12))
    
    ##Probability of diagnosis (direct input to HIV model) --------
    prob.diag = khm$target.parameters$prob.diag[as.character(pop$params$CYNOW),,]    
    if(sum(prob.diag>1)>1)
      stop(paste("Error: probability of prob.diag >1 in year ",pop$params$CYNOW))
    prob.diag[prob.diag==Inf]<-0
    khm.prob.hiv.diag=(1-(1-prob.diag)^(1/12))
  }
  
  ##### AT EACH TIMESTEP WITHIN THE YEAR:
  for(i in (1:ANNUAL.TIMESTEPS)){
    # CVD events and HIV transitions are independent, so the order doesn't matter    # we model HIV deaths based on new HIV states after new transitions are modeled
    
    # 1- modeling cvd events based on current cvd annual risk    # counting new events: marking those who will die after the event
    pop<-model.cvd.events(pop)
    
    # 2- modeling HIV transitions based on khm outputs
    pop<-model.hiv.transitions(pop,
                               khm.prob.hiv.inc,
                               khm.prob.hiv.eng,
                               khm.prob.hiv.diseng,
                               khm.prob.hiv.diag)
    
    # 3- modeling & removing HIV and CVD deaths
    pop<-model.hiv.cvd.deaths( pop,
                               khm.monthly.hiv.mort,
                               khm.monthly.non.hiv.mort)
    
    ## MODEL AGING --------
    pop$modelAging()
    
    ## MODEL BIRTHS -------
    {
      n.births.non.hiv=n.births.non.hiv.per.month[i]
      n.births.hiv=n.births.hiv.per.month[i]
      
      if (n.births.non.hiv>0){
        vIds = c((pop$params$LAST.PERSON.ID+1): (pop$params$LAST.PERSON.ID+n.births.non.hiv))
        pop$params$LAST.PERSON.ID=pop$params$LAST.PERSON.ID+n.births.non.hiv
        vSexes = sample(c(MALE,FEMALE),n.births.non.hiv,prob = c(.5,.5),replace = T) # still 50/50 male/female
        memberListNew = (mapply(PERSON$new, pop$params$TNOW,
                                vIds,
                                vSexes,
                                0,#age
                                HIV.NEG,
                                NCD.NEG)) 
        pop$addMembers(unlist(memberListNew))
        # record stats:
        pop$stats$n.births.non.hiv[pop$params$YNOW]=n.births.non.hiv
      }
      if (n.births.hiv>0){
        vIds = c((pop$params$LAST.PERSON.ID+1): (pop$params$LAST.PERSON.ID+n.births.hiv))
        pop$params$LAST.PERSON.ID=pop$params$LAST.PERSON.ID+n.births.hiv
        vSexes = sample(c(MALE,FEMALE),n.births.hiv,prob = c(.5,.5),replace = T)
        memberListNew = (mapply(PERSON$new,pop$params$TNOW,
                                vIds,
                                vSexes,
                                0,#age
                                HIV.UNDIAG,
                                NCD.NEG))  
        pop$addMembers(memberListNew)
        # record stats:
        pop$stats$n.births.hiv[pop$params$YNOW]=n.births.hiv
        nMaleNewborns=sum(vSexes==MALE)
        pop$stats$n.hiv.inc["0-4","MALE","HIV.NEG", "NCD.NEG",pop$params$YNOW]=pop$stats$n.hiv.inc["0-4","MALE","HIV.NEG","NCD.NEG",pop$params$YNOW] + nMaleNewborns
        pop$stats$n.hiv.inc["0-4","FEMALE","HIV.NEG","NCD.NEG",pop$params$YNOW]=pop$stats$n.hiv.inc["0-4","FEMALE","HIV.NEG","NCD.NEG",pop$params$YNOW] + (n.births.hiv-nMaleNewborns)
      }
      pop$stats$n.births[pop$params$YNOW]=n.births.non.hiv + n.births.hiv
    }
    
    pop$increaseMonth()
    
  
    # Model intervention???
    }
  
  #### AT YEAR's END:
  ## --MODEL Deaths due to aging out --------
  pop$model.aging.out()
  
  ## -- UPDATE NCD STATES & CVD RISKS FOR NEXT YEAR --------
  pop<-update.ncd.states(pop)
  pop<-invisible(set.cvd.risk(pop))
  ##############################################
  
  ##Model the intervention??? 
  
  # END OF YEAR----
  if (bPrint1) cat("End of year: ",pop$params$CYNOW," --------------------------- \n")
  # Record annual statistics --------
  pop$record.annual.stats()
  #Increment the clock
  pop$increaseYear()
  pop
}




# covering a certain percentage of the population or those that meet certain criteria,
# screening for NCDs with 100% sensitivity/specificity and starting people on treatment with prob "pNcdTrtInitiation"
# pNcdTrtInitiation represents a combination of factors related to acceptability, uptake, and adherence, 
# represenitng the likelihood of starting and adhereing to ncd treatment
# we are not modeling drop outs
# Dimensions
# Time: freq of interevention: one time, annual, limited number of years
# scope: community wide vs HIV clinic
# coverage: proportion receiving the intervention
# prbOfEffTrtInitiation: a combined measure of access, uptake, and adherence

# allowForReenrollment: should we allow individuals to enroll in the intervention if they have been screened and/or started trt before?
# rate of dropout: 
  # HIV: are new ART initiators subject to the same cascade? 

# so HIV intervention will run seperately in khm, we will read the input here but dont model explicit HIV intervention

model.onetime.intervention<-function(pop,scenario){
  # successful trt initiation
  pCoverage=0.5
  pNcdTrtInitiation=1
  
  
  #estiamte the number of people that will be screened
  nToScreen= round(pCoverage * length(pop$members))
  #choose a random set from the community
  popIds=sample(size = nToScreen,x = length(pop$members),replace = F )
  #loop
  invisible(lapply(popIds,function(x){
    p=pop$members[[x]]
    # print(x)
    tnow=pop$params$TNOW
    # NCD screening and treatment
    if(p$ncdState ==2){
      pop$record.diab.diag.int(p$agegroup,p$sex,p$hivState,p$ncdState)
      if(runif(1) < pNcdTrtInitiation){
        pop$record.diab.trt.int(p$agegroup,p$sex,p$hivState,p$ncdState)
        p$start.diab.trt(tnow)
      }}
    if(p$ncdState ==3){
      pop$record.hyp.diag.int(p$agegroup,p$sex,p$hivState,p$ncdState)
      if(runif(1) < pNcdTrtInitiation){
        pop$record.hyp.trt.int(p$agegroup,p$sex,p$hivState,p$ncdState)
        p$start.hyp.trt(tnow)
      }}
    if(p$ncdState ==4){
      pop$record.diab.hyp.diag.int(p$agegroup,p$sex,p$hivState,p$ncdState)
      if(runif(1) < pNcdTrtInitiation){
        pop$record.diab.hyp.trt.int(p$agegroup,p$sex,p$hivState,p$ncdState)
        p$start.diab.hyp.trt(tnow)
      }}
    
    # HIV screening and engagement
    if (p$hivState ==2){ #undiagnosed
      pop$record.hiv.diag.int(p$agegroup,p$sex,p$hivState,p$ncdState) # record new diagnosis
      # engagement?
      if(runif(1) < pHivTrtInitiation){
        pop$record.hiv.trt.int(p$agegroup,p$sex,p$hivState,p$ncdState)
        p$model.hiv.eng(tnow)
      } else {
        p$model.hiv.uneng(tnow) 
      }}
    if (p$hivState ==3){ #unengaged
      if(runif(1) < pHivTrtInitiation){
        pop$record.hiv.trt.int(p$agegroup,p$sex,p$hivState,p$ncdState)
        p$model.hiv.eng(tnow)
      }}
    
  }))
  pop
}