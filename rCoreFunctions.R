#
#  R HIVNCD 2022
#  rCoreFunctions.R class
#  
#####################################
print("Sourcing rCoreFunctions.R ... ")

# records annual statistics
record.annual.gss<-function(pop,gss){
  gss$pop.size[mc$YNOW]<<-length(pop)
  
  gss$n.hiv.prev[,,,mc$YNOW]<<- return.gss.hiv.state.sizes(pop)  #'@MS: redundant?
  
  #'@MS: the new function to save HIV & NCD states sizes by age/sex
  gss$n.state.sizes[,,,,mc$YNOW]<<-return.pop.state.size.distribution(pop)
}


#creates the initial population
print("create.initial.population")
create.initial.population <- function( n=0 # number of people if not specified as in mc
){
  cat("Generating ",n, "people in year=",mc$CYNOW,"\n")
  
  #subset the first n row to create n persons
  sim.pop = read.csv("data/stepSimPop2015.csv")
  if (n>nrow(sim.pop)) stop ("Requested size of initial population is greater than simpop data in 2015")
  sim.pop=sim.pop[1:n,]
  #set up attributes for each row
  sim.pop$age[sim.pop$age==0] = 0.5 #we need this so that the first agegroups is set to 1
  sim.pop$age[sim.pop$age>85] = 85 
  sim.pop$sex[sim.pop$sex=="MALE"] = mc$MALE 
  sim.pop$sex[sim.pop$sex=="FEMALE"] = mc$FEMALE
  sim.pop$ncd[sim.pop$hypertension==0 & sim.pop$diabetes==0] = mc$NCD.NEG #1, neither 
  sim.pop$ncd[sim.pop$hypertension==0 & sim.pop$diabetes==1] = mc$NCD.DIAB #2, diabetic
  sim.pop$ncd[sim.pop$hypertension==1 & sim.pop$diabetes==0] = mc$NCD.HYP #3, hypertensive
  sim.pop$ncd[sim.pop$hypertension==1 & sim.pop$diabetes==1] = mc$NCD.DIAB_HYP #4, both
  #vector of attributes for agents
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

# sets the initial HIV status (based on HIV state prevalences in 2015) 
print("set.initial.hiv.status")
set.initial.hiv.status = function(){
  #get hiv state proportions by age and sex
  hiv.probs = get.hiv.state.proportions(jheem.hivPrev2015)
  
  invisible(lapply(c(1:length(pop)),function(x){
    p<-pop[[x]]
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
  cat("Initial HIV status set \n")
  pop
}

# sets the annual CVD risk (from 10-year risk of cvd events)
print("set.annual.cvd.risk")
set.annual.cvd.risk = function(){
  #read pooled 10-year CVD risk data by age, sex and ncd status
  cvd.risk = pooled.risk.by.age.sex.ncd 
  younger.age.groups = c(1:8) #c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39")
  older.age.groups = c(16:17) #c("75-79","80-85")
  
  invisible(lapply(c(1:length(pop)),function(x){
    p<-pop[[x]]
    
    # for whatever age group they are in, access the 10-year risk for the previous age group 
    # this is what is used to calculate annual risk
    p.age.indicator = p$agegroup-1 
    p.age.indicator = pmax(1,p.age.indicator) # for the youngest age group, just make this 1, not 0
    
    if(p.age.indicator %in% younger.age.groups) { # if younger than 40, assume CVD risk of 40-44
      p.cvd.risk.10.year = (cvd.risk["40-44",p$sex,p$ncdState])/100
    } else if(p.age.indicator %in% older.age.groups) { # if older than 75, assume CVD risk of 70-74
      p.cvd.risk.10.year = (cvd.risk["70-74",p$sex,p$ncdState])/100
    } else if(!(p.age.indicator %in% younger.age.groups) & !(p.age.indicator %in% older.age.groups)){ # if not in youngest/oldest, use actual age
      p.age.group=mc$DIM.NAMES.AGE[p.age.indicator] # access age group name
      p.cvd.risk.10.year = (cvd.risk[p.age.group,p$sex,p$ncdState])/100
    } else stop("age group not within CVD risk calculator")
    
    # Convert from 10-year CVD risk to annual 
    p.cvd.risk.annual = -((log(1-p.cvd.risk.10.year))/10) #'@MS: did you assume that 10-year risk decade exponentially?
    p.cvd.risk.annual = p.cvd.risk.annual*100
    p$annualCvdRisk=p.cvd.risk.annual
    p$monthlyCvdRisk= return.monthly.prob(p.cvd.risk.annual) #'@MS: should we use this?
  }))
  cat("Annual CVD risks are set \n")
  pop
}

# model one simulated year
print("model.annual.dynamics")
run.one.year<-function(sim){
  pop=sim$pop
  mc=sim$mc
  
  
  #check the TNOW and break if it's not correctly set
  if ((mc$TNOW%%mc$ANNUAL.TIMESTEPS)!=0) break("TNOW is not set correctly")
  cat("Beginning the year ... ",mc$CYNOW,"\n")

  #### AT YEAR's BEGINNING:
  { #computing event probabilities from JHEEM
    ##Probability of HIV mortality (estimated via # events/ elig pop) --------
    n.hiv.pos = apply(hiv.output.for.ncd$population[as.character(mc$CYNOW-1),-1,,],c(2:3),sum) # extract all but hiv.negative, sum over hiv states
    target.hiv.mort = hiv.output.for.ncd$hiv.mortality[as.character(mc$CYNOW),,] # pull out current year; dimensions are year, age, sex
    prob.hiv.mort=target.hiv.mort/n.hiv.pos
    if(sum(prob.hiv.mort>1)>1)
      stop(paste("Error: probability of prob.hiv.mort >1 in year ",mc$CYNOW))
    prob.hiv.mort[prob.hiv.mort==Inf]<-0
    jheem.prob.hiv.mort<-prob.hiv.mort/mc$ANNUAL.TIMESTEPS
    
    ##Probability of non.HIV mortality (estimated via # events/ elig pop) --------
    n.pop = apply(hiv.output.for.ncd$population[as.character(mc$CYNOW-1),,,],c(2:3),sum) # extract all population, sum over hiv states
    target.non.hiv.mort = hiv.output.for.ncd$non.hiv.mortality[as.character(mc$CYNOW),,] # pull out current year; dimensions are year, age, sex
    prob.non.hiv.mort=target.non.hiv.mort/n.pop
    if(sum(prob.non.hiv.mort>1)>1)
      stop(paste("Error: probability of prob.non.hiv.mort >1 in year ",mc$CYNOW))
    prob.non.hiv.mort[prob.non.hiv.mort==Inf]<-0
    jheem.prob.non.hiv.mort<-prob.non.hiv.mort/mc$ANNUAL.TIMESTEPS
    
    ##Probability of incidence (estimated via # events/ elig pop) --------
    n.hiv.neg = hiv.output.for.ncd$population[as.character(mc$CYNOW-1),"HIV.NEG",,]
    target.inc = hiv.output.for.ncd$incidence[as.character(mc$CYNOW),,] # pull out current year; dimensions are year, age, sex
    prob.inc=target.inc/n.hiv.neg
    if(sum(prob.inc>1)>1)
      stop(paste("Error: probability of prob.inc >1 in year ",mc$CYNOW))
    prob.inc[prob.inc==Inf]<-0
    jheem.prob.hiv.inc<-prob.inc/mc$ANNUAL.TIMESTEPS
    
    ##Probability of engagement (direct input to HIV model) --------
    prob.eng=target.parameters$prob.eng[as.character(mc$CYNOW),,]
    if(sum(prob.eng>1)>1)     
      stop(paste("Error: probability of engagement >1 in year ",mc$CYNOW))
    prob.eng[prob.eng==Inf]<-0
    jheem.prob.hiv.eng<-prob.eng/mc$ANNUAL.TIMESTEPS
    
    ##Probability of disengagement (direct input to HIV model) --------
    prob.diseng=target.parameters$prob.diseng[as.character(mc$CYNOW),,]
    if(sum(prob.diseng>1)>1)     
      stop(paste("Error: probability of disengagement >1 in year ",mc$CYNOW))
    prob.diseng[prob.diseng==Inf]<-0
    jheem.prob.hiv.diseng<-prob.diseng/mc$ANNUAL.TIMESTEPS
    
    ##Probability of diagnosis (direct input to HIV model) --------
    prob.diag = target.parameters$prob.diag[as.character(mc$CYNOW),,]
    if(sum(prob.diag>1)>1)     
      stop(paste("Error: probability of prob.diag >1 in year ",mc$CYNOW))
    prob.diag[prob.diag==Inf]<-0
    jheem.prob.hiv.diag<-prob.diag/mc$ANNUAL.TIMESTEPS
    
  }
  
  ##### AT EACH TIMESTEP WITHIN THE YEAR:
  for(i in (1:mc$ANNUAL.TIMESTEPS)){
    mc$TNOW=mc$TNOW+1
    n=length(pop)
    # CVD events and HIV transisions are independant, so the order doesnt matter
    # we model HIV deaths based on new HIV states after new transitions are modeled
    
    # 1- modeling cvd events based on current cvd annual risk
    # counting new events: marking those who will die after the event
    model.cvd.events()
    
    # 2- modeling HIV transitions based on jheem outputs
    model.hiv.transitions(jheem.prob.hiv.inc, 
                          jheem.prob.hiv.eng, 
                          jheem.prob.hiv.diseng, 
                          jheem.prob.hiv.diag)
    
    # 3- modeling HIV and CVD deaths
    #prob of deaths for eveyone/killing those who are marked 
    model.hiv.cvd.deaths( jheem.prob.hiv.mort,
                          jheem.prob.non.hiv.mort)
    
    cat("End of timestep: ",mc$TNOW," ---")
    # cat("Total incidence= ",n.inc," diag= ",n.diag," eng= ",n.eng," uneng= ",n.uneng)
    # cat("Hiv.deaths=",n.deaths.hiv,"gen.deaths=",n.deaths.gen,"\n")
    cat("current pop size is ",length(pop),"\n")
  }
  

  #### AT YEAR's END:
  ## 1-MODEL REMAINING DEATHS --------
  n.ageout=sum(unlist(invisible(lapply(pop,function(x){
    if(x$age>=mc$MAX.AGE) {
      x$bMarkedDead.ageout=T;
      return(1)
    }}))))
  # balancing deaths
  {
    n<-length(pop)
    death.status=unlist(invisible(lapply(c(1:n),function(x){
      return(pop[[x]]$bMarkedDead.ageout)
    })))
    n.deaths.ageout<-sum(death.status)
    gss$n.deaths.ageout[mc$YNOW]=n.deaths.ageout
    pop<-pop[!death.status]   #only keep those who are alive #'@JP: should we delete people directly?
  }
  
  ## 2-MODEL BIRTHS --------
  { # non-HIV births
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
  }
  
  ## 3- MODEL AGING --------
  invisible(lapply(pop,function(x){x$incAge}))
  
  ## 4- UPDATE NCD STATES & CVD RISKS FOR NEXT YEAR --------
  update.ncd.states()
  set.annual.cvd.risk()
  

    ##############################################
  # END OF YEAR----
  cat("Final pop size is ",length(pop),"\n")
  print(paste("End of year: ",mc$CYNOW," ---------------------------"))
  
  
  # Record annual statatistics --------
  record.annual.gss(pop)
  
  #'@MS: for now, we let the TNOW increase over time and use it to record the event times for agents
  mc$YNOW<-mc$YNOW+1
  mc$CYNOW<-mc$CYNOW+1
  
  invisible(list(pop=pop,
                 mc=mc))
}

# current age at year's beginning:
# Jan 1st to Dec 31st:

# each months:
## changes in HIV states from jheem
## new cvd events based on current risks
## deaths from jheem (HIV and age-specific deaths) > these include some CVD deaths too
## deaths from CVD events

# end of year:
# deaths due to aging
# remaining deaths to balance with jheem population
# new births 

# aging >>  
# changes in NCD states
# updating cvd annual risks based on new age and ncd state



model.hiv.transitions<-function(prob.inc,
                                prob.eng,
                                prob.diseng,
                                prob.diag){
  
  n=length(pop)
  { invisible(lapply(c(1:n),function(x){
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
          }else         {
            browser()
            stop(paste("Error: Person ",x," hivState is ",p$hivState," and it didnt meet anuy criteria"))
          }}}}}))
  }
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
  }
  
}


model.hiv.cvd.deaths<-function(prob.hiv.mort,
                               prob.non.hiv.mort){
  
  #evaluate prob of hiv and non.hiv mortality for everyone who is not already marked dead
  invisible(lapply(c(1:n),function(x){
    p=pop[[x]] 
    if (p$bMarkedDead.cvd==FALSE){
      # HIV MORTALITY
      if(p$hivState!=mc$HIV.NEG){ # all HIV positive
        p.prob = prob.hiv.mort[p$agegroup,p$sex]
        if(runif(1)<p.prob)
          p$bMarkedDead.hiv=T
      }
      # NON.HIV MORTALITY 
      p.prob = prob.non.hiv.mort[p$agegroup,p$sex]
      if(runif(1)<p.prob)
        p$bMarkedDead.gen=T
    }}))
  
  # modeling deaths & saving gss
  {
    # cvd deaths
    n=length(pop)
    death.status=unlist(invisible(lapply(c(1:n),function(x){return(pop[[x]]$bMarkedDead.cvd)  })))
    n.deaths.cvd<-sum(death.status)
    gss$n.deaths.cvd[mc$YNOW]=n.deaths.cvd
    pop<-pop[!death.status] #only keep those who are alive
    
    # hiv deaths
    n=length(pop)
    death.status=unlist(invisible(lapply(c(1:n),function(x){return(pop[[x]]$bMarkedDead.hiv)  })))
    n.deaths.hiv<-sum(death.status)
    gss$n.deaths.hiv[mc$YNOW]=n.deaths.hiv
    pop<-pop[!death.status] #only keep those who are alive
    
    # non.hiv deaths
    n=length(pop)
    death.status=unlist(invisible(lapply(c(1:n),function(x){return(pop[[x]]$bMarkedDead.non.hiv)  })))
    n.deaths.non.hiv<-sum(death.status)
    gss$n.deaths.non.hiv[mc$YNOW]=n.deaths.non.hiv
    pop<-pop[!death.status] #only keep those who are alive
  }
}


#'@MS: to be completed

# new function to update NCD state after aging
update.ncd.states<-function(){ 
  # for each age/sex subgroup:
  #   compute current prp of ncd states, compare to baseline proportions, and estimate the difference
  # if diference > 0, assign individuals to new ncd states with corresponding probabilities 
  # states: S, D, H, DH
  # only allowing one NCD incidence at a time (H>DH, D>DH and not S>>DH)
  # 1- modeling moves from H >> DH or D>> DH
  # 2- modeling moves from S >> D
  # 3- modeling moves from S >>H

# master.ncd.distrubution.by.age.sex >>> to extract from 2015 step dataset  
#   can be included as an input to the model.hiv.cvd.deaths
#   
#   Baseline: we assume similar ncd prevalences for HIV+/-, and keep the prevalence fix over time
#   SA:  assuming differential ncd prevalences based on hiv status
#   SA: assuming an increase in ncd prevalences over time
  
  }

#'@MS: new function to mdeol cvd events
#'need more data on which CVD events are modeled
model.cvd.events<-function(){
  # a.	First event: WHO calculator 
  # # b.	Repeat events: 2x risk of first event (Smith-Spangler assumption) 
  # invisible(lapply(c(1:length(pop)),function(x){
  #   p<-pop[[x]]
  #   
  #   # person has no history of previous cvd events? 
  #   # { evaluate prob of first CVD event
  #     # if true: mark the cvd >>> evaluate prob of death following that event?
  #     # if true: mark dead
  #   # }
  #   # else{
  #   # evaluare prob of subsequent CVD events & death
  #   # }
  #   # model cvd events that are marked & count the incidence 
  #   
  # }))
}


