#
#  R HIVNCD 2022
#  GlobalEnvironment.R
#  
#####################################
print("Sourcing GlobalEnvironment.R ... ")
############################################################################################################################################
# GLOBAL PARAMETERS ARE CONSTANT. THEY ARE VISIBLE TO ALL CLASSES AND FUNCTIONS AND DONT CHANGE
cat("Setting up global parameters .... \n")
ANNUAL.TIMESTEPS=12 #how many timepsteps in a year?
INITIAL.YEAR=2014 #simulation starts
# INT.START.YEAR=2025 #intervention starts
# INT.END.YEAR=2030 #intervention ends
END.YEAR=2030 #simulation ends
#
AGE.INTERVAL=5
MIN.AGE=0
MAX.AGE=85*12
POP.SIZE=100000
#
FEMALE=1
MALE=2
#
HIV.NEG=1
HIV.UNDIAG=2 #undiagnosed
HIV.UNENG=3  #diagnosed but not on trt 
HIV.ENG=4   #on trt & suppressed
#
NCD.NEG=1 #no diabetes or hypertension
NCD.DIAB=2 #diabetic
NCD.HYP=3 #hypertensive
NCD.DIAB_HYP=4 #
NCD.DIAB.TRT=5 #diabetic on treatment
NCD.HYP.TRT=6 #hypertensive on treatment
NCD.DIAB_HYP.TRT=7 #diab&hyp on treatment
#
DEATH.NATURAL=1
DEATH.HIV=2
DEATH.STROKE=3
DEATH.MI=4
#
NCDTRT.NONE=1
NCDTRT.DIAB=2
NCDTRT.HYP=3
NCDTRT.DIABHYP=4

DIM.NAMES.SEX=c("FEMALE","MALE")
DIM.NAMES.AGE=c("0-4","5-9","10-14","15-19", "20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59",
                "60-64","65-69","70-74","75-79","80-85")
DIM.NAMES.HIV=c("HIV.NEG","HIV.UNDIAG","HIV.UNENG", "HIV.ENG")
DIM.NAMES.NCD=c("NCD.NEG","NCD.DIAB","NCD.HYP","NCD.DIAB_HYP","NCD.DIAB.TRT","NCD.HYP.TRT","NCD.DIAB_HYP.TRT")
DIM.NAMES.YEAR=c(INITIAL.YEAR:END.YEAR)

DIM.SEX=length(DIM.NAMES.SEX)
DIM.AGE=length(DIM.NAMES.AGE)
DIM.HIV=length(DIM.NAMES.HIV)
DIM.NCD=length(DIM.NAMES.NCD)
DIM.YEAR=length(DIM.NAMES.YEAR)
################################################################################################################
# MODEL PARAMETERS (MP) HOUSES ALL PARAMETERS THAT MAY BE CHANGED IN SENSITIVITY ANALYSIS. THEY'RE CREATED ONCE FOR EACH POPULATION
cat("loading function generate.new.modelParameter ... \n")
generate.new.modelParameter<-function(){
  #variables
  MP<-list(
    TNOW=1, #current timestep
    YNOW=1, #variable showing current year
    CYNOW=INITIAL.YEAR, #calendar year (we start one year earlier, so that we save the initial population state before simulation begins)
    LAST.PERSON.ID=0)
  
  #1- load HIV data 
  # load('data/hiv_sim.RData')  #a single run from the HIV model
  load("data/hiv_simset.RData") #multiple runs from the HIV model
  MP$khm.full=khm # leaving full simset in here for plotting purposes
  class(MP$khm.full) = "khm_simulation_output"
  x=sample(1:length(khm),1)
  print(paste("KHM model ",x," was sampled"))
  MP$khm.id=x #khm id that was sampled for this run
  khm = khm[[x]]# randomly sample one hiv sim from the length of n.hiv.sims
  khm.hivPrev2015 = khm$population["2015",,,]
  MP$khm=khm
  MP$khm.hivPrev2015=khm.hivPrev2015
  #
  # Making sure the KHM timeline agrees with the NCD model:
  if(!as.numeric(unlist(dimnames(khm$incidence)[1])[[1]])==INITIAL.YEAR)
    stop("Error: KHM starting year is different from NCD model")
  
  n=length(unlist(dimnames(khm$incidence)[1]))
  if(as.numeric(unlist(dimnames(khm$incidence)[1])[[n]])< END.YEAR)
    stop(paste0("Error: KHM END year (",as.numeric(unlist(dimnames(khm$incidence)[1])[[n]]),") is smaller than NCD model (",END.YEAR,")"))
  
    
  
  #2- load STEP dataset to generate the initial population by age, sex and ncd state
  step.dataset = read.csv("data/stepSimPop2015.csv")
  step.dataset$agegroup=ceiling((step.dataset$age+1)/AGE.INTERVAL)
  MP$step.dataset=step.dataset
  
  #3- read target NCD sizes and compute the target proportions based on 2015 step dataset
  D<-read.csv("data/ncd.state.sizes.2015.csv",header = T)[,2:9]
  target.ncd.sizes<-array(0,
                          dim=c(DIM.AGE,DIM.SEX,DIM.NCD),
                          dimnames = list(DIM.NAMES.AGE,DIM.NAMES.SEX,DIM.NAMES.NCD))
  invisible(lapply(1:4,function(i){
    target.ncd.sizes[,,i]<<-array(unlist(D[,(i*2-1):(i*2)]),dim = c(DIM.AGE,DIM.SEX) )}))
  MP$target.ncd.sizes= target.ncd.sizes
  #proportions 
  target.ncd.props<-target.ncd.sizes
  invisible(sapply(1:length(DIM.NAMES.SEX), function(sex){
    sapply(1:length(DIM.NAMES.AGE), function(age){
      target.ncd.props[age,sex,]<<-target.ncd.props[age,sex,]/sum(target.ncd.sizes[age,sex,]) # double assignment goes back to the most recent value of D in the upper environment
    })}))
  target.ncd.props[is.na(target.ncd.props)]<-0
  MP$target.ncd.props=target.ncd.props
  
  #relative risk of ncd incidence by hiv status (relative hiv negative)
  MP$relative.ncd.risk.by.hiv=1 
  #annual growth in age/sex-specific prev of ncds relative to baseline
  MP$annaul.growth.ncd.prev=1
  
  #4-load pooled 10-year CVD risk by age/sex/ncd category
  load('data/10.year.cvd.risk.by.age.sex.ncd.Rdata')
  q=pooled.risk.by.age.sex.ncd
  # dimnames(q)
  x=array(0,dim = c(DIM.AGE,DIM.SEX,DIM.NCD),dimnames = list(DIM.NAMES.AGE,DIM.NAMES.SEX,DIM.NAMES.NCD))
  # dimnames(x)
  x[unlist(dimnames(q)[1]),unlist(dimnames(q)[2]),unlist(dimnames(q)[3])]<-q
  #applying values for 40-44 to younger agegroups and from 70-74 to older agegroups
  MP$cvd.risk.multiplier.15.to.29 = 1/50 
  MP$cvd.risk.multiplier.30.to.39 = 1/6 
  for(i in 1:3) x[c(DIM.NAMES.AGE[i]),,]=0 # no risk below 15 
  for(i in 4:6) x[c(DIM.NAMES.AGE[i]),,]=x["40-44",,]*MP$cvd.risk.multiplier.15.to.29 # 15-19, 20-24, 25-29
  for(i in 7:8) x[c(DIM.NAMES.AGE[i]),,]=x["40-44",,]*MP$cvd.risk.multiplier.30.to.39 # 30-34, 35-39
  for(i in 16:17) x[c(DIM.NAMES.AGE[i]),,]=x["70-74",,]
  
  # annual risk computed from an exponential decay
  annual.cvd.risk.by.age.sex=-((log(1- x/100 ))/10) # not included in MP since we only need monthly values 
  #assuming geometric distribution of risk over time
  MP$monthly.cvd.risk.by.age.sex=(1-(1-annual.cvd.risk.by.age.sex)^(1/12))
  
  # risk of recurrent event here to 2x original risk; able to change in sensitivity analysis
  MP$recurrent.cvd.event.risk.multiplier=2
  #probability that the first CVD event is mi (vs stroke)
  MP$prob.first.cvd.event.mi.male = 0.6 
  MP$prob.first.cvd.event.mi.female = 0.6 
  
  #5-load CVD mortality data
  load("data/monthly.stroke.mortality.Rdata")
  load("data/monthly.mi.mortality.Rdata")
  MP$first.stroke.monthly.mortality = stroke.monthly.mortality #first time stroke mortality
  MP$first.mi.monthly.mortality = mi.monthly.mortality
  #recurrent events
  recur.stroke.mort.OR.multiplier=2.53 # this is an ODDS RATIO (relative to current probability), so have to convert to odds and then back to probability (in returnCvdMortality function)
  #adjusted OR:
  x=stroke.monthly.mortality/(1-stroke.monthly.mortality) * recur.stroke.mort.OR.multiplier
  MP$rec.stroke.monthly.mortality= x/ (1+x) #back to prob
  
  recur.mi.mortality.multiplier=1.856 
  MP$rec.mi.monthly.mortality= mi.monthly.mortality * recur.mi.mortality.multiplier
  
  
  
  
  
  
  return(MP)
}

################################################################################################################
cat("loading function generate.new.stat ... \n")
generate.new.stat<-function(){
  #global statistics
  DIM.N=END.YEAR-INITIAL.YEAR+2
  DIM.NAMES.N=c(INITIAL.YEAR:(END.YEAR+1))
  
  #temporary empty arrays to initialize stats
  #1D
  v1temp=rep(0,DIM.N,
             dim=DIM.N,
             dimnames = list(year=DIM.NAMES.N))
  
  
  #5D
  v5temp=array(rep(0,DIM.AGE*DIM.SEX*DIM.HIV*DIM.NCD*DIM.N),  
               dim = c(DIM.AGE,
                       DIM.SEX,
                       DIM.HIV,
                       DIM.NCD,
                       DIM.N),
               dimnames=list(age = DIM.NAMES.AGE,
                             sex = DIM.NAMES.SEX,
                             hiv.status = DIM.NAMES.HIV,
                             ncd.status = DIM.NAMES.NCD,
                             year = DIM.NAMES.N))
  
  
  stats<-list(
    ncd.id=0,
    
    #1D arrays for entire population over time
    pop.size=v1temp,
    n.births=v1temp,
    n.births.non.hiv=v1temp,
    n.births.hiv=v1temp,
  
    

    #5D arrays [age, sex, hiv, ncd, year]
    # counting events modeled (incidence, getting diagnosed, etc)
    n.hiv.inc=v5temp, 
    n.hiv.diag=v5temp, 
    n.hiv.eng=v5temp, 
    n.hiv.uneng=v5temp, 
    
    # ncd incidence
    n.diab.hyp.inc=v5temp,
    n.diab.inc=v5temp,
    n.hyp.inc=v5temp,
    
    #cvd incidence
    n.mi.inc=v5temp,
    n.stroke.inc=v5temp,
    
    ########### intervention stats ##########
    # hiv additional diagnosis
    n.hiv.diag.int=v5temp,
    # hiv additional treatment
    n.hiv.trt.int=v5temp,
    
    # ncd new diagnosis
    n.diab.diag.int=v5temp,
    n.hyp.diag.int=v5temp,
    n.diab.hyp.diag.int=v5temp,
    
    # ncd treatment initiation
    n.diab.trt.int=v5temp,
    n.hyp..trt.int=v5temp,
    n.diab.hyp..trt.int=v5temp,
    
    
    ## STATE SIZES ##
    n.state.sizes=v5temp,
    
    n.deaths.hiv=v5temp,
    n.deaths.cvd=v5temp,
    n.deaths.ageout=v5temp,
    n.deaths.non.hiv=v5temp
  )
  return(stats)
}



# Code to extract the initial NCD prevalences from the STEP survey
# Saved externally; read into the model everytime (so we no longer need this code; but keeping for now)
# #add agegroups
# step.dataset$agegroup[step.dataset$agegroup<1]<-1
# step.dataset$agegroup[step.dataset$agegroup>DIM.AGE]<-GP$DIM.AGE
# #add ncd state
# step.dataset$ncdstate=step.dataset$diabetes + 2*step.dataset$hypertension + 1 
# #loop through and count the state sizes
# ncd.state.sizes<-array(0,
#                        dim=c(GP$DIM.AGE,GP$DIM.SEX,GP$DIM.NCD),
#                        dimnames = list(GP$DIM.NAMES.AGE,GP$DIM.NAMES.SEX,GP$DIM.NAMES.NCD))
# invisible(lapply(1:nrow(step.dataset),function(x){
#   p<-step.dataset[x,]
#   ncd.state.sizes[p$agegroup,
#                   p$sex,
#                   p$ncdstate] <<- ncd.state.sizes[   p$agegroup,
#                                                      p$sex,
#                                                      p$ncdstate] +1
#   }))
# write.csv(ncd.state.sizes,file = "ncd.state.sizes.2015.csv")






