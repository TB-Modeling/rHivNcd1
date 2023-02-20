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
INITIAL.YEAR=2014
END.YEAR=2060
#
AGE.INTERVAL=5
MIN.AGE=0
MAX.AGE=85
POP.SIZE=10000
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
NCD.DIAB_HYP=4 #both
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
#
CVD.NONE=1
CVD.MI=2
CVD.STROKE=3

DIM.SEX=2
DIM.AGE=17
DIM.HIV=4
DIM.NCD=4
DIM.YEAR=END.YEAR-INITIAL.YEAR+1

DIM.NAMES.SEX=c("FEMALE","MALE")
DIM.NAMES.AGE=c("0-4","5-9","10-14","15-19", "20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59",
                "60-64","65-69","70-74","75-79","80-85")
DIM.NAMES.HIV=c("HIV.NEG","HIV.UNDIAG","HIV.UNENG", "HIV.ENG")
DIM.NAMES.NCD=c("NCD.NEG","NCD.DIAB","NCD.HYP","NCD.DIAB_HYP")
DIM.NAMES.YEAR=c(INITIAL.YEAR:END.YEAR)
################################################################################################################
# MODEL PARAMETERS (MP) HOUSES ALL PARAMETERS THAT MAY BE CHANGED IN SENSITIVITY ANALYSIS. THEY'RE CREATED ONCE FOR EACH POPULATION
cat("loading function generate.new.modelParameter ... \n")
generate.new.modelParameter<-function(){
  #variables
  MP<-list(
    TNOW=1, #current timestep
    YNOW=1, #variable showing current year
    CYNOW=INITIAL.YEAR, #calendar year (we start one year earlier, so that we save the initial population state before simulation begins)
    LAST.ID=0)
  
  #1- load HIV data 
  # load('data/hiv_sim.RData')  #a single run from the HIV model
  load("data/hiv_simset.RData") #multiple runs from the HIV model
  khm.hivPrev2015 = khm[[1]]$population["2015",,,]
  MP$khm=khm
  MP$khm.hivPrev2015=khm.hivPrev2015
  class(MP$khm) = "khm_simulation_output"
  
  #2- load STEP dataset to generate the initial population by age, sex and ncd state
  step.dataset = read.csv("data/stepSimPop2015.csv")
  step.dataset$agegroup=ceiling((step.dataset$age+1)/AGE.INTERVAL)
  MP$step.dataset=step.dataset
  
  #3- read target NCD sizes and compute the target proportions based on 2015 step dataset
  D<-read.csv("ncd.state.sizes.2015.csv",header = T)[,2:9]
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
  
  #4-load pooled CVD risk by age/sex/ncd category
  load('data/10.year.cvd.risk.by.age.sex.ncd.Rdata')
  MP$pooled.risk.by.age.sex.ncd=pooled.risk.by.age.sex.ncd
  #'@PK - setting risk of recurrent event here to 2x original risk; able to change in sensitivity analysis
  MP$recurrent.event.risk.multiplier=2 
  
  
  #5-load CVD mortality data
  load("data/monthly.stroke.mortality.Rdata")
  load("data/monthly.mi.mortality.Rdata")
  MP$stroke.monthly.mortality = stroke.monthly.mortality
  MP$mi.monthly.mortality = mi.monthly.mortality
  
  
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
  #3D
  v3temp=array(rep(0,DIM.AGE*DIM.SEX*DIM.N),  
               dim = c(DIM.AGE,
                       DIM.SEX,
                       DIM.N),
               dimnames=list(age = DIM.NAMES.AGE,
                             sex = DIM.NAMES.SEX,
                             year = DIM.NAMES.N))
  #4D
  v4temp=array(rep(0,DIM.AGE*DIM.SEX*DIM.HIV*DIM.N),  
               dim = c(DIM.AGE,
                       DIM.SEX,
                       DIM.HIV,
                       DIM.N),
               dimnames=list(age = DIM.NAMES.AGE,
                             sex = DIM.NAMES.SEX,
                             hiv.status = DIM.NAMES.HIV,
                             year = DIM.NAMES.N))
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
    #1D arrays for entire population over time
    pop.size=v1temp,
    n.births=v1temp,
    n.births.non.hiv=v1temp,
    n.births.hiv=v1temp,
    n.deaths.hiv=v1temp,
    n.deaths.cvd=v1temp,
    n.deaths.ageout=v1temp,
    n.deaths.non.hiv=v1temp,
    
    #3D arrays by age, sex over time
    n.hiv.inc=v3temp,#'@MS: redundant?
    n.hiv.diag=v3temp,#'@MS: redundant?
    n.hiv.eng=v3temp,#'@MS: redundant?
    n.hiv.uneng=v3temp,#'@MS: redundant?
    
    # 4D arrays by age, sex and hiv state over time 
    #'@MS: should we use the new n.state.sizes instead of this one to extract both HIV and NCD outputs?
    #'@MS: redundant?
    n.hiv.prev=v4temp,
    
    n.diab.hyp.inc=v4temp,
    n.diab.inc=v4temp,
    n.hyp.inc=v4temp,
    
    # 5D arrays by age, sex, ncd & hiv state over time
    n.state.sizes=v5temp
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