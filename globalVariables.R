#
#  R HIVNCD 2022
#  GlobalVariables.R
#  
#####################################
print("Sourcing GlobalVariables.R ... ")
pop<-NULL

mc <- list(
  #constants
  INITIAL.YEAR=2015,
  END.YEAR=2030,
  AGE.INTERVAL=5,
  MIN.AGE=0,
  MAX.AGE=85,
  POP.SIZE=10000,
  #
  #variables
  YNOW=1, #variable showing current year
  CYNOW=2015, #calendar year
  TNOW=0, #current timestep
  
  ANNUAL.TIMESTEPS=12, #how many timepsteps in a year?
  lastID=0, #global variable to keep track of person's id
  #
  FEMALE=1,
  MALE=2,
  #
  HIV.NEG=1,
  HIV.UNDIAG=2, #undiagnosed
  HIV.UNENG=3,  #diagnosed but not on trt 
  HIV.ENG=4,   #on trt & suppressed
  #
  NCD.NEG=1, #no diabetes or hypertension
  NCD.DIAB=2, #diabetic
  NCD.HYP=3, #hypertensive
  NCD.DIAB_HYP=4, #both
  #
  DEATH.NATURAL=1,
  DEATH.HIV=2,
  DEATH.STROKE=3,
  DEATH.MI=4,
  #
  NCDTRT.NONE=1,
  NCDTRT.HYP=2,
  NCDTRT.DIAB=3,
  NCDTRT.HYPDIAB=4,
  #
  CVD.NONE=1,
  CVD.MI=2,
  CVD.STROKE=3,

  DIM.SEX=2,
  DIM.AGE=17,
  DIM.HIV=4,
  DIM.NCD=4,
  DIM.NAMES.SEX=c("FEMALE","MALE"),
  DIM.NAMES.AGE=c("0-4","5-9","10-14","15-19", "20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59",
                      "60-64","65-69","70-74","75-79","80-85"),
  DIM.NAMES.HIV=c("HIV.NEG","HIV.UNDIAG","HIV.UNENG", "HIV.ENG"),
  DIM.NAMES.NCD=c("NCD.NEG","NCD.DIAB","NCD.HYP","NCD.DIAB_HYP")
)

{
#load hiv sim workspace with: (1) sim object, (2) data manager, (3) extracted data (khm)
load('data/hiv_sim.RData') 
  
load("data/hiv_simset.RData")
class(khm) = "khm_simulation_output"

#distribution of HIV states for each age/sex category
khm.hivPrev2015 = khm[[1]]$population["2015",,,]

#load pooled CVD risk by age/sex/ncd category
load('data/10.year.cvd.risk.by.age.sex.ncd.Rdata')

#load STEP dataset to generate the initial population by age, sex and ncd state
step.dataset = read.csv("data/stepSimPop2015.csv")

#@MS: I added the step dataset here so that all external files are linked from the same place
step.dataset$agegroup=ceiling((step.dataset$age+1)/mc$AGE.INTERVAL)

#@MS:I wrote the code below to extract the initial NCD prevalences from the STEP survey. 
# I then saved it externally so that we read it into the model everytime. you can remove this after reviewing it
# #add agegroups
# step.dataset$agegroup[step.dataset$agegroup<1]<-1
# step.dataset$agegroup[step.dataset$agegroup>mc$DIM.AGE]<-mc$DIM.AGE
# #add ncd state
# step.dataset$ncdstate=step.dataset$hypertension+2*step.dataset$diabetes+1
# #loop through and count the state sizes
# ncd.state.sizes<-array(0,
#                        dim=c(mc$DIM.AGE,mc$DIM.SEX,mc$DIM.NCD),
#                        dimnames = list(mc$DIM.NAMES.AGE,mc$DIM.NAMES.SEX,mc$DIM.NAMES.NCD))
# invisible(lapply(1:nrow(step.dataset),function(x){
#   p<-step.dataset[x,]
#   ncd.state.sizes[p$agegroup,
#                   p$sex,
#                   p$ncdstate] <<- ncd.state.sizes[   p$agegroup,
#                                                      p$sex,
#                                                      p$ncdstate] +1
#   }))
# write.csv(ncd.state.sizes,file = "ncd.state.sizes.2015.csv")

# read target NCD sizes and compute the target proportions based on 2015 step dataset
D<-read.csv("ncd.state.sizes.2015.csv",header = T)[,2:9]
target.ncd.sizes<-array(0,
                       dim=c(mc$DIM.AGE,mc$DIM.SEX,mc$DIM.NCD),
                       dimnames = list(mc$DIM.NAMES.AGE,mc$DIM.NAMES.SEX,mc$DIM.NAMES.NCD))
invisible(lapply(1:4,function(i){
  target.ncd.sizes[,,i]<<-array(unlist(D[,(i*2-1):(i*2)]),dim = c(mc$DIM.AGE,mc$DIM.SEX) )}))
target.ncd.sizes
#proportions 
target.ncd.props<-target.ncd.sizes
invisible(sapply(1:length(mc$DIM.NAMES.SEX), function(sex){
  sapply(1:length(mc$DIM.NAMES.AGE), function(age){
    target.ncd.props[age,sex,]<<-target.ncd.props[age,sex,]/sum(target.ncd.sizes[age,sex,]) # double assignment goes back to the most recent value of D in the upper environment
  })}))
target.ncd.props[is.na(target.ncd.props)]<-0

cat("Input data loaded \n")
}