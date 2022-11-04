#
#  R HIVNCD 2022
#  Variable definitions
#  
#####################################
print("Reading global variables... ")


mc<-list(
  #constants
  INITIAL.YEAR=2015,
  END.YEAR=2050,
  AGE.INTERVAL=5,
  MIN.AGE=0,
  MAX.AGE=85,
  POP.SIZE=1000,
  #
  #variables
  YNOW=0, #variable showing current year
  CYNOW=2015, #calendar year
  TNOW=0, #current timestep
  
  ANNUAL.TIMESTEPS=12, #how many timepsteps in a year?
  lastID=0, #global variable to keep track of person's id
  #
  MALE=1,
  FEMALE=2,
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
  DIM.NAMES.SEX=c("MALE","FEMALE"),
  DIM.NAMES.AGE=c("0-4","5-9","10-14","15-19", "20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59",
                      "60-64","65-69","70-74","75-79","80-85"),
  DIM.NAMES.HIV=c("HIV.NEG","HIV.UNDIAG","HIV.UNENG", "HIV.ENG"),
  DIM.NAMES.NCD=c("NCD.NEG","NCD.DIAB","NCD.HYP","NCD.DIAB_HYP")
)
