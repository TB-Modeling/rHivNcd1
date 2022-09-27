#
#  R HIVNCD 2022
#  Variable definitions
#  
#####################################
print("Reading global variables... ")

NUM_HIV_STATES=4
NUM_NCD_STATES=4
NUM_AGE_GROUPS=18

INITIAL.YEAR=2022
END.YEAR=2050
AGE.INTERVAL=5
MIN.AGE=0
MAX.AGE=85

DIM.NAMES.HIV=c("HIV.NEG","HIV.UNDIAG","HIV.DIAG_UNENG", "HIV.ENG")
DIM.NAMES.NCD=c("NCD.NEG","NCD.DIAB","NCD.HYP","NCD.DIAB_HYP")

#Set the start time to the initial year of model
TICK <- INITIAL.YEAR
lastID<-0 #global variable to keep track of person's id

mc<-list(
  MALE=0,
  FEMALE=1,
  
  HIV.NEG=0,
  HIV.UNDIAG=1, #undiagnosed
  HIV.DIAG_UNENG=2, #diagnosed but not on trt 
  HIV.ENG=3, #on trt
  
  NCD.NEG=0, #no diabetes or hypertension
  NCD.DIAB=1, #diabetic
  NCD.HYP=2, #hypertensive
  NCD.DIAB_HYP=3, #both
  
  DEATH.NATURAL=0,
  DEATH.HIV=1,
  DEATH.STROKE=2,
  DEATH.MI=3,
  
  NCDTRT.NONE=0,
  NCDTRT.HYP=1,
  NCDTRT.DIAB=2,
  NCDTRT.HYPDIAB=3,
  
  CVD.NONE=0,
  CVD.MI=1,
  CVD.STROKE=2
)