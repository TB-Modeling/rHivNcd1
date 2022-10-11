#
#  R HIVNCD 2022
#  Variable definitions
#  
#####################################
print("Reading global variables... ")







#Set the start time to the initial year of model
TICK <- INITIAL.YEAR
lastID<-0 #global variable to keep track of person's id

mc<-list(
  NUM.SEXES=2,
  NUM.AGE.GROUPS=17,
  NUM.HIV.STATES=4,
  NUM.NCD.STATES=4,
  
  INITIAL.YEAR=2015,
  END.YEAR=2050,
  AGE.INTERVAL=5,
  MIN.AGE=0,
  MAX.AGE=85,
  
  
  MALE=1,
  FEMALE=2,
  
  HIV.NEG=1,
  HIV.UNDIAG=2, #undiagnosed
  HIV.UNENG=3,  #diagnosed but not on trt 
  HIV.SUPP=4,   #on trt & suppressed
  
  NCD.NEG=1, #no diabetes or hypertension
  NCD.DIAB=2, #diabetic
  NCD.HYP=3, #hypertensive
  NCD.DIAB_HYP=4, #both
  
  DEATH.NATURAL=1,
  DEATH.HIV=2,
  DEATH.STROKE=3,
  DEATH.MI=4,
  
  NCDTRT.NONE=1,
  NCDTRT.HYP=2,
  NCDTRT.DIAB=3,
  NCDTRT.HYPDIAB=4,
  
  CVD.NONE=1,
  CVD.MI=2,
  CVD.STROKE=3,

  # hiv.model.status = c("hiv_negative","undiagnosed","diagnosed_unengaged","engaged_unsuppressed","engaged_suppressed"),
  
  DIM.NAMES.SEX=c("FEMALE","MALE"),
  DIM.NAME.AGEGROUP=c("0-4","5-9","10-14","15-19", "20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59",
                      "60-64","65-69","70-74","75-79","80-85"),
  DIM.NAMES.HIV=c("HIV.NEG","HIV.UNDIAG","HIV.UNENG", "HIV.SUPP"),
  DIM.NAMES.NCD=c("NCD.NEG","NCD.DIAB","NCD.HYP","NCD.DIAB_HYP")
)
