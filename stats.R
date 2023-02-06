#
#  R HIVNCD 2022
#  Stat.R
#  
#####################################
print("Sourcing statistics ... ")

#' generate.new.stat<-function(){
#'   #global statistics
#'   DIM.N=GP$END.YEAR-GP$INITIAL.YEAR+2
#'   DIM.NAMES.N=c(GP$INITIAL.YEAR:(GP$END.YEAR+1))
#'   
#'   #temporary empty arrays to initialize stats
#'   #1D
#'   v1temp=rep(0,DIM.N,
#'              dim=DIM.N,
#'              dimnames = list(year=DIM.NAMES.N))
#'   #3D
#'   v3temp=array(rep(0,GP$DIM.AGE*GP$DIM.SEX*DIM.N),  
#'                dim = c(GP$DIM.AGE,
#'                        GP$DIM.SEX,
#'                        DIM.N),
#'                dimnames=list(age = GP$DIM.NAMES.AGE,
#'                              sex = GP$DIM.NAMES.SEX,
#'                              year = DIM.NAMES.N))
#'   #4D
#'   v4temp=array(rep(0,GP$DIM.AGE*GP$DIM.SEX*GP$DIM.HIV*DIM.N),  
#'                dim = c(GP$DIM.AGE,
#'                        GP$DIM.SEX,
#'                        GP$DIM.HIV,
#'                        DIM.N),
#'                dimnames=list(age = GP$DIM.NAMES.AGE,
#'                              sex = GP$DIM.NAMES.SEX,
#'                              hiv.status = GP$DIM.NAMES.HIV,
#'                              year = DIM.NAMES.N))
#'   #5D
#'   v5temp=array(rep(0,GP$DIM.AGE*GP$DIM.SEX*GP$DIM.HIV*GP$DIM.NCD*DIM.N),  
#'                dim = c(GP$DIM.AGE,
#'                        GP$DIM.SEX,
#'                        GP$DIM.HIV,
#'                        GP$DIM.NCD,
#'                        DIM.N),
#'                dimnames=list(age = GP$DIM.NAMES.AGE,
#'                              sex = GP$DIM.NAMES.SEX,
#'                              hiv.status = GP$DIM.NAMES.HIV,
#'                              ncd.status = GP$DIM.NAMES.NCD,
#'                              year = DIM.NAMES.N))
#'   
#'   
#'   stats<-list(
#'     #1D arrays for entire population over time
#'     pop.size=v1temp,
#'     n.births=v1temp,
#'     n.births.non.hiv=v1temp,
#'     n.births.hiv=v1temp,
#'     n.deaths.hiv=v1temp,
#'     n.deaths.cvd=v1temp,
#'     n.deaths.ageout=v1temp,
#'     n.deaths.non.hiv=v1temp,
#'     
#'     #3D arrays by age, sex over time
#'     n.hiv.inc=v3temp,#'@MS: redundant?
#'     n.hiv.diag=v3temp,#'@MS: redundant?
#'     n.hiv.eng=v3temp,#'@MS: redundant?
#'     n.hiv.uneng=v3temp,#'@MS: redundant?
#'     
#'     # 4D arrays by age, sex and hiv state over time 
#'     #'@MS: should we use the new n.state.sizes instead of this one to extract both HIV and NCD outputs?
#'     #'@MS: redundant?
#'     n.hiv.prev=v4temp,
#'     
#'     #'@PK: added 4D arrays for tracking new NCD incidences (by age/sex/hiv status/year)
#'     n.diab.hyp.inc=v4temp,
#'     n.diab.inc=v4temp,
#'     n.hyp.inc=v4temp,
#'     
#'     # 5D arrays by age, sex, ncd & hiv state over time
#'     n.state.sizes=v5temp
#'   )
#'   return(stats)
#' }


# reset.GS<-function(gss){
#   gss$pop.size=v1temp
#   gss$n.births=v1temp
#   gss$n.births.non.hiv=v1temp
#   gss$n.births.hiv=v1temp
#   gss$n.deaths.hiv=v1temp
#   gss$n.deaths.cvd=v1temp
#   gss$n.deaths.ageout=v1temp
#   gss$n.deaths.non.hiv=v1temp
# 
#   gss$n.hiv.inc=v3temp
#   gss$n.hiv.diag=v3temp
#   gss$n.hiv.eng=v3temp
#   gss$n.hiv.uneng=v3temp
# 
#   gss$n.hiv.prev=v4temp   #'@MS: redundant?
# 
#   gss$n.diab.hyp.inc=v4temp
#   gss$n.diab.inc=v4temp
#   gss$n.hyp.inc=v4temp
# 
#   gss$n.state.sizes=v5temp
#   return(gss)
# }

# #return specific Stats
# return.GS.field<-function(gss, statName){
#   print(eval(parse(text = deparse(substitute(statName)))))
# }

