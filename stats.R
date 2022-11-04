#
#  R HIVNCD 2022
#  Stat class
#  
#####################################
print("Reading Stats... ")
#' @MS: add more arrays to keep track of HIV and NCD sizes

#global statistics
DIM.N=mc$END.YEAR-mc$INITIAL.YEAR+1
DIM.NAMES.N=c(mc$INITIAL.YEAR:mc$END.YEAR)

vtemp=rep(0,DIM.N)
vvtemp=array(rep(0,mc$DIM.SEX*mc$DIM.AGE*DIM.N),  
             dim = c(mc$DIM.SEX,mc$DIM.AGE,DIM.N),
             dimnames=list(mc$DIM.NAMES.SEX,
                           mc$DIM.NAMES.AGE,
                           DIM.NAMES.N))
gss<-list(
  #1D arrays for entire population over time
  pop.size=vtemp,
  n.births=vtemp,
  n.deaths=vtemp,
  
  #3D arrays by sex and age over time
  n.hiv.inc=vvtemp,
  n.hiv.diag=vvtemp,
  n.hiv.eng=vvtemp,
  n.hiv.uneng=vvtemp
)
reset.gss<-function(){
  gss$pop.size=vtemp
  gss$n.births=vtemp
  gss$n.deaths=vtemp
  
  gss$n.hiv.inc=vvtemp
  gss$n.hiv.diag=vvtemp
  gss$n.hiv.eng=vvtemp
  gss$n.hiv.uneng=vvtemp
}
#return specific Stats
return.gss<-function(statName){
   print(eval(parse(text = deparse(substitute(statName)))))
}


#list of annual statistics that are collected throughout the simulation
# astats<-list(
#   pop.size=0,
#   hiv.inc=0,
#   hiv.diag=0,
#   art.initiation=0,
#   art.disengagement=0,
#   
#   diab.inc=0,
#   hyp.inc=0
# )

# rest.annual.stats=function(){
#   annual.stats$hiv.inc <<- 0
#   annual.stats$hiv.diag <<- 0
#   annual.stats$art.initiation <<- 0
#   annual.stats$art.disengagement <<- 0
#   annual.stats$diab.inc <<-0
#   annual.stats$hyp.inc<<-0
# }