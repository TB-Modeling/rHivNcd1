#
#  R HIVNCD 2022
#  Stat.R
#  
#####################################
print("Sourcing Stat.R ... ")


#' @MS: add more arrays to keep track of HIV and NCD sizes

#global statistics
DIM.N=mc$END.YEAR-mc$INITIAL.YEAR+1
DIM.NAMES.N=c(mc$INITIAL.YEAR:mc$END.YEAR)

#temporary epty arrays to initialize stats
#1D
v1temp=rep(0,DIM.N)
#3D
v3temp=array(rep(0,mc$DIM.AGE*mc$DIM.SEX*DIM.N),  
             dim = c(mc$DIM.AGE,
                     mc$DIM.SEX,
                     DIM.N),
             dimnames=list(mc$DIM.NAMES.AGE,
                           mc$DIM.NAMES.SEX,
                           DIM.NAMES.N))
#4D
v4temp=array(rep(0,mc$DIM.AGE*mc$DIM.SEX*mc$DIM.HIV*DIM.N),  
             dim = c(mc$DIM.AGE,
                     mc$DIM.SEX,
                     mc$DIM.HIV,
                     DIM.N),
             dimnames=list(mc$DIM.NAMES.AGE,
                           mc$DIM.NAMES.SEX,
                           mc$DIM.NAMES.HIV,
                           DIM.NAMES.N))

gss<-list(
  #1D arrays for entire population over time
  pop.size=v1temp,
  n.births=v1temp,
  n.deaths.hiv=v1temp,
  n.deaths.cvd=v1temp,
  n.deaths.ageout=v1temp,
  n.deaths.gen=v1temp,
  
  #3D arrays by age, sex over time
  n.hiv.inc=v3temp,
  n.hiv.diag=v3temp,
  n.hiv.eng=v3temp,
  n.hiv.uneng=v3temp,
  
  # 4D arrays by age, sex and hiv state over time
  n.hiv.prev=v4temp
)
reset.gss<-function(){
  gss$pop.size=v1temp
  gss$n.births=v1temp
  gss$n.deaths.hiv=v1temp
  gss$n.deaths.cvd=v1temp
  gss$n.deaths.ageout=v1temp
  gss$n.deaths.gen=v1temp
  
  gss$n.hiv.inc=v3temp
  gss$n.hiv.diag=v3temp
  gss$n.hiv.eng=v3temp
  gss$n.hiv.uneng=v3temp
  
  
  gss$n.hiv.prev=v4temp
  
}
#return specific Stats
return.gss<-function(statName){
  print(eval(parse(text = deparse(substitute(statName)))))
}

#return hiv.state sizes by age and sex
return.gss.hiv.state.sizes<-function(pop){
  n=length(pop)
  hiv.state.sizes<-array(0,
                         dim=c(mc$DIM.AGE,mc$DIM.SEX,mc$DIM.HIV),
                         dimnames = list(mc$DIM.NAMES.AGE,mc$DIM.NAMES.SEX,mc$DIM.NAMES.HIV))
  invisible(lapply(c(1:n),function(x){
    hiv.state.sizes[  pop[[x]]$agegroup,pop[[x]]$sex, pop[[x]]$hivState] <<- hiv.state.sizes[  pop[[x]]$agegroup,pop[[x]]$sex, pop[[x]]$hivState]+1  }))
  
  hiv.state.sizes
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