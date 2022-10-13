#
#  R HIVNCD 2022
#  Stat class
#  
#####################################
print("Reading Stats... ")

#global statistics
N=mc$END.YEAR-mc$INITIAL.YEAR+1

gss<-list(
  pop.size=rep(0,N),
  n.births=rep(0,N),
  n.deaths=rep(0,N)
  #' @MS: add more arrays to keep track of HIV and NCD sizes
)
reset.gss<-function(){
  gss$pop.size<<-rep(0,N)
  gss$n.births<<-rep(0,N)
  gss$n.deaths<<-rep(0,N)
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