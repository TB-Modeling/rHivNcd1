#
#  R HIVNCD 2022
#  Stat class
#  
#####################################
print("Reading Stats... ")

#list of annual statistics that are collected throughout the simulation
annual.stats<-list(
  hiv.inc=0,
  hiv.diag=0,
  art.initiation=0,
  art.disengagement=0,
  
  diab.inc=0,
  hyp.inc=0
)

rest.annual.stats=function(){
  annual.stats$hiv.inc <<- 0  #@JP:what's this operator? <<-
  annual.stats$hiv.diag <<- 0
  annual.stats$art.initiation <<- 0
  annual.stats$art.disengagement <<- 0
  annual.stats$diab.inc <<-0
  annual.stats$hyp.inc<<-0
}