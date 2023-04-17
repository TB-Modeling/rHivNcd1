# # Reading populations back into a simset object

#10 pops with a 500K persons
getwd()
setwd("Rockfish//")

simset=list()
lapply(c(1:10),function(rep){
  rep=1
  pop<-readRDS(file = "pop1")
  pop<-readRDS(file = sprintf("pop%g",rep))
  simset[[sprintf("pop%g",rep)]]<<-pop
})
simset