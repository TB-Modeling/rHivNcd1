# # Reading populations back into a simset object

#10 pops with a 500K persons
r_path="~/OneDrive - Johns Hopkins/HIV NCD modeling/MELISSA/Model/rHivNcd/"
setwd(r_path)
setwd("outputs/")

ncd.simset=list()
invisible(lapply(c(11:20),function(rep){
  pop<-readRDS(file = sprintf("popList-c%g",rep))
  ncd.simset[[sprintf("pop%g",rep)]]<<-pop
}))
print(paste0("ncd.simset read with ",length(ncd.simset)," members"))
class(ncd.simset)="ncd_simulation_output"

setwd(r_path)


