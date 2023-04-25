# need a standard set of plots to review the population
# please add notes/documentation to plots.R

#
#  R HIVNCD 2022
#  Driver.R class
#  
#####################################
# list.of.packages <- c("ggplot2", "R6","Rcpp")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)

library(R6)
# library(Rcpp)
library(ggplot2)
# library(data.table)
#######################################################
#function to return elapse run time for the simulation
hms_span <- function(start, end) {
  dsec <- as.numeric(difftime(end, start, unit = "secs"))
  hours <- floor(dsec / 3600)
  minutes <- floor((dsec - 3600 * hours) / 60)
  seconds <- dsec - 3600*hours - 60*minutes
  paste0(
    sapply(c(hours, minutes, seconds), function(x) {
      formatC(x, width = 2, format = "d", flag = "0")
    }), collapse = ":")
}

print("Sourcing dependencies")
{
  source("globalEnvironment.R")
  source("person.R")
  source("population.R")
  source("rHelperFunctions.R")
  source("rCoreFunctions.R")
  source("plots.R")
}
# #######################################################
# # SINGLE RUN ON ROCKFISH
# # {
#   # Create the population in year 2014; save the stats and move the clock to 2015
#   args = commandArgs(trailingOnly=TRUE)
#   rep=as.numeric(args[1])
#   # we need to set the seed first, then sample KHM models
#   set.seed(rep)
#   print(paste("replication ",rep,"starting..."))
# 
#   ####
#   start_time <- Sys.time()
#   pop<-initialize.simulation(id = rep, n = POP.SIZE)
# 
#   while(pop$params$CYNOW<= END.YEAR)
#     pop<-run.one.year(pop)
# 
#   #saving population
#   res=list(stats=pop$stats,
#                params=pop$params)
#   saveRDS(res,file = paste0("outputs/popList-c",rep),compress = T)
# 
#   # saving time
#   end_time <- Sys.time()
#   session_time=hms_span(start_time,end_time)
#   write.table(session_time,file = paste0("outputs/out-sessionTime",rep),col.names = F,row.names = F)
# # }

#######################################################
#######################################################
# SINGLE RUN WITH INTERVENTION
# {
#   # Create the population in year 2014; save the stats and move the clock to 2015
#   rep=1
#   bDebugMode=F
#   set.seed(1)
#   pop<-initialize.simulation(id = rep,n = POP.SIZE)
# 
# 
#   # pre-intervention
#   # while(pop$params$CYNOW< INT.START.YEAR)
#   #   pop<-run.one.year(pop)
#   #
#   # # model intervention
#   # while(pop$params$CYNOW<= INT.END.YEAR){
#   #   pop<-model.intervention(pop)
#   #   pop<-run.one.year(pop)
#   #   }
#   # post-intervention
#   while(pop$params$CYNOW<= 2030)
#     pop<-run.one.year(pop)
# 
#   filter.5D.stats.by.field(pop$stats$n.diab.inc,keep.dimensions = c("year"))
#   #saving population
#   res=list(stats=pop$stats,
#                params=pop$params)
#   saveRDS(res,file = paste0("outputs/popList-c",rep),compress = T)
# }
# #######################################################

# MULTI REPS
lapply(c(1:10),function(rep){
  start_time <- Sys.time()
  bDebugMode=F
  set.seed(rep)
  # create pop at the end of 2014; set up hiv/ncd states; records stats and increament the year to 2015
  pop<-initialize.simulation(id = rep, n = POP.SIZE)
  pop$stats$ncd.id
  
  #run sims
  while(pop$params$CYNOW<= 2030)
    pop<-run.one.year(pop)
  #saving population
  res=list(stats=pop$stats,
           params=pop$params)
  saveRDS(res,file = paste0("outputs/popList-",rep),compress = T)
  # saving time
  end_time <- Sys.time()
  session_time=end_time - start_time
  txt=paste("Model ",rep," >> session time ",session_time)
  write.table(x = txt,file = "outputs/out-sessionTime.txt",col.names = F,row.names = F,append = T)
})
# 
# #######################################################
