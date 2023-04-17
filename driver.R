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


#######################################################
# # SINGLE RUN ON ROCKFISH
{
  # Create the population in year 2014; save the stats and move the clock to 2015
  args = commandArgs(trailingOnly=TRUE)
  rep=as.numeric(args[1])
  # we need to set the seed first, then sample KHM models
  set.seed(rep)
  print(paste("replication ",rep,"starting..."))
#
  print("Sourcing dependencies")
  {
    source("globalEnvironment.R")
    source("person.R")
    source("population.R")
    source("rHelperFunctions.R")
    source("rCoreFunctions.R")
    source("plots.R")
  }
  ####
  start_time <- Sys.time()
  pop<-initialize.simulation(id = rep, n = POP.SIZE)

  while(pop$params$CYNOW<= END.YEAR)
    pop<-run.one.year(pop)
  
  #saving population
  saveRDS(list(stats=pop$stats,
               params=pop$params),file = paste0("outputs/popList-c",rep),compress = T)
  
  # saving time
  end_time <- Sys.time()
  session_time=hms_span(start_time,end_time)
  write.table(session_time,file = paste0("outputs/out-sessionTime",rep),col.names = F,row.names = F)
}

#######################################################
#######################################################
# SINGLE RUN
# {
#   # Create the population in year 2014; save the stats and move the clock to 2015
# rep=1
# bDebugMode=T
# set.seed(1)
# pop<-initialize.simulation(id = rep,n = POP.SIZE)
# # setting up person attributes
# pop<-invisible(set.initial.hiv.status(pop ))
# pop<-invisible(set.cvd.risk(pop))
# pop$record.annual.stats()
# pop$increaseYear()
# # run
# while(pop$params$CYNOW<= END.YEAR)
# pop<-run.one.year(pop)

#saving population
# saveRDS(pop,file = "outputs/pop1",compress = F)
# }
#######################################################

# MULTI REPS
# lapply(c(1:6),function(rep){
#  
#   start_time <- Sys.time()
#   bDebugMode=F
#   set.seed(rep)
#   # create pop; set up hiv/ncd states; records stats and increate year
#   pop<-initialize.simulation(id = rep, n = POP.SIZE)
#   #run sims
#   while(pop$params$CYNOW<= END.YEAR)
#     pop<-run.one.year(pop)
#   #saving population
#   saveRDS(pop,file = sprintf("outputs/pop%g",rep),compress = F)
#   # saving time
#   end_time <- Sys.time()
#   session_time=end_time - start_time
#   txt=paste("Model ",rep," >> session time ",session_time)
#   write.table(x = txt,file = "outputs/out-sessionTime.txt",col.names = F,row.names = F,append = T)
# })

#######################################################
# # Reading populations back into a simset object
# simset=list()
# lapply(c(1:6),function(rep){
#   pop<-readRDS(sprintf("outputs/pop%g",rep))
#   simset[[sprintf("pop%g",rep)]]<<-pop
# })
# simset
# 

# #######################################################
# 
# 
# # pop$stats$n.births
# # pop$stats$n.births.non.hiv
# # pop$stats$n.births.hiv
# # # #
# # pop$stats$n.deaths.ageout
# # pop$stats$n.deaths.hiv
# # pop$stats$n.deaths.non.hiv
# # pop$stats$n.deaths.cvd
# # #  NCD incidence
# filter.5D.stats.by.field(pop$stats$n.diab.inc, keep.dimensions = c('year'))
# filter.5D.stats.by.field(pop$stats$n.hyp.inc, keep.dimensions = c('year'))
# filter.5D.stats.by.field(pop$stats$n.diab.hyp.inc, keep.dimensions = c('year'))
# # # HIV events
# filter.5D.stats.by.field(pop$stats$n.hiv.inc, keep.dimensions = c('year',"age"))
# filter.5D.stats.by.field(pop$stats$n.diab.hyp.inc, keep.dimensions = c('year',"sex"))
# filter.5D.stats.by.field(pop$stats$n.diab.inc, keep.dimensions = c('year',"age","sex"))
# filter.5D.stats.by.field(pop$stats$n.hyp.inc, keep.dimensions = c('year',"age","sex"))
# # 
# filter.5D.stats.by.field(pop$stats$n.mi.inc, keep.dimensions = c('year'))
# filter.5D.stats.by.field(pop$stats$n.stroke.inc, keep.dimensions = c('year'))
# # 
# filter.5D.stats.by.field(pop$stats$n.state.sizes, keep.dimensions = c('year'))
# filter.5D.stats.by.field(pop$stats$n.state.sizes, keep.dimensions = c('year','hiv.status'))
# filter.5D.stats.by.field(pop$stats$n.state.sizes, keep.dimensions = c('year','ncd.status'))

# ####################################################################################
# for(i in c(INITIAL.YEAR:END.YEAR)){
#   pop = run.one.year.for.ncd.test(pop)
# }
# ####################################################################################
# ####################################################################################
# # plot NCD prevalence at the population-level by year
# # props and freq
# ####################################################################################
# # simulated ncd prev by year
# {
#   sim.ncd.prev.size = filter.stateSizes.by.field(pop$stats$n.state.sizes, keep.dimensions = c('ncd.status','year'))
#   D<-lapply(1:DIM.YEAR,function(year){
#     return(sim.ncd.prev.size[,year]/sum(sim.ncd.prev.size[,year]))})
#   sim.ncd.prev.prp<-t(do.call(rbind,D))
#
#   x=pop$params$target.ncd.size
#   dim(x)=c(dim(x),1) #add year
#   dimnames(x)=list(
#     age = DIM.NAMES.AGE,
#     sex = DIM.NAMES.SEX,
#     ncd.status = DIM.NAMES.NCD,
#     year=as.character(2015)
#   )
#   target.ncd.prev.size=filter.4D.stats.by.field.ncd(x,
#                                                     years=as.character(2015),
#                                                     keep.dimensions = c('ncd.status','year'))
#   #prp of total population in 2015
#   target.ncd.prev.prp<-target.ncd.prev.size/sum(target.ncd.prev.size)
#
#   # eqivalent target sizes for our model
#   target.ncd.prev.simPop= round(target.ncd.prev.prp* POP.SIZE)
#
#   {  jpeg("ncdPrev_total.jpeg",width = 3000,height = 1500,res=300)
#     par(mfrow=c(2,4))
#     lapply(1:DIM.NCD,function(c){
#       sim=sim.ncd.prev.prp[c,]
#       target=target.ncd.prev.prp[c,]
#       plot(sim, ylim=c(min(sim,target,na.rm = T),max(sim,target,na.rm = T)),
#            main=DIM.NAMES.NCD[c],type="l",lwd=2,ylab="proportion")
#       abline(h=target,col="red",lwd=2)
#     })
#     lapply(1:DIM.NCD,function(c){
#       sim=sim.ncd.prev.size[c,]
#       target=target.ncd.prev.simPop[c,]
#       plot(sim, ylim=c(min(sim,target,na.rm = T),max(sim,target,na.rm = T)),
#            main=DIM.NAMES.NCD[c],type="l",lwd=2,ylab="Frequency")
#       abline(h=target,col="red",lwd=2)
#     })
#     dev.off()
#   }
# }
# ########################################################################################
# # plot NCD prevalence by age and sex over time
# {
#   #frequency distribution of NCD states by age and sex
#   sim.ncd.prev.size = filter.stateSizes.by.field(pop$stats$n.state.sizes, keep.dimensions = c( 'age','sex' ,'ncd.status','year'))
#   # estimate ncd prevalence proportions in each age/sex strata
#   vFreq=sim.ncd.prev.size
#   vProp=vFreq
#   invisible(
#     sapply(1:length(DIM.NAMES.SEX), function(sex){
#       sapply(1:length(DIM.NAMES.AGE), function(age){
#         sapply(1:length(DIM.NAMES.YEAR), function(year){
#           vProp[age,sex,,year]<<- vProp[age,sex,,year]/sum(vFreq[age,sex,,year])
#         })
#       })
#     }))
#   vProp[vProp=="NaN"] = 0 # to remove NaN values that were introduced by dividing by 0
#   sim.ncd.prev.prp=vProp
#   dim(sim.ncd.prev.prp)
#
#   #target ncd prev proportions
#   target.ncd.prev.prp=pop$params$target.ncd.props
#
#   #estimate corresponding target frequencies in our population
#   target.ncd.prev.simPop=sim.ncd.prev.size
#   invisible(
#     lapply(1:DIM.AGE,function(age){
#       lapply(1:DIM.SEX,function(sex){
#         lapply(1:DIM.YEAR,function(year){
#           t=target.ncd.prev.prp[age,sex,]
#           popSize=sum(sim.ncd.prev.size[age,sex,,year])
#           #
#           target.ncd.prev.simPop[age,sex,,year]<<-round(t*popSize)
#         })  })}))
#
# }
#
# { #plot the ncd 'proportions' within each age/sex strata against target
#   jpeg("ncdPrevProp_ageSex.jpeg",width = 12000,height = 5000,res=300)
#   par(mfrow=c(8,17))
#   invisible(
#     lapply(1:DIM.SEX,function(sex){
#       lapply(1:DIM.NCD,function(ncd){
#         lapply(1:DIM.AGE,function(age){
#           sim=sim.ncd.prev.prp[age,sex,ncd,]
#           target=target.ncd.prev.prp[age,sex,ncd]
#           plot(sim, ylim=c(min(sim,target,na.rm = T),max(sim,target,na.rm = T)),lwd=2,
#                main=paste0(DIM.NAMES.NCD[ncd],"_",DIM.NAMES.SEX[sex],"_",DIM.NAMES.AGE[age]),
#                type="l",ylab="prop",xlab="")
#           abline(h=target,col="red",lwd=2)
#         })       })     })   )
#   dev.off()
# }
# {#plot the ncd 'frequencies' within each age/sex strata against target
#   jpeg("ncdPrevFreq_ageSex.jpeg",width = 12000,height = 6000,res=300)
#   par(mfrow=c(8,17))
#   invisible(
#     lapply(1:DIM.SEX,function(sex){
#       lapply(1:DIM.NCD,function(ncd){
#         lapply(1:DIM.AGE,function(age){
#           sim=sim.ncd.prev.size[age,sex,ncd,]
#           target=target.ncd.prev.simPop[age,sex,ncd,]
#           plot(sim, ylim=c(min(sim,target,na.rm = T),max(sim,target,na.rm = T)),lwd=4,
#                main=paste0(DIM.NAMES.NCD[ncd],"_",DIM.NAMES.SEX[sex],"_",DIM.NAMES.AGE[age]),
#                type="l",ylab="Freq",xlab="")
#           lines(target,col="green",lwd=4)
#         })       })     })   )
#   dev.off()
# }
# ########################################################################################
# # sum square error for sim freq vs target freq over year:
# # SSE= (sim.ncd.prev.size - target.ncd.prev.simPop)^2
#
# # SSE between proportions:
# target.prp<-array(rep(target.ncd.prev.prp, DIM.YEAR),dim=c(DIM.AGE,DIM.SEX,DIM.NCD,DIM.YEAR),dimnames = list(DIM.NAMES.AGE,DIM.NAMES.SEX,DIM.NAMES.NCD,DIM.NAMES.YEAR))
# SSE= (sim.ncd.prev.prp - target.prp)^2
#
# # for each NCD state in each YEAR: compute mean squared error accross all age/sex strata
# mse.ncd.year<-array(0,dim=c(DIM.NCD,DIM.YEAR),dimnames = list(DIM.NAMES.NCD,DIM.NAMES.YEAR))
# invisible(lapply(1:DIM.NCD,function(ncd){
#   lapply(1:DIM.YEAR,function(year){
#     mse.ncd.year[ncd,year] <<- mean(SSE[,,ncd,year])
#   })}))
#
#
# {  jpeg("mse_byNcdYear.jpeg",width = 1500,height = 1500,res=300)
#   par(mfrow=c(2,2))
#   lapply(1:DIM.NCD,function(c){
#     sim=mse.ncd.year[c,]
#     plot(sim,
#          main=DIM.NAMES.NCD[c],type="l",lwd=2,ylab="MSE")
#   })
# dev.off()
# }

#' 
#'      
#' # @MS:
#' #' @step.dataset has been randomized, correct?
#' 
#' 
#' #seems like lapply over the pop$member is the winner
#' system.time(
#'   invisible(sapply(pop$members,function(p){
#'     p.probs = hiv.probs[,p$agegroup,p$sex]
#'     if (p$hivState==HIV.NEG) p$bMarkedDead.hiv=T
#'   })), gcFirst = TRUE)
#' system.time(
#'   invisible(lapply(pop$members,function(p){
#'     p.probs = hiv.probs[,p$agegroup,p$sex]
#'     if (p$hivState==HIV.NEG) p$bMarkedDead.hiv=T
#'   })), gcFirst = TRUE)
#' 
#' system.time(
#'   invisible(lapply((1:length(pop$members)),function(x){
#'     p=pop$members[[x]]
#'     p.probs = hiv.probs[,p$agegroup,p$sex]
#'     if (p$hivState==HIV.NEG) p$bMarkedDead.hiv=T
#'   })), gcFirst = TRUE)
#' system.time(
#'   invisible(sapply((1:length(pop$members)),function(x){
#'     p=pop$members[[x]]
#'     p.probs = hiv.probs[,p$agegroup,p$sex]
#'     if (p$hivState==HIV.NEG) p$bMarkedDead.hiv=T
#'   })), gcFirst = TRUE)
#' 
#' 
#' #@MS
#' MP$annual.cvd.risk.by.age.sex=-((log(1- x/100 ))/10)
#' #assuming geometric distribution of risk over time
#' MP$monthly.cvd.risk.by.age.sex=(1-(1-MP$annual.cvd.risk.by.age.sex)^(1/12))
#' # why using differnt approaches to convert risk?
#' #why accessing previous agegroup?


# end_time <- Sys.time()
# session_time=end_time - start_time
# print(paste("Session time=",session_time))
# write.table(x = session_time,file = "outputs/out-sessionTime",col.names = F,row.names = F)
