# need a standard set of plots to review the population
# please add notes/documentation to plots.R

#
#  R HIVNCD 2022
#  Driver.R class
#  
#####################################
print("Sourcing Driver.R ... ")
{
  source("globalEnvironment.R")
  source("person.R")
  source("population.R")
  source("rHelperFunctions.R")
  source("rCoreFunctions.R")
  source("plots.R")
  source("testing_ncd_prevalences.R")
  source("ncdTestFunctions.R")
}
#######################################################
# SINGLE RUN
{
  # Create the population in year 2014; save the stats and move the clock to 2015
  bDebugMode=T
  set.seed(1)
  pop<-create.initial.population(id = 1,n = POP.SIZE)
  # setting up person attributes
  pop<-invisible(set.initial.hiv.status(pop ))
  pop<-invisible(set.annual.cvd.risk(pop))
  pop$record.annual.stats()
  pop$increaseYear()
  # run
  while(pop$params$CYNOW<= END.YEAR)
  pop<-run.one.year(pop)

  #saving population
  saveRDS(pop,file = "outputs/pop1",compress = F)
}

#######################################################
# multiple reps:
# lapply(c(1:5),function(rep){
#   bDebugMode=T
#   set.seed(1)
#   # create pop
#   pop<-create.initial.population(id = rep, n = POP.SIZE)
#   # setting up person attributes
#   pop<-invisible(set.initial.hiv.status(pop ))
#   pop<-invisible(set.annual.cvd.risk(pop))
#   pop$record.annual.stats()
#   pop$increaseYear() #
#   #run
#   while(pop$params$CYNOW<= END.YEAR)
#     pop<-run.one.year(pop)
#   #saving population
#   saveRDS(pop,file = sprintf("outputs/pop%g",rep),compress = F)
# })

#######################################################
# # Reading populations back into a simset object
# simset=list()
# lapply(c(1:4),function(rep){
#   pop<-readRDS(sprintf("outputs/pop%g",rep))
#   simset[[sprintf("pop%g",rep)]]<<-pop
# })
# simset

#######################################################
# PROFILING A SINGLE RUN
library(profvis)
profvis({
  {
    # Create the population in year 2014; save the stats and move the clock to 2015
    bDebugMode=T
    set.seed(1)
    pop<-create.initial.population(id = 1,n = POP.SIZE)
    # setting up person attributes
    pop<-invisible(set.initial.hiv.status(pop ))
    pop<-invisible(set.annual.cvd.risk(pop))
    pop$record.annual.stats()
    pop$increaseYear()
    # run
    while(pop$params$CYNOW<= END.YEAR)
      pop<-run.one.year(pop)
    
    #saving population
    # saveRDS(pop,file = "outputs/pop1",compress = F)
  }
})
#######################################################

# @MS
# add the mortality for those with recurrent event - DONE
# add the choice between two CVD events - DONE (keeping the same for sex right now)
# put all stats in a 5d format 
#'@PK - see note in globaleEnvironment about needing 4D arrays for transitions
# plotting functions for NCD model
# set of standard plots 
# meeting with Todd in a couple of weeks

# @PK
# work on saving a single replication to file for future simset (Todd)
# profiling the code :: 100,000 / 1m
# run the model on cloud



# pop$stats$n.births
# pop$stats$n.births.non.hiv
# pop$stats$n.births.hiv
# # #
# pop$stats$n.deaths.ageout
# pop$stats$n.deaths.hiv
# pop$stats$n.deaths.non.hiv
# pop$stats$n.deaths.cvd
# #  NCD incidence
# filter.4D.stats.by.field(pop$stats$n.diab.inc, keep.dimensions = c('year'))
# filter.4D.stats.by.field(pop$stats$n.hyp.inc, keep.dimensions = c('year'))
# filter.4D.stats.by.field(pop$stats$n.diab.hyp.inc, keep.dimensions = c('year'))
# # HIV events
# filter.4D.stats.by.field(pop$stats$n.hiv.inc, keep.dimensions = c('year',"age"))
# filter.4D.stats.by.field(pop$stats$n.diab.hyp.inc, keep.dimensions = c('year',"sex"))
# filter.4D.stats.by.field(pop$stats$n.diab.inc, keep.dimensions = c('year',"age","sex"))
# filter.4D.stats.by.field(pop$stats$n.hyp.inc, keep.dimensions = c('year',"age","sex"))
# 
# filter.5D.stats.by.field(pop$stats$n.mi.inc, keep.dimensions = c('year'))
# filter.5D.stats.by.field(pop$stats$n.stroke.inc, keep.dimensions = c('year'))
# 
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
