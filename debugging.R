
# # # Reading populations back into a simset object{
{  
  simset=list()
  invisible(lapply(c(1:10),function(rep){
    pop<-readRDS(sprintf("outputs/popList-%g",rep))
    simset[[sprintf("pop%g",rep)]]<<-pop
  }))
  print(paste0(length(simset)," ncd populationd data is read"))
  ncd.simset = simset
  #
  khm.simset = ncd.simset[[1]]$params$khm.full # HIV simset
  khm.ids<-lapply(simset,
                  function(x){ return (x$params$khm.id)})
  khm.ids=unlist(khm.ids)
  khm.simset=khm.simset[khm.ids];class(khm.simset)="khm_simulation_output"
  print(paste0(length(khm.simset)," khm populationd data is read"))
}


#   #comparing ncd and khm population sizes
#   simplot(khm.simset,ncd.simset,data.type = "population",scale.population = T)
simplot(khm.simset,ncd.simset,data.type = "population",scale.population = T, facet.by = c("age","sex"))
#   # simplot(ncd.simset,data.type = "population",facet.by = "age")
# simplot(khm.simset,ncd.simset,data.type = "population",scale.population = T, facet.by = "sex")
simplot(khm.simset,ncd.simset,data.type = "population",scale.population = T, facet.by = "hiv.status")

# # comparing deaths ???
# simplot(khm.simset,ncd.simset,data.type = "hiv.mortality",scale.population =T)
# simplot(khm.simset,ncd.simset,data.type = "non.hiv.mortality",scale.population = T,facet.by = "age")
# 
# simplot(ncd.simset,data.type = "mortality",scale.population = F,facet.by = "age")
# simplot(ncd.simset,data.type = "mortality",scale.population = F,facet.by = c("sex","age"))



# looking at population age distribution one year at a time
{
  #choose a year:
  plottingYear="2025"
  ##
  D<-lapply(c(1:10),function(x){
    pop=ncd.simset[[x]]
    res=filter.5D.stats.by.field(pop$stats$n.state.sizes,years = plottingYear,keep.dimensions = c("year","age","sex" ))
    res<-as.data.frame(rbind(res[,,1],res[,,2]));
    res=res/rowSums(res)
    res$sex=c("FEMALE","MALE")
    res$id=x
    return(res)
  })
  D=do.call(rbind,D)
  ncd.pop=D
  ##
  D=lapply(c(1:10),function(x){
    khm=khm.simset[[x]]
    res=return.khm.data(khm.output=khm,
                        data.type = "population",
                        years=plottingYear,
                        keep.dimensions = c("year","age","sex" ))
    res<-as.data.frame(rbind(res[,,1],res[,,2]));
    res=res/rowSums(res)
    res$sex=c("FEMALE","MALE")
    res$id=x
    return(res)
  })
  D=do.call(rbind,D)
  khm.pop=D
  ##
  res=as.data.frame(rbind(ncd.pop,khm.pop))
  par(mfrow=c(2,1))
  barplot( as.matrix(res[res$sex=="MALE",1:17]),beside = T,col = sapply(c("cyan","red"),rep,10),names.arg = DIM.NAMES.AGE,main = paste("Male pop in ",plottingYear))
  legend("topright",legend = c("ncd","khm"),fill = c("cyan","red"))
  barplot( as.matrix(res[res$sex=="FEMALE",1:17]),beside = T,col = sapply(c("cyan","red"),rep,10),names.arg = DIM.NAMES.AGE,main = paste("Female pop in ",plottingYear))
  legend("topright",legend = c("ncd","khm"),fill = c("cyan","red"))
}

# looking at changes in each agegroup accross various simulations over time
# we have 2 options:
# looking at each year independently, and mapping the age distribution in that year
# mapping each year's age dist relative to agegroup size in 2015 (all plots start from 1)
{
  plottingYear=as.character(c(2015:2030))
  N=length(ncd.simset)
  #NCD
  D=lapply(ncd.simset,function(pop){
    res=filter.5D.stats.by.field(pop$stats$n.state.sizes,ages = DIM.NAMES.AGE ,years = plottingYear,keep.dimensions = c("year","age" ))
    # return(res/rowSums(res)) #prop in each year
    
    res=t(t(res)/t(res)[,1]) #relative to 2015
    res[is.nan(res)]<-1;res[res==Inf]<-1;
    return(res)
  })
  D=as.data.frame(do.call(rbind,D))
  D$year=as.numeric(rep(plottingYear,length(N)))
  ncd.pop=D
  
  #KHM
  D=lapply(khm.simset,function(khm){
    res= return.khm.data(khm.output=khm,
                         data.type = "population",
                         ages = DIM.NAMES.AGE,
                         years=plottingYear,
                         keep.dimensions = c("year","age" ))
    # return(res/rowSums(res)) #prop in each year
    
    res=t(t(res)/t(res)[,1]) #relative to 2015
    res[is.nan(res)]<-1;res[res==Inf]<-1;
    return(res)
  })
  D=as.data.frame(do.call(rbind,D))
  D$year=as.numeric(rep(plottingYear,length(N)))
  khm.pop=D
  
  
  par(mfrow=c(4,5))
  lapply(c(1:17),function(c){
    plot(x=ncd.pop$year, y = ncd.pop[,c],col="cyan",main=DIM.NAMES.AGE[c],xlab ="",ylab=""
         ,ylim=range(ncd.pop[,c])*c(0.8,1.2)
    )
    points(x=khm.pop$year+.2, y = khm.pop[,c],col="red")
  })
  
}

#' #check NCD prevalence in 2015
#' {
#'   pop=simset$pop1
#'   ncd.states2015 = filter.5D.stats.by.field(pop$stats$n.state.sizes,
#'                                             years = as.character(2014),
#'                                             keep.dimensions = c('age','sex','ncd.status','year'))
#'   ncd.states2015=ncd.states2015[,,,1] #to remove year dimension
#'   ncd.props2015<-return.prop.sex.age(vFreq = ncd.states2015)
#'
#'   par(mfrow=c(2,2))
#'   plot(pop$params$target.ncd.props[,"MALE","NCD.DIAB"],type="l",ylab="",main="diab.prev male",xlab="agegroups")
#'   lines(ncd.props2015[,"MALE","NCD.DIAB"],col="red")
#'   plot(pop$params$target.ncd.props[,"FEMALE","NCD.DIAB"],type="l",ylab="",main="diab.prev female",xlab="agegroups")
#'   lines(ncd.props2015[,"FEMALE","NCD.DIAB"],col="red")
#'   plot(pop$params$target.ncd.props[,"MALE","NCD.HYP"],ylim=c(0, 0.6), type="l",ylab="",main="hyp.prev male",xlab="agegroups")
#'   lines(ncd.props2015[,"MALE","NCD.HYP"],col="red")
#'   plot(pop$params$target.ncd.props[,"FEMALE","NCD.HYP"],ylim=c(0, 0.6), type="l",ylab="",main="hyp.prev female",xlab="agegroups")
#'   lines(ncd.props2015[,"FEMALE","NCD.HYP"],col="red")
#' }

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
