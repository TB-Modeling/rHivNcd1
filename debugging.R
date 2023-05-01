
# # # Reading populations back into a simset object{
#1-5: baseline model
#6-10:  10X risk by HIV
#11-15: 1.1 annual ncd prev growth
{  
  reps=5
  simset=list()
  invisible(lapply(c((1:reps)+0),function(rep){
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


# Plot type 1: population - SEE STANDARD PLOTS LIST (WORD DOC) - Commented out means it's saved in the above for loops 
# simplot(khm.simset,ncd.simset,data.type = "population",scale.population = T)
simplot(khm.simset,ncd.simset,data.type = "population",scale.population = T, facet.by = "age")
# simplot(khm.simset,ncd.simset,data.type = "population",scale.population = T, facet.by = "sex")
# simplot(ncd.simset,data.type = "population",scale.population = F, facet.by="age")

# Plot type 2: HIV hiv.incidence
simplot(khm.simset, ncd.simset, data.type = "hiv.incidence", scale.population = T)
simplot(khm.simset, ncd.simset, data.type = "hiv.incidence", scale.population = T, facet.by = "age")
simplot(khm.simset, ncd.simset, data.type = "hiv.incidence", scale.population = T, facet.by = "sex")
# simplot(ncd.simset, data.type = "hiv.incidence")
# simplot(ncd.simset, data.type = "hiv.incidence", facet.by = "ncd.status")

# Plot type 3: HIV prevalence
simplot(khm.simset, ncd.simset, data.type = "hiv.prevalence", scale.population = T, facet.by = "age")
simplot(khm.simset, ncd.simset, data.type = "hiv.prevalence", scale.population = T, facet.by = c("age","sex"))
# simplot(khm.simset, ncd.simset, data.type = "hiv.prevalence", scale.population = T, facet.by = "hiv.status")
# simplot(ncd.simset, data.type = "hiv.prevalence", facet.by = "age")
# simplot(ncd.simset, data.type = "hiv.prevalence", facet.by = "ncd.status")

# Plot type 4: Diabetes incidence & prevalence
simplot(ncd.simset, data.type = "diab.inc")
simplot(ncd.simset, data.type = "diab.inc", facet.by = "age")
# simplot(ncd.simset, data.type = "diab.inc", facet.by = "sex")
# simplot(ncd.simset, data.type = "diab.inc", facet.by = "hiv.status")
simplot(ncd.simset, data.type = "diab.prev")
simplot(ncd.simset, data.type = "diab.prev", facet.by = "age")
simplot(ncd.simset, data.type = "diab.prev", facet.by = c("age","sex"),view.as.rate = T,per.X.population = 1)
simplot(ncd.simset, data.type = "diab.prev", facet.by = "sex")
jpeg("diab.prev.by.hiv0.jpeg")
simplot(ncd.simset, data.type = "diab.prev", facet.by = "hiv.status",view.as.rate = T,per.X.population = 1)
dev.off()
# Plot type 5: Hypertension incidence & prevalence
# simplot(ncd.simset, data.type = "hyp.inc")
# simplot(ncd.simset, data.type = "hyp.inc", facet.by = "age")
# simplot(ncd.simset, data.type = "hyp.inc", facet.by = "sex")
jpeg("hyp.inc.by.agesex0.jpeg",width = 1500,height = 1000)
simplot(ncd.simset, data.type = "hyp.inc", facet.by = c("age","sex"),view.as.rate = T,per.X.population = 1)
dev.off()
# simplot(ncd.simset, data.type = "hyp.inc", facet.by = "hiv.status",view.as.rate = T,per.X.population = 1)
simplot(ncd.simset, data.type = "hyp.prev",view.as.rate = T,per.X.population = 1) 
simplot(ncd.simset, data.type = "hyp.prev", facet.by = "age",view.as.rate = T,per.X.population = 1)
jpeg("hyp.prev.by.agesex0.jpeg",width = 1500,height = 1000)
simplot(ncd.simset, data.type = "hyp.prev", facet.by = c("age","sex"),view.as.rate = T,per.X.population = 1)
dev.off()
simplot(ncd.simset, data.type = "hyp.prev", facet.by = "sex",view.as.rate = T,per.X.population = 1)
# jpeg("hyp.prev.by.hiv0.jpeg")
simplot(ncd.simset, data.type = "hyp.prev", facet.by = "hiv.status",view.as.rate = T,per.X.population = 1)
# dev.off()

# Plot type 6: Diabetes + Hypertension incidence & prevalence
# simplot(ncd.simset, data.type = "diab.hyp.inc")
# simplot(ncd.simset, data.type = "diab.hyp.inc", facet.by = "age",view.as.rate = T,per.X.population = 1)
# simplot(ncd.simset, data.type = "diab.hyp.inc", facet.by = "sex")
# simplot(ncd.simset, data.type = "diab.hyp.inc", facet.by = "hiv.status",view.as.rate = T,per.X.population = 1)

# simplot(ncd.simset, data.type = "diab.hyp.prev")
# simplot(ncd.simset, data.type = "diab.hyp.prev", facet.by = "age",view.as.rate = T,per.X.population = 1)
# simplot(ncd.simset, data.type = "diab.hyp.prev", facet.by = "sex")
jpeg("diabHyp.prev.by.hiv0.jpeg")
simplot(ncd.simset, data.type = "diab.hyp.prev", facet.by = "hiv.status",view.as.rate = T,per.X.population = 1)
dev.off()

# looking at population age distribution one year at a time
{
  #choose a year:
  plottingYear="2015"
  ##
  D<-lapply(c(1:reps),function(x){
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
  D=lapply(c(1:reps),function(x){
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
  barplot( as.matrix(res[res$sex=="MALE",1:17]),beside = T,col = sapply(c("cyan","red"),rep,reps),names.arg = DIM.NAMES.AGE,main = paste("Male pop in ",plottingYear))
  legend("topright",legend = c("ncd","khm"),fill = c("cyan","red"))
  barplot( as.matrix(res[res$sex=="FEMALE",1:17]),beside = T,col = sapply(c("cyan","red"),rep,reps),names.arg = DIM.NAMES.AGE,main = paste("Female pop in ",plottingYear))
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
    res=as.data.frame(filter.5D.stats.by.field(pop$stats$n.state.sizes,
                                               ages = DIM.NAMES.AGE ,
                                               sex="FEMALE",
                                               years = plottingYear,
                                               keep.dimensions = c("year","age","sex" )))
    return(res/rowSums(res)) #prop in each year
    # 
    # res=t(t(res)/t(res)[,1]) #relative to 2015
    # res[is.nan(res)]<-1;res[res==Inf]<-1;
    # return(res)
  })
  D=as.data.frame(do.call(rbind,D))
  D$year=as.numeric(rep(plottingYear,length(N)))
  ncd.pop=D
  
  #KHM
  D=lapply(khm.simset,function(khm){
    res= as.data.frame(return.khm.data(khm.output=khm,
                                       data.type = "population",
                                       ages = DIM.NAMES.AGE,
                                       sex="FEMALE",
                                       years=plottingYear,
                                       keep.dimensions = c("year","age","sex" )))
    return(res/rowSums(res)) #prop in each year
    
    # res=t(t(res)/t(res)[,1]) #relative to 2015
    # res[is.nan(res)]<-1;res[res==Inf]<-1;
    # return(res)
  })
  D=as.data.frame(do.call(rbind,D))
  D$year=as.numeric(rep(plottingYear,length(N)))
  khm.pop=D
  
  
  par(mfrow=c(4,5))
  lapply(c(1:17),function(c){
    plot(x=khm.pop$year, y = khm.pop[,c],col="red",main=DIM.NAMES.AGE[c],xlab ="",ylab=""
         ,ylim=range(ncd.pop[,c])*c(0.8,1.2)
         
    )
    points(x=ncd.pop$year+.2, y = ncd.pop[,c],col="cyan")
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
