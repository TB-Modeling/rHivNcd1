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
}

{ 
  set.seed(1)
  # -- Create the population in year 2014; save the stats and move the clock to 2015
  pop<-create.initial.population(id = 1,
                                 n = 10000)
  # setting up person attributes
  # pop<-invisible(set.initial.hiv.status(pop ))
  # pop<-invisible(set.annual.cvd.risk(pop))
  pop$record.annual.stats()
  pop$increaseYear() #
}

# RUNNING / DEBUGGING
# {
#   for(i in c(INITIAL.YEAR:END.YEAR)){
#   pop<-run.one.year(pop)
#    }



#review the statistics 
# filter.stateSizes.by.field(pop$stats$n.state.sizes, keep.dimensions = c('year'))
# # filter.stateSizes.by.field(pop$stats$n.state.sizes, keep.dimensions = c('year','hiv.status'))
# # filter.stateSizes.by.field(pop$stats$n.state.sizes, keep.dimensions = c('year','ncd.status'))
# 
# pop$stats$n.births
# pop$stats$n.births.non.hiv
# pop$stats$n.births.hiv
# # 
# pop$stats$n.deaths.ageout
# pop$stats$n.deaths.hiv
# pop$stats$n.deaths.non.hiv
# pop$stats$n.deaths.cvd
# 
# 
# filter.4D.stats.by.field(pop$stats$n.diab.inc, keep.dimensions = c('year'))
# filter.4D.stats.by.field(pop$stats$n.hyp.inc, keep.dimensions = c('year'))
# filter.4D.stats.by.field(pop$stats$n.diab.hyp.inc, keep.dimensions = c('year'))

# filter.4D.stats.by.field(pop$stats$n.diab.hyp.inc, keep.dimensions = c('year',"age"))
# filter.4D.stats.by.field(pop$stats$n.diab.hyp.inc, keep.dimensions = c('year',"sex"))
# filter.4D.stats.by.field(pop$stats$n.diab.inc, keep.dimensions = c('year',"age","sex"))
# filter.4D.stats.by.field(pop$stats$n.hyp.inc, keep.dimensions = c('year',"age","sex"))


####################################################################################
for(i in c(INITIAL.YEAR:END.YEAR)){
pop = run.one.year.for.ncd.test(pop)
}
####################################################################################
####################################################################################
####################################################################################
# plot total NCD prevalence by year

# simulated ncd prev by year
sim.ncd.prev = filter.stateSizes.by.field(pop$stats$n.state.sizes, keep.dimensions = c('year','ncd.status'))
D<-lapply(1:nrow(sim.ncd.prev),function(x){return(sim.ncd.prev[x,]/sum(sim.ncd.prev[x,]))})
sim.ncd.prev<-do.call(rbind,D)                                                                                         

x=pop$params$target.ncd.sizes
dim(x)=c(dim(x),1) #add year
dimnames(x)=list(
  age = DIM.NAMES.AGE, 
  sex = DIM.NAMES.SEX,
  ncd.status = DIM.NAMES.NCD,
  year=as.character(2015)
)
target.ncd.prev=filter.4D.stats.by.field.ncd(x,
                                             years=as.character(2015),
                                             keep.dimensions = c('year','ncd.status'))
target.ncd.prev<-target.ncd.prev/sum(target.ncd.prev)

par(mfrow=c(2,2))
lapply(1:DIM.NCD,function(c){
  sim=sim.ncd.prev[,c]
  target=target.ncd.prev[,c]
  plot(sim, ylim=c(min(sim,target,na.rm = T),max(sim,target,na.rm = T)),
       main=DIM.NAMES.NCD[c],type="l")
  abline(h=target,col="red")
}) 

########################################################################################
# plot NCD prevalence by age and sex over time
{
  sim.ncd.prev = filter.stateSizes.by.field(pop$stats$n.state.sizes, keep.dimensions = c( 'age','sex' ,'year','ncd.status'))
dimnames(sim.ncd.prev)

vProp=sim.ncd.prev
vFreq=sim.ncd.prev
invisible(
  sapply(1:length(DIM.NAMES.SEX), function(sex){
    sapply(1:length(DIM.NAMES.AGE), function(age){
      sapply(1:length(DIM.NAMES.YEARS), function(year){
        # vFreq[age,sex,as.character(year),]
        vProp[age,sex,year,]<<- vProp[age,sex,year,]/sum(vFreq[age,sex,year,]) 
      })
    })
  }))
vProp[vProp=="NaN"] = 0 # to remove NaN values that were introduced by dividing by 0 
sim.ncd.prev=vProp
dim(sim.ncd.prev)

target.ncd.prev=pop$params$target.ncd.sizes
target.ncd.prev<-  return.prop.sex.age(vFreq = target.ncd.prev)
dim(target.ncd.prev)
}

{jpeg("ncdPrev_ageSex.jpeg",width = 3000,height = 1400)
par(mfrow=c(8,17))
invisible(
  lapply(1:DIM.SEX,function(sex){
    lapply(1:DIM.NCD,function(ncd){
      lapply(1:DIM.AGE,function(age){
        sim=sim.ncd.prev[age,sex,,ncd]
        target=target.ncd.prev[age,sex,ncd]
        plot(sim, ylim=c(min(sim,target,na.rm = T),max(sim,target,na.rm = T)),lwd=2,
             main=paste0(DIM.NAMES.NCD[ncd],"_",DIM.NAMES.SEX[sex],"_",DIM.NAMES.AGE[age]),
             type="l")
        abline(h=target,col="red",lwd=2)
      }) 
    }) 
  }) 
)
dev.off()
}

# getwd()
# D<-lapply(1:nrow(sim.ncd.prev),function(x){return(sim.ncd.prev[x,]/sum(sim.ncd.prev[x,]))})
# sim.ncd.prev<-do.call(rbind,D)                                                                                         
# 
# 
# ncd.prev = filter.stateSizes.by.field(pop$stats$n.state.sizes, keep.dimensions = c('year','age','sex','ncd.status'))
# pop.by.age.sex = filter.stateSizes.by.field(pop$stats$n.state.sizes, keep.dimensions = c('year',"age","sex"))
# 
# ncd.prev.by.age.sex = array(NA,
#                             dim = dim(ncd.prev),
#                             dimnames = dimnames(ncd.prev))
# for(i in 1:length(DIM.NAMES.NCD)){
#   ncd.prev.by.age.sex[,,,i] = ncd.prev[,,,i]/pop.by.age.sex
# }
# 
# apply(ncd.prev.by.age.sex,c(1:3),sum)
# 
# prev.difference = array(NA,
#                         dim = length(DIM.NAMES.YEARS),
#                         dimnames = list(DIM.NAMES.YEARS))
# # check all years; mean squared error
# for(i in 1:length(DIM.NAMES.YEARS)){
# 
#   prev.difference[i] = mean((ncd.prev.by.age.sex[i,,,]-pop$params$target.ncd.props)^2,na.rm = T) 
# }
# 
# plot(prev.difference)  




