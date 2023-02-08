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

{  # -- Create the population in year 2014; save the stats and move the clock to 2015
  pop<-create.initial.population(id = 1,
                                 n = 10000)
  # filter.stateSizes.by.field(pop$return.state.size.distribution(), years = "2014",keep.dimensions = c("year","hiv.status") )
  
  # setting up person attributes
  pop<-invisible(set.initial.hiv.status(pop ))
  # filter.stateSizes.by.field(pop$return.state.size.distribution(), years = "2014",keep.dimensions = c("year","hiv.status") )
  
  pop<-invisible(set.annual.cvd.risk(pop))
  pop$record.annual.stats()
  
  # filter.stateSizes.by.field(pop$stats$n.state.sizes, keep.dimensions = c('year'))
  # filter.stateSizes.by.field(pop$stats$n.state.sizes,keep.dimensions = c('year','hiv.status'))
  
  pop$increaseYear() #
}

# RUNNING / DEBUGGING
# {
#   for(i in c(INITIAL.YEAR:END.YEAR)){
#   pop<-run.one.year(pop)
#    }

for(i in c(INITIAL.YEAR:END.YEAR)){
  pop = run.one.year.for.ncd.test(pop)
}

#review the statistics 
filter.stateSizes.by.field(pop$stats$n.state.sizes, keep.dimensions = c('year'))
filter.stateSizes.by.field(pop$stats$n.state.sizes, keep.dimensions = c('year','hiv.status'))
filter.stateSizes.by.field(pop$stats$n.state.sizes, keep.dimensions = c('year','ncd.status'))

# pop$stats$n.births
# pop$stats$n.births.non.hiv
# pop$stats$n.births.hiv
# 
# pop$stats$n.deaths.ageout
# pop$stats$n.deaths.hiv
# pop$stats$n.deaths.non.hiv
# pop$stats$n.deaths.cvd


filter.4D.stats.by.field(pop$stats$n.diab.hyp.inc, keep.dimensions = c('year'))
filter.4D.stats.by.field(pop$stats$n.diab.hyp.inc, keep.dimensions = c('year',"age"))
filter.4D.stats.by.field(pop$stats$n.diab.hyp.inc, keep.dimensions = c('year',"sex"))
filter.4D.stats.by.field(pop$stats$n.diab.inc, keep.dimensions = c('year',"age","sex"))
filter.4D.stats.by.field(pop$stats$n.hyp.inc, keep.dimensions = c('year',"age","sex"))

#ncd prev by year
ncd.prev = filter.stateSizes.by.field(pop$stats$n.state.sizes, keep.dimensions = c('year','ncd.status'))
D<-lapply(1:nrow(ncd.prev),function(x){return(ncd.prev[x,]/sum(ncd.prev[x,]))})
ncd.prev.prp<-do.call(rbind,D)                                                                                         

x=pop$params$target.ncd.sizes
dim(x)=c(dim(x),1)
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



ncd.prev = filter.stateSizes.by.field(pop$stats$n.state.sizes, keep.dimensions = c('year','age','sex','ncd.status'))
pop.by.age.sex = filter.stateSizes.by.field(pop$stats$n.state.sizes, keep.dimensions = c('year',"age","sex"))

ncd.prev.by.age.sex = array(NA,
                            dim = dim(ncd.prev),
                            dimnames = dimnames(ncd.prev))
for(i in 1:length(DIM.NAMES.NCD)){
  ncd.prev.by.age.sex[,,,i] = ncd.prev[,,,i]/pop.by.age.sex
}

apply(ncd.prev.by.age.sex,c(1:3),sum)

prev.difference = array(NA,
                        dim = length(DIM.NAMES.YEARS),
                        dimnames = list(DIM.NAMES.YEARS))
# check all years; mean squared error
for(i in 1:length(DIM.NAMES.YEARS)){

  prev.difference[i] = mean((ncd.prev.by.age.sex[i,,,]-pop$params$target.ncd.props)^2,na.rm = T) 
}

plot(prev.difference)  




