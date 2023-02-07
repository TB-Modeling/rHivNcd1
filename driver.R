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
                                 n = 1000)
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
{
  # for(i in c(INITIAL.YEAR:END.YEAR)){
  pop<-run.one.year(pop)
  # }
  
  #review the statistics 
  filter.stateSizes.by.field(pop$stats$n.state.sizes, keep.dimensions = c('year'))
  filter.stateSizes.by.field(pop$stats$n.state.sizes, keep.dimensions = c('year','hiv.status'))
  filter.stateSizes.by.field(pop$stats$n.state.sizes, keep.dimensions = c('year','ncd.status'))
  
  pop$stats$n.births
  pop$stats$n.births.non.hiv
  pop$stats$n.births.hiv
  
  pop$stats$n.deaths.ageout
  pop$stats$n.deaths.hiv
  pop$stats$n.deaths.non.hiv
  pop$stats$n.deaths.cvd
  
  filter.4D.stats.by.field(pop$stats$n.diab.hyp.inc, keep.dimensions = 'year')
  filter.4D.stats.by.field(pop$stats$n.diab.inc, keep.dimensions = 'year')
  filter.4D.stats.by.field(pop$stats$n.hyp.inc, keep.dimensions = 'year')
  
}
