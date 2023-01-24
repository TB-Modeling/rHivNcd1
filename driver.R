#
#  R HIVNCD 2022
#  Driver.R class
#  
#####################################
print("Sourcing Driver.R ... ")
{
  source("globalVariables.R")
  source("person.R")
  source("stats.R")
  source("rHelperFunctions.R")
  source("rCoreFunctions.R")
  source("plots.R")
  source("testing_ncd_prevalences.R")
  
  #sample run for debugging
  reset.gss()
  mc$TNOW <-0
  mc$YNOW <-1
  mc$CYNOW <-mc$INITIAL.YEAR
  mc$ANNUAL.TIMESTEPS <-12 #modeling monthly dynamics
}

{
  #create a pop with desired size
  pop<<-create.initial.population(n = 10000)
  
  # setting up person attributes
  invisible(set.initial.hiv.status())
  invisible(set.annual.cvd.risk())
  gss<-record.annual.gss(pop,mc,gss) # year 2015
  
  #building simulation object to run forward
  sim<-list(pop=pop,mc=mc,gss=gss)
}

# run simulation
# for(i in c(XX:YY)){ #set up the number of years to run the simulation
sim<-run.one.year(sim)
# }

# run simulation WITH ONLY: DEATHS, BIRTHS (balancing), AGING, NCD PREVALENCE UPDATES
for(i in c(mc$INITIAL.YEAR:mc$END.YEAR)){ #set up the number of years to run the simulation
sim<-run.one.year.for.ncd.test(sim)
}

# apply(gss$n.diab.hyp.inc,4,sum)
# apply(gss$n.diab.inc,4,sum)
# apply(gss$n.hyp.inc,4,sum)

# this is for temporary debugging
invisible(lapply(1:10,function(x){
  pop<-update.ncd.states()
}))

# # wrapper function to initialize and run the simulation
# run.simulation<-function(rep=1 # replication count (1,...,Inf)
# ){
# sims<-lapply(c(1:rep),function(r){
# cat("Running replication ",r," \n")
# 
#     #initialize variables
#     reset.gss()
#     mc$TNOW <-0
#     mc$YNOW <-1
#     mc$CYNOW <-mc$INITIAL.YEAR
#     mc$ANNUAL.TIMESTEPS <-12 #modeling monthly dynamics
# 
#     #Create initial population
#     cat("Generating Population ... ")
#     pop<<-create.initial.population(n = mc$POP.SIZE)
#     # D<-extract.pop.state.size.distribution(pop)
#     # apply(D,1,sum)
#     # apply(D,3,sum)
# 
#     cat("Setting attributes ... ")
#     # Set initial HIV status in 2015
#     invisible(set.initial.hiv.status()) #@JP: is the POP visible to all classes and methods?
#     # D<-extract.pop.state.size.distribution(pop)
#     # apply(D,3,sum)
#     # apply(D,4,sum)
#     # apply(D,c(3,4),sum)
# 
#     # Set initial CVD risks in 2015
#     invisible(set.annual.cvd.risk())
# 
# 
#     cat("Recording statistics ... ")
#     # record annual statistics for year 2015
#     gss<-record.annual.gss(pop,mc,gss)
#     # return.gss.state.size.distribution(sim = list(pop=pop,mc=mc,gss=gss),
#     #                                    keep.dimensions = c('hiv.status', 'year'))
# 
#     cat("Initial population is ready.")
#     sim<-list(pop=pop,mc=mc,gss=gss)
# 
#     # run simulation
#     for(i in c(mc$INITIAL.YEAR:mc$END.YEAR)){
#       sim<-run.one.year(sim)
#     }
#     # return the gss
#     return(sim)
# 
# cat("End of replication",rep,"-----------","\n")
# })
# return(sims)
# }
# 
# # res<-run.simulation(rep=1)



# ABM (main interface for running the final code )
# Driver
# rCoreFunction (pop) >>> Person.R...


