#
#  R HIVNCD 2022
#  Driver.R class
#  
#####################################
print("Sourcing Driver.R ... ")

source("globalVariables.R")
source("person.R")
source("stats.R")
# source("driver.R")
source("rHelperFunctions.R")
source("rCoreFunctions.R")
source("plots.R")

#sample run for debugging
reset.gss()
mc$TNOW <-0
mc$YNOW <-1
mc$CYNOW <-mc$INITIAL.YEAR
mc$ANNUAL.TIMESTEPS <-12 #modeling monthly dynamics

pop<<-create.initial.population(n = mc$POP.SIZE)

invisible(set.initial.hiv.status()) #@JP: is the POP visible to all classes and methods?
invisible(set.annual.cvd.risk())
cat("Recording statistics ... ")
gss<-record.annual.gss(pop,mc,gss)
sim<-list(pop=pop,mc=mc,gss=gss)
# sim<-run.one.year(sim)


pop<-update.ncd.states()
sum(unlist(lapply(pop,function(x) return(x$bMarkedTransHyp))))
sum(unlist(lapply(pop,function(x) return(x$bMarkedTransDiab))))
sum(unlist(lapply(pop,function(x) return(x$bMarkedTransDiabHyp))))


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
