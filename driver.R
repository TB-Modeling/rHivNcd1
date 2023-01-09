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

# wrapper function to initialize and run the simulation
run.simulation<-function(rep=1 # replication count (1,...,Inf)
){
  sims<-lapply(c(1:rep),function(r){
    cat("Running replication ",r," \n")
    
    #initialize variables
    reset.gss()
    mc$TNOW=0
    mc$YNOW=1
    mc$CYNOW=mc$INITIAL.YEAR
    mc$ANNUAL.TIMESTEPS=12 #modeling monthly dynamics
    
    #Create initial population 
    cat("Generating Population ... ")
    pop<<-create.initial.population(n = mc$POP.SIZE)
    # D<-extract.pop.state.size.distribution(pop)
    # apply(D,1,sum)
    # apply(D,3,sum)
    
    cat("Setting attributes ... ")
    # Set initial HIV status in 2015
    invisible(set.initial.hiv.status()) #@JP: is the POP visible to all classes and methods?
    # D<-extract.pop.state.size.distribution(pop)
    # apply(D,3,sum)
    # apply(D,4,sum)
    # apply(D,c(3,4),sum)
    
    # Set initial CVD risks in 2015
    invisible(set.annual.cvd.risk())
    
    cat("Recording statistics ... ")
    # record annual statistics for year 2015
    record.annual.gss(pop,gss) 
    # return.gss.state.size.distribution(sim = list(pop=pop,mc=mc,gss=gss),
    #                                    keep.dimensions = c('hiv.status', 'year'))

    cat("Initial population is ready.")
    
    # run simulation
    sim<-list(pop=pop,mc=mc,gss=gss) 
    for(i in c(mc$INITIAL.YEAR:mc$END.YEAR)){
      sim<-run.one.year(sim)
    }
    
    # return the gss #@MS: we can return all stats or selected stats only, depending on how we use them
    return(sim)
    
    cat("End of replication",rep,"-----------","\n")
  })
  return(sims)
}

res<-run.simulation(rep=1)
