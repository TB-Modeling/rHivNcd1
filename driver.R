#
#  R HIVNCD 2022
#  Driver.R class
#  
#####################################
print("Sourcing Driver.R ... ")

 
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
    print(return.pop.distribution(var = "sex"))
    print(return.pop.distribution(var = "agegroup"))
    print(return.pop.distribution(var = "hiv"))
    print(return.pop.distribution(var = "ncd"))
    
    # Set initial HIV status in 2015
    invisible(set.initial.hiv.status()) #@JP: is the POP visible to all classes and methods?
    print(return.pop.distribution(var = "hiv"))
    
    # Set initial CVD risk in 2015
    invisible(set.initial.annual.cvd.risk())
    
    # run simulation

    sim<-list(pop=pop,mc=mc,gss=gss)
    for(i in c(mc$INITIAL.YEAR:mc$END.YEAR)){
      sim<-model.annual.dynamics(sim)
    }
    
    # return the gss #@MS: we can return all stats or selected stats only, depending on how we use them
    return(sim)
    
    cat("End of replication",rep,"-----------","\n")
  })
  return(sims)
}

