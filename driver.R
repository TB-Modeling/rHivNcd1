#
#  R HIVNCD 2022
#  Driver.R class
#  
#####################################
print("Sourcing Driver.R ... ")

 
# wrapper function to initialize and run the simulation
run.simulation<-function(rep=1 # replication count (1,...,Inf)
){
  outputs<-lapply(c(1:rep),function(r){
    cat("Running replication ",r," \n")
    
    #initialize variables
    reset.gss()
    mc$TNOW=0
    mc$YNOW=1
    mc$CYNOW=mc$INITIAL.YEAR
    mc$ANNUAL.TIMESTEPS=12 #modeling monthly dynamics
    pop=NULL
    
    #Create initial population 
    cat("Generating Population ... ")
    pop<-create.initial.population(n = mc$POP.SIZE)
    
    # Set initial HIV status in 2015
    invisible(mapply(set.initial.hiv.status,c(1:length(pop)))) 
    
    # run simulation
    sim<-list(pop=pop,mc=mc,gss=gss)
    for(i in c(mc$INITIAL.YEAR:mc$END.YEAR)){
      sim<-model.annual.dynamics(sim)
    }
    
    # return the gss #@MS: we can return all stats or selected stats only, depending on how we use them
    return(sim$gss)
    
    cat("End of replication",rep,"-----------","\n")
  })
  return(outputs)
}

