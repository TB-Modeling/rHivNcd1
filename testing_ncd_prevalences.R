

run.one.year.for.ncd.test = function(sim){
  
  pop=sim$pop
  mc=sim$mc
  gss=sim$gss
  
  if ((mc$TNOW%%mc$ANNUAL.TIMESTEPS)!=0) break("TNOW is not set correctly")
  cat("Beginning the year ... ",mc$CYNOW,"\n")
  
  
  ## 1-MODEL REMAINING DEATHS --------
  n.ageout=sum(unlist(invisible(lapply(pop,function(x){
    if(x$age>=mc$MAX.AGE) {
      x$bMarkedDead.ageout=T;
      return(1)
    }}))))
  # killing agents
  {
    n<-length(pop)
    death.status=unlist(invisible(lapply(c(1:n),function(x){
      return(pop[[x]]$bMarkedDead.ageout)
    })))
    n.deaths.ageout<-sum(death.status)
    gss$n.deaths.ageout[mc$YNOW]=n.deaths.ageout
    pop<-pop[!death.status]   
  }
  
  ## 2-MODEL BIRTHS --------
  { # (just balance deaths; put all into non-HIV ) 
    n.births = n.deaths.ageout
    n.births.non.hiv = n.births 
    
    if(n.births.non.hiv>0){
      cat(n.births.non.hiv," non-HIV newborns are added","\n")
      vIds = c((mc$lastID+1): (mc$lastID+n.births.non.hiv))
      mc$lastID=mc$lastID+n.births.non.hiv
      vSexes = sample(c(mc$MALE,mc$FEMALE),n.births.non.hiv,prob = c(.5,.5),replace = T) # still 50/50 male/female
      pop1 = (mapply(Person$new, vIds,vSexes,0,mc$TNOW,mc$HIV.NEG,mc$NCD.NEG)) 
      pop<-c(pop,pop1)
      #
      gss$n.births.non.hiv[mc$YNOW]=n.births.non.hiv
    }
    
    gss$n.births[mc$YNOW]=n.births.non.hiv
  }
  
  ## 3- MODEL AGING --------
  invisible(lapply(pop,function(x){x$incAge}))
  
  ## 4- UPDATE NCD STATES & CVD RISKS FOR NEXT YEAR --------
  res= update.ncd.states(pop,mc,gss)
  pop=(res[[1]])
  mc=res[[2]]
  gss=(res[[3]])
  
 
  invisible(set.annual.cvd.risk())
  
  ##############################################
  # END OF YEAR----
  cat("Final pop size is ",length(pop),"\n")
  print(paste("End of year: ",mc$CYNOW," ---------------------------"))
  
  mc$YNOW<-mc$YNOW+1
  mc$CYNOW<-mc$CYNOW+1
  
  # Record annual statatistics --------
  gss<-record.annual.gss(pop,
                         mc,
                         gss)
  
  invisible(list(pop=pop,
                 mc=mc,
                 gss=gss))
  
}
