

run.one.year.for.ncd.test = function(pop){

  cat("Beginning the year ... ",pop$params$CYNOW,"\n")
  khm=pop$params$khm
  
  ## 1-MODEL Deaths due to aging out --------
  n.ageout=sum(unlist(invisible(lapply(pop$members,function(x){
    if(x$age>=MAX.AGE) {
      x$bMarkedDead.ageout=T;
      return(1)
    }}))))
  # modeling deaths
  n.deaths.ageout=pop$remove.dead.ageout()
  pop$stats$n.deaths.ageout[pop$params$YNOW]=n.deaths.ageout
  
  
  ## 2-MODEL BIRTHS --------
  { # (just balance deaths; put all into non-HIV ) 
    n.births = n.deaths.ageout
    n.births.non.hiv = n.births 
    
    if(n.births.non.hiv>0){
      cat(n.births.non.hiv," non-HIV newborns are added","\n")
      vIds = c((pop$params$LAST.ID+1): (pop$params$LAST.ID+n.births.non.hiv))
      pop$params$LAST.ID=pop$params$LAST.ID+n.births.non.hiv
      vSexes = sample(c(MALE,FEMALE),n.births.non.hiv,prob = c(.5,.5),replace = T) # still 50/50 male/female
      memberListNew = (mapply(PERSON$new, vIds,vSexes,0,pop$params$TNOW,HIV.NEG,NCD.NEG)) #'@JP: do we need to delete pop1 and open memory?
      pop$addMembers(unlist(memberListNew))
      # record stats:
      pop$stats$n.births.non.hiv[pop$params$YNOW]=n.births.non.hiv
    }
    
    #record stats:
    pop$stats$n.births[pop$params$YNOW]=n.births.non.hiv 
    cat("n.births.non.hiv= ",n.births.non.hiv, " modeled \n")
  }
  
  ## 3- MODEL AGING --------
  pop$modelAging()
  
  ## 4- UPDATE NCD STATES & CVD RISKS FOR NEXT YEAR --------
  pop<-update.ncd.states(pop)
  pop<-invisible(set.annual.cvd.risk(pop))
  
  ##############################################
  # END OF YEAR----
  cat("Final pop size is ",length(pop$members),"\n")
  cat("End of year: ",pop$params$CYNOW," --------------------------- \n")
  
  # Record annual statatistics --------
  pop$record.annual.stats()
  
  #Increment the clock
  pop$increaseYear()
  
  pop
  
}
