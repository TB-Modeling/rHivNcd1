update.ncd.states.diabHyp<-function(pop){ 
  # CURRENT NCD state sizes & prop   # 3D array of ncd state sizes: age, sex, ncd, year
  current.ncd.states = filter.stateSizes.by.field(pop$return.state.size.distribution(),
                                                  years = as.character(pop$params$CYNOW),
                                                  keep.dimensions = c('age','sex','ncd.status','year'))
  current.ncd.states=current.ncd.states[,,,1] #to remove year dimension
  current.ncd.props<-return.prop.sex.age(vFreq = current.ncd.states)
  
  # DIFFERENCE in prevalence proportions of NCDs
  diff.props =  pop$params$target.ncd.props-current.ncd.props
  
  # ADDITIONAL Transitions required to reach the target proportions in our current population:
  trans.freq=diff.props
  invisible(lapply(1:DIM.AGE, function(ag){
    lapply(1:DIM.SEX, function(sex){
      lapply(1:DIM.NCD, function(ncd){
        trans.freq[ag,sex,ncd]<<-diff.props[ag,sex,ncd]*sum(current.ncd.states[ag,sex,]) # the required number of new transitions
      })})}))
  trans.freq<-round(trans.freq,0) #round to integers

  ####################################################################################
  # DIAB_HYP
  ####################################################################################
  #PROBABILITY Of transition to DH for those in D or H state
  # list of people who are diabetric or hypertensive
  pop.ids.diab=rep(list(NULL),DIM.AGE*DIM.SEX)
  pop.ids.hyp=rep(list(NULL),DIM.AGE*DIM.SEX)
  
  # list ids for people in diab or hyp states who are eligible to transit to diab_hyp
  invisible(lapply(c(1:length(pop$members)),function(x){
    p=pop$members[[x]] 
    if(p$ncdState==NCD.DIAB) pop.ids.diab[ p$agegroup*p$sex] <<- list(c(unlist(pop.ids.diab[ p$agegroup*p$sex]), x))
    if(p$ncdState==NCD.HYP) pop.ids.hyp[ p$agegroup*p$sex] <<- list(c(unlist(pop.ids.hyp[ p$agegroup*p$sex]), x))
  }))    
  
  # model events for each strata
  invisible(lapply(1:DIM.AGE,function(age){
    lapply(1:DIM.SEX,function(sex){
      nEvents.from.diab=0
      nEvents.from.hyp=0
      nEvents.from.diab.hyp=0
      
      nNeeded<-trans.freq[age,sex,]
      if (bDebugMode) print(paste0("Num events needed in age=",age," sex=",sex," :  ",nNeeded["NCD.DIAB_HYP"]))
      
      if(nNeeded["NCD.DIAB_HYP"] >0){
        if( sum(nNeeded[c("NCD.HYP","NCD.DIAB")]>0)< 2) { #at least one state event is <0
          ### From Hyp?
          if (nNeeded[c("NCD.HYP")]<0){
            if ( nNeeded["NCD.DIAB_HYP"] - abs(nNeeded["NCD.HYP"])  >0){ #more hypDiab needed than those extra in Hyp
              nEvents.from.hyp= abs(nNeeded["NCD.HYP"])
              nNeeded["NCD.DIAB_HYP"]=nNeeded["NCD.DIAB_HYP"]-abs(nNeeded["NCD.HYP"])
            }else{
              nEvents.from.hyp=nNeeded["NCD.DIAB_HYP"]  
              nNeeded["NCD.DIAB_HYP"]  = 0
            } }
          ### From Diab?
          if(nNeeded["NCD.DIAB_HYP"] >0 && nNeeded["NCD.DIAB"] < 0 ) {
            if ( nNeeded["NCD.DIAB_HYP"] - abs(nNeeded["NCD.DIAB"])  >0){ 
              nEvents.from.diab= abs(nNeeded["NCD.DIAB"])
              nNeeded["NCD.DIAB_HYP"]=nNeeded["NCD.DIAB_HYP"]-abs(nNeeded["NCD.DIAB"])
            }else{
              nEvents.from.diab=nNeeded["NCD.DIAB_HYP"]  
              nNeeded["NCD.DIAB_HYP"]  = 0
            }}}
        ### From Hyp or Diab?
        nEvents.from.diab.hyp= nNeeded["NCD.DIAB_HYP"]
      }
      # print(paste0("calculated events from Hyp=",nEvents.from.hyp," from Diab=",nEvents.from.diab," from Diab.Hyp=",nEvents.from.diab.hyp))
      ###########################
      # eligible pop within this age/sex in who have diab or hyp
      elg.pop.ids.diab<-unlist(pop.ids.diab[age * sex])
      elg.pop.ids.hyp<-unlist(pop.ids.hyp[age * sex])
      # mark events
      if(nEvents.from.diab>0){
        x= sample(x =  1:length(elg.pop.ids.diab),
                  size = min(nEvents.from.diab,length(elg.pop.ids.diab)),
                  replace = F)
        selected.ids=elg.pop.ids.diab[x]
        # remove from list
        elg.pop.ids.diab <- elg.pop.ids.diab[-x]
        # mark pop members
        lapply(selected.ids,function(x){pop$members[[x]]$bMarkedTransDiabHyp=T  
        # p=pop$members[[x]]; print(paste0(p$agegroup," ",p$sex," marked"))
        })
        }
      ###
      if(nEvents.from.hyp>0){
        x= sample(x =  1:length(elg.pop.ids.hyp),
                  size = min(nEvents.from.hyp,length(elg.pop.ids.hyp)),
                  replace = F)
        selected.ids=elg.pop.ids.hyp[x]
        # remove from list
        elg.pop.ids.hyp <-elg.pop.ids.hyp[-x]
        # mark pop members
        lapply(selected.ids,function(x){pop$members[[x]]$bMarkedTransDiabHyp=T    
        # p=pop$members[[x]]; print(paste0(p$agegroup," ",p$sex," marked"))
        })
        }
        ###
        if(nEvents.from.diab.hyp>0){
          elg.pop.id=c(elg.pop.ids.hyp,elg.pop.ids.diab)
          x= sample(x =  1:length(elg.pop.id),
                    size = min(nEvents.from.diab.hyp,
                               length(elg.pop.id)),
                    replace = F)
          selected.ids=elg.pop.id[x]
          #no need to update the lists
          # mark pop members
          lapply(selected.ids,function(x){pop$members[[x]]$bMarkedTransDiabHyp=T    
          # p=pop$members[[x]]; print(paste0(p$agegroup," ",p$sex," marked"))
          })
          }
    })
  }))
  
  #model events
  D<- lapply(pop$members,function(p) {
    if (p$bMarkedTransDiabHyp==T){
      if(bDebugMode) print(paste0(p$agegroup," ",p$sex," became infected"))
      p$diab.hyp.getInfected(pop$params$TNOW)
      pop$record.inc.diab.hyp(p$agegroup,p$sex,p$hivState)
      return(1)
    }})
  # n.diab.hyp.inc=sum(unlist(D))
  #
  pop
}


update.ncd.states.hyp<-function(pop){ 
  # CURRENT NCD state sizes & prop   # 3D array of ncd state sizes: age, sex, ncd, year
  current.ncd.states = filter.stateSizes.by.field(pop$return.state.size.distribution(),
                                                  years = as.character(pop$params$CYNOW),
                                                  keep.dimensions = c('age','sex','ncd.status','year'))
  current.ncd.states=current.ncd.states[,,,1] #to remove year dimension
  current.ncd.props<-return.prop.sex.age(vFreq = current.ncd.states)
  
  # DIFFERENCE in prevalence proportions of NCDs
  diff.props =  pop$params$target.ncd.props-current.ncd.props
  
  # ADDITIONAL Transitions required to reach the target proportions in our current population:
  trans.freq=diff.props
  invisible(lapply(1:DIM.AGE, function(ag){
    lapply(1:DIM.SEX, function(sex){
      lapply(1:DIM.NCD, function(ncd){
        trans.freq[ag,sex,ncd]<<-diff.props[ag,sex,ncd]*sum(current.ncd.states[ag,sex,]) # the required number of new transitions
      })})}))
  trans.freq<-round(trans.freq,0) #round to integers
  
  ####################################################################################
  # HYP
  ####################################################################################
  #PROBABILITY Of transition to HYP for those SUS state
  # list of people who are sus
  pop.ids.ncdNeg=rep(list(NULL),DIM.AGE*DIM.SEX)
  invisible(lapply(c(1:length(pop$members)),function(x){
    p=pop$members[[x]] 
    if(p$ncdState==NCD.NEG) pop.ids.ncdNeg[ p$agegroup*p$sex] <<- list(c(unlist(pop.ids.ncdNeg[ p$agegroup*p$sex]), x))
  }))    
  
  # model events for each strata
  invisible(lapply(1:DIM.AGE,function(age){
    lapply(1:DIM.SEX,function(sex){
      nEvents.from.ncdNeg=trans.freq[age,sex,"NCD.HYP"]
      elg.pop.ids.ncdNeg<-unlist(pop.ids.ncdNeg[age * sex])
      if (bDebugMode) print(paste0("Num events needed in age=",age," sex=",sex," :  ",nEvents.from.ncdNeg))
      
      if(nEvents.from.ncdNeg>0 && length(elg.pop.ids.ncdNeg) >0 ){
      x= sample(x =  1:length(elg.pop.ids.ncdNeg),
                  size = min(nEvents.from.ncdNeg,length(elg.pop.ids.ncdNeg)),
                  replace = F)
        selected.ids=elg.pop.ids.ncdNeg[x]
        lapply(selected.ids,function(x){pop$members[[x]]$bMarkedTransHyp=T  })
      }
      })}))
  
  #model events
  D<- lapply(pop$members,function(p) {
    if (p$bMarkedTransHyp==T){
      if(bDebugMode) print(paste0(p$agegroup," ",p$sex," became infected"))
      p$hyp.getInfected(pop$params$TNOW)
      pop$record.inc.hyp(p$agegroup,p$sex,p$hivState)
      return(1)
    }})
  # sum(unlist(D))#
  pop
}

update.ncd.states.diab<-function(pop){ 
  # CURRENT NCD state sizes & prop   # 3D array of ncd state sizes: age, sex, ncd, year
  current.ncd.states = filter.stateSizes.by.field(pop$return.state.size.distribution(),
                                                  years = as.character(pop$params$CYNOW),
                                                  keep.dimensions = c('age','sex','ncd.status','year'))
  current.ncd.states=current.ncd.states[,,,1] #to remove year dimension
  current.ncd.props<-return.prop.sex.age(vFreq = current.ncd.states)
  
  # DIFFERENCE in prevalence proportions of NCDs
  diff.props =  pop$params$target.ncd.props-current.ncd.props
  
  # ADDITIONAL Transitions required to reach the target proportions in our current population:
  trans.freq=diff.props
  invisible(lapply(1:DIM.AGE, function(ag){
    lapply(1:DIM.SEX, function(sex){
      lapply(1:DIM.NCD, function(ncd){
        trans.freq[ag,sex,ncd]<<-diff.props[ag,sex,ncd]*sum(current.ncd.states[ag,sex,]) # the required number of new transitions
      })})}))
  trans.freq<-round(trans.freq,0) #round to integers
  ####################################################################################
  # DIAB
  ####################################################################################
  #PROBABILITY Of transition to DIAB for those SUS state
  # list of people who are sus
  pop.ids.ncdNeg=rep(list(NULL),DIM.AGE*DIM.SEX)
  invisible(lapply(c(1:length(pop$members)),function(x){
    p=pop$members[[x]] 
    if(p$ncdState==NCD.NEG) pop.ids.ncdNeg[ p$agegroup*p$sex] <<- list(c(unlist(pop.ids.ncdNeg[ p$agegroup*p$sex]), x))
  }))    
  
  # model events for each strata
  invisible(lapply(1:DIM.AGE,function(age){
    lapply(1:DIM.SEX,function(sex){
      nEvents.from.ncdNeg=trans.freq[age,sex,"NCD.DIAB"]
      elg.pop.ids.ncdNeg<-unlist(pop.ids.ncdNeg[age * sex])
      if (bDebugMode) print(paste0("Num events needed in age=",age," sex=",sex," :  ",nEvents.from.ncdNeg))
      
      if(nEvents.from.ncdNeg>0 && length(elg.pop.ids.ncdNeg) >0 ){
        x= sample(x =  1:length(elg.pop.ids.ncdNeg),
                  size = min(nEvents.from.ncdNeg,length(elg.pop.ids.ncdNeg)),
                  replace = F)
        selected.ids=elg.pop.ids.ncdNeg[x]
        lapply(selected.ids,function(x){pop$members[[x]]$bMarkedTransDiab=T  })
      }
    })}))
  
  #model events
  D<- lapply(pop$members,function(p) {
    if (p$bMarkedTransDiab==T){
      if(bDebugMode) print(paste0(p$agegroup," ",p$sex," became infected"))
      p$diab.getInfected(pop$params$TNOW)
      pop$record.inc.diab(p$agegroup,p$sex,p$hivState)
      return(1)
    }})
  #
  pop
}