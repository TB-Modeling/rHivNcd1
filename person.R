#
#  R HIVNCD 2022
#  Person class
#  
#####################################
print("Sourcing Person.R ... ")
# library(R6)

# We create class instances with the Agent$new()
# function, which calls the 'initialize()' function
# we defined alice <- Agent$new(SEX.FEMALE, 35)
PERSON<-R6Class("PERSON",
                clone=F, #allows for creating a deep copy of this person
                public=list(
                  id=NULL,
                  tborn=NULL,
                  age=NULL, #monthly value
                  sex=NULL,
                  # hiv state & times
                  hivState=HIV.NEG,
                  tHivInc=-1,
                  tHivDiag=-1,
                  tHivEng=-1,
                  tHivUneng=-1,
                  # ncd state & times
                  ncdState=NCD.NEG, 
                  tDiabInc=-1,
                  tHypInc=-1,
                  tDiabHypInc=-1,
                  
                  #ncd screening
                  bNcdScreened=F,
                  
                  # ncd diag and treatment times
                  tDiabDiag=-1,
                  tHypDiag=-1,
                  tDiabHypDiag=-1,
                  tDiabTrt=-1,
                  tHypTrt=-1,
                  tDiabHypTrt=-1, 
                  
                  # cvd events and times
                  monthlyCvdRisk=0,
                  nMi=0,
                  nStroke=0,
                  tMiInc=-1,
                  tStrokeInc=-1,
                  
                  # marked for death?
                  bMarkedDead.hiv=F,
                  bMarkedDead.non.hiv=F,
                  bMarkedDead.cvd=F,
                  bMarkedDead.ageout=F,
                  
                  ################################
                  #define public functions here:
                  initialize=function(tborn=0,id=NA,sex=NA,age=NA,hivState=NA,ncdState=NA,tDiabInc=NA,tHypInc=NA,tDiabHypInc=NA){
                    self$tborn<-tborn
                    self$id<-id
                    self$sex<-sex
                    self$age<-age
                    self$hivState<-hivState
                    self$ncdState<-ncdState  
                    self$tDiabInc<-max(tDiabInc,tDiabHypInc)
                    self$tHypInc<-max(tHypInc,tDiabHypInc)
                    self$tDiabHypInc<-tDiabHypInc
                  },
                  greet = function() {
                    cat(paste0("Hello, my id is ", self$id,", age=",self$age,", sex=",self$sex," , hivState=",self$hivState,",ncdState=",self$ncdState, ".\n"))
                  },
                  
                  ### HIV transitions:
                  model.hiv.inc=function(tnow){
                    self$hivState=HIV.UNDIAG
                    self$tHivInc=tnow
                  },
                  model.hiv.diag=function(tnow){
                    self$hivState=HIV.UNENG
                    self$tHivDiag=tnow
                  },
                  model.hiv.eng=function(tnow){
                    self$hivState=HIV.ENG
                    self$tHivEng=tnow
                  },
                  model.hiv.uneng=function(tnow){
                    self$hivState=HIV.UNENG
                    self$tHivUneng=tnow
                  },
                  ### NCD incidence 
                  model.diab.inc=function(tnow){
                    self$ncdState=NCD.DIAB
                    self$tDiabInc=tnow
                  },
                  model.hyp.inc=function(tnow){
                    self$ncdState=NCD.HYP
                    self$tHypInc=tnow
                  },
                  model.diab.hyp.inc=function(tnow){
                    self$ncdState=NCD.DIAB_HYP
                    self$tDiabHypInc=tnow
                  },
                  
                  ### CVD events
                  model.mi.event=function(tnow){
                    self$nMi=self$nMi+1
                    self$tMiInc=tnow
                  },
                  model.stroke.event=function(tnow){
                    self$nStroke=self$nStroke+1
                    self$tStrokeInc=tnow
                  },
                  
                  ### risk of CVD events 
                  return.cvd.risk=function(params){
                    ncd.trt.multiplier=1
                    if(self$ncdState==NCD.DIAB.TRT) ncd.trt.multiplier=params$red.cvd.event.diab.trt
                    if(self$ncdState==NCD.HYP.TRT) ncd.trt.multiplier=params$red.cvd.event.hyp.trt
                    if(self$ncdState==NCD.DIAB_HYP.TRT) ncd.trt.multiplier=params$red.cvd.event.diabHyp.trt
                    
                    if(self$nMi+ self$nStroke==0) # if no history of CVD, return original risk
                      risk =self$monthlyCvdRisk * ncd.trt.multiplier
                    else 
                      risk =self$monthlyCvdRisk* params$recurrent.cvd.event.risk.multiplier * ncd.trt.multiplier
                    risk 
                  },
                  
                  ### return CVD mortality
                  return.cvd.mortality = function(params){
                    ncd.trt.multiplier=1
                    if(self$ncdState==NCD.DIAB.TRT) ncd.trt.multiplier=params$red.cvd.death.diab.trt
                    if(self$ncdState==NCD.HYP.TRT) ncd.trt.multiplier=params$red.cvd.death.hyp.trt
                    if(self$ncdState==NCD.DIAB_HYP.TRT) ncd.trt.multiplier=params$red.cvd.death.diabHyp.trt
                    ##
                    p.cvd.mortality = 0
                    
                    # First, evaluate if they have had at least one event
                    if(self$nMi + self$nStroke > 0){
                      # Next, evaluate which event (stroke vs. MI) is more recent
                      ## STROKE MORE RECENT ##
                      if(self$tStrokeInc >= self$tMiInc){
                        p.months.since.stroke=params$TNOW-self$tStrokeInc+1
                        
                        # Next, evaluate if this is a first or recurrent event
                        if(self$nMi + self$nStroke > 1) ## STROKE AS RECURRENT EVENT
                          p.cvd.mortality=params$rec.stroke.monthly.mortality[min(p.months.since.stroke,60)]
                        else ## STROKE AS FIRST EVENT ##
                          p.cvd.mortality=params$first.stroke.monthly.mortality[min(p.months.since.stroke,60)]
                      } else {
                        ## MI MORE RECENT ##
                        p.months.since.mi=params$TNOW - self$tMiInc+1
                        if(self$nMi + self$nStroke > 1)  ## MI AS RECURRENT EVENT ##
                          p.cvd.mortality=params$rec.mi.monthly.mortality[min(p.months.since.mi,120)]
                        else       ## MI AS FIRST EVENT ##
                          p.cvd.mortality=params$first.mi.monthly.mortality[min(p.months.since.mi,120)]
                      }
                    } # (If no events, don't need to return anything since we already set p.cvd.mortality to 0)
                    p.cvd.mortality * ncd.trt.multiplier
                  },
                  
                  ### INTERVENTION ###
                  mark.ncd.screening=function(){
                    self$bNcdScreened=TRUE
                  },
                  #model ncd diag
                  model.diab.diag=function(tnow){
                    self$tDiabDiag=tnow
                  },
                  model.hyp.diag=function(tnow){
                    self$tHypDiag=tnow
                  },
                  model.diab.hyp.diag=function(tnow){
                    self$tDiabHypDiag=tnow
                  },
                  # start ncd treatment
                  start.diab.trt=function(tnow){
                    self$ncdState=NCD.DIAB.TRT
                    self$tDiabTrt=tnow
                  },
                  start.hyp.trt=function(tnow){
                    self$ncdState=NCD.HYP.TRT
                    self$tHypTrt=tnow
                  },
                  start.diab.hyp.trt=function(tnow){
                    self$ncdState=NCD.DIAB_HYP.TRT
                    self$tDiabHypTrt=tnow
                  },
                  model.ncd.trt.dropout=function(pDropOut){
                    if(runif(1)<pDropOut){
                      if (self$ncdState==NCD.DIAB.TRT) self$ncdState=NCD.DIAB
                      if (self$ncdState==NCD.HYP.TRT) self$ncdState=NCD.HYP
                      if (self$ncdState==NCD.DIAB_HYP.TRT) self$ncdState=NCD.DIAB_HYP
                    }
                  }
                  
                ),
                #Active fields are particularly useful in conjunction with private fields, 
                # because they make it possible to implement components that look like fields 
                # from the outside but provide additional checks.
                active=list(
                  annualAge=function(){floor(age/12)},
                  agegroup=function(){ min(max(ceiling(( floor(self$age/12) +1)/AGE.INTERVAL),1),DIM.AGE)}, #we limit the last agegroups to not drop below 1 nor exceed 17
                  incAge=function(){ #monthly age
                    self$age<-self$age+1
                    invisible(self)
                  }
                )
                
)


# p1<-Person$new(1,2,"male")        
# p1$age1year
# p1$greet()    
# p1$age<-10        
# p1$greet()    
# class(p1)
# names(p1)


