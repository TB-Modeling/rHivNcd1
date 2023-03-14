#
#  R HIVNCD 2022
#  Person class
#  
#####################################
print("Sourcing Person.R ... ")

# The R6 library has a lightweight class
library(R6)

# We create class instances with the Agent$new()
# function, which calls the 'initialize()' function
# we defined alice <- Agent$new(SEX.FEMALE, 35)
PERSON<-R6Class("PERSON",
                clone=F, #allows for creating a deep copy of this person
      public=list(
        id=NULL,
        tborn=NULL,
        age=NULL,
        sex=NULL,
        #
        hivState=HIV.NEG,
        tHivInc=-1,
        tHivDiag=-1,
        tHivEng=-1,
        tHivUneng=-1,
        bMarkedHivInc=F,
        bMarkedHivDiag=F,
        bMarkedHivEng=F,
        bMarkedHivUneng=F,
        #
        ncdState=NCD.NEG, 
        bMarkedTransDiabHyp=FALSE, # True if person is chosen for transition to the new state
        bMarkedTransDiab=FALSE,
        bMarkedTransHyp=FALSE,
        tDiabInc=-1,
        tHypInc=-1,
        tDiabHypInc=-1,
        #
        bMarkedDead.hiv=F,
        bMarkedDead.non.hiv=F,
        bMarkedDead.cvd=F,
        bMarkedDead.ageout=F,
        #
        nMi=0,
        nStroke=0,
        #incidence
        #recording the time
        tMiInc=-1,
        tStrokeInc=-1,
        
        
        # risk of CVD events 
        returnCVDrisk=function(params){
          if(self$nMi==0 && self$nStroke==0){ # if no history of CVD, return original risk
            annualCvdRisk=self$annualCvdRisk
            monthlyCvdRisk=self$monthlyCvdRisk
          } else if(self$nMi>0 || self$nStroke>0){ # if any history of CVD, return risk*multiplier
            annualCvdRisk=self$annualCvdRisk* params$recurrent.event.risk.multiplier # right now 2x the risk, but can change in SA
            monthlyCvdRisk=self$monthlyCvdRisk* params$recurrent.event.risk.multiplier
          }
          monthlyCvdRisk
        },
        
        # return CVD mortality
        returnCvdMortality = function(params){
      
          p.cvd.mortality = 0
          
          # First, evaluate if they have had at least one event 
          
          if(self$nMi + self$nStroke > 0){ 
            
            # Next, evaluate which event (stroke vs. MI) is more recent 
            ## STROKE MORE RECENT ##
            if(self$tStrokeInc > self$tMiInc){  
              p.months.since.stroke=params$TNOW-self$tStrokeInc+1
              
              # Next, evaluate if this is a first or recurrent event 
              if(self$nMi + self$nStroke > 1) { 
                ## STROKE AS RECURRENT EVENT ## params value is an ODDS RATIO so have to convert to odds, multiply, then back to probability
                non.recurrent.stroke.mortality.prob = params$stroke.monthly.mortality[min(p.months.since.stroke,60)] # start with base probability 
                non.recurrent.stroke.mortality.odds = non.recurrent.stroke.mortality.prob/(1-non.recurrent.stroke.mortality.prob) # convert to odds 
                recurrent.stroke.mortality.odds = non.recurrent.stroke.mortality.odds*params$recurrent.stroke.mortality.OR # multiply by odds ratio 
                recurrent.stroke.mortality.prob = recurrent.stroke.mortality.odds/(1+recurrent.stroke.mortality.odds) # convert back to probability 
                
                p.cvd.mortality=recurrent.stroke.mortality.prob
            
              } else { 
                ## STROKE AS FIRST EVENT ##
                p.cvd.mortality=params$stroke.monthly.mortality[min(p.months.since.stroke,60)]
              }
              
            } else { 
              ## MI MORE RECENT ##
              p.months.since.mi=params$TNOW - self$tMiInc+1
              
              if(self$nMi + self$nStroke > 1) {  
                ## MI AS RECURRENT EVENT ##
                p.cvd.mortality=params$mi.monthly.mortality[min(p.months.since.mi,120)]*params$recurrent.MI.mortality.multiplier 
                # if 60 months or greater, return the value for 120 months
                
              } else { 
                ## MI AS FIRST EVENT ##
                p.cvd.mortality=params$mi.monthly.mortality[min(p.months.since.mi,120)]
              }
            }
          } # (If no events, don't need to return anything since we already set p.cvd.mortality to 0)
          
          p.cvd.mortality
        },
        
        annualCvdRisk=NULL,
        monthlyCvdRisk=NULL,
        
        
        tDiabDiag=NULL,
        tDiabTrt=NULL,
        tHypDiag=NULL,
        tHypTrt=NULL,

        
  
        ncdtrtState=NULL,
        bNcdscreened=F,
        tNcdscreened=F,
        ################################
        #define public functions here:
        initialize=function(id=NA,sex=NA,age=NA,tborn=0,hivState=NA,ncdState=NA,tDiabInc=NA,tHypInc=NA,tDiabHypInc=NA){
          self$id<-id
          self$sex<-sex
          self$age<-age
          self$tborn<-tborn
          self$hivState<-hivState
          self$ncdState<-ncdState
          self$tDiabInc<-max(tDiabInc,tDiabHypInc)
          self$tHypInc<-max(tHypInc,tDiabHypInc)
          self$tDiabHypInc<-tDiabHypInc
          },
        greet = function() {
          cat(paste0("Hello, my id is ", self$id,", age=",self$age,", sex=",self$sex," , hivState=",self$hivState,",ncdState=",self$ncdState, ".\n"))
        },
        #HIV transitions:
        hiv.getInfected=function(tnow){
          self$hivState=HIV.UNDIAG
          self$tHivInc=tnow
          self$bMarkedHivInc=F
        },
        hiv.getDiagnosed=function(tnow){
          self$hivState=HIV.UNENG
          self$tHivDiag=tnow
          self$bMarkedHivDiag=F
        },
        hiv.getEngaged=function(tnow){
          self$hivState=HIV.ENG
          self$tHivEng=tnow
          self$bMarkedHivEng=F
        },
        hiv.getUnengage=function(tnow){
          self$hivState=HIV.UNENG
          self$tHivUneng=tnow
          self$bMarkedHivUneng=FALSE
        },
        #NCD incidence 
        diab.getInfected=function(tnow){
          self$ncdState=NCD.DIAB
          self$tDiabInc=tnow
          self$bMarkedTransDiab=F
        },
        hyp.getInfected=function(tnow){
          self$ncdState=NCD.HYP
          self$tHypInc=tnow
          self$bMarkedTransHyp=F
        },
       diab.hyp.getInfected=function(tnow){
            self$ncdState=NCD.DIAB_HYP
            self$tDiabHypInc=tnow
            self$bMarkedTransDiabHyp=F
          },
       model.cvd.mi.event=function(tnow){
         self$nMi=self$nMi+1
         self$tMiInc=tnow
       },
       model.cvd.stroke.event=function(tnow){
         self$nStroke=self$nStroke+1
         self$tStrokeInc=tnow
       }
       

        
        
        #' @JP: we need to check this
        # #run a function when the object is garbage collected
        # finalize = function() {
        #   print("Finalizer has been called!")
        # }
      
        ),
      
      #Active fields are particularly useful in conjunction with private fields, 
      # because they make it possible to implement components that look like fields 
      # from the outside but provide additional checks.
      active=list(
        agegroup=function(){ min(max(ceiling((self$age+1)/AGE.INTERVAL),1),DIM.AGE)}, #we limit the last agegroups to not drop below 1 nor exceed 17
        incAge=function(){ 
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


