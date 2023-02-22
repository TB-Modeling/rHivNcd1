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
        tHivInc=NULL,
        tHivDiag=NULL,
        tHivEng=NULL,
        tHivUneng=NULL,
        bMarkedHivInc=F,
        bMarkedHivDiag=F,
        bMarkedHivEng=F,
        bMarkedHivUneng=F,
        #
        ncdState=NCD.NEG, 
        bMarkedTransDiabHyp=FALSE, # True if person is chosen for transition to the new state
        bMarkedTransDiab=FALSE,
        bMarkedTransHyp=FALSE,
        tDiabInc=NULL,
        tHypInc=NULL,
        tDiabHypInc=NULL,
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
        tMiInc=NULL,
        tStrokeInc=NULL,
        
        
        # risk of CVD events 
        returnCVDrisk=function(p,params){
          if(p$nMi==0 && p$nStroke==0){ # if no history of CVD, return original risk
            annualCvdRisk=self$annualCvdRisk
            monthlyCvdRisk=p$monthlyCvdRisk
          } else if(p$nMi>0 || p$nStroke>0){ # if any history of CVD, return risk*multiplier
            annualCvdRisk=p$annualCvdRisk*pop$params$recurrent.event.risk.multiplier # right now 2x the risk, but can change in SA
            monthlyCvdRisk=p$monthlyCvdRisk*pop$params$recurrent.event.risk.multiplier
          }
        },
        
        # return CVD mortality
        returnCvdMortality = function(p,params){
          p.cvd.mortality = 0
          
          # First, evaluate if they have had at least one event 
          if(p$nMi + p$nStroke > 0){ 
            
            # Next, evaluate which event (stroke vs. MI) is more recent 
            ## STROKE MORE RECENT ##
            if(p$tStrokeInc>p$tMiInc){  
              p.months.since.stroke=tnow-p$tStrokeInc
              
              # Next, evaluate if this is a first or recurrent event 
              if(p$nMi + p$nStroke > 1) { 
                ## STROKE AS RECURRENT EVENT ##
                p.cvd.mortality=params$stroke.monthly.mortality[min(p.months.since.stroke,60)]*params$recurrent.event.mortality.multiplier
                # if 60 months or greater, return the value for 60 months

              } else { 
                ## STROKE AS FIRST EVENT ##
                p.cvd.mortality=params$stroke.monthly.mortality[min(p.months.since.stroke,60)]
              }
              
            } else { 
              ## MI MORE RECENT ##
              p.months.since.mi=tnow-p$tMiInc
              
              if(p$nMi + p$nStroke > 1) {  
                ## MI AS RECURRENT EVENT ##
                p.cvd.mortality=params$mi.monthly.mortality[min(p.months.since.mi,120)]*params$recurrent.event.mortality.multiplier 
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


