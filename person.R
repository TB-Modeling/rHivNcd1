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
        cvdState=CVD.NONE,
        nMi=0,
        nStroke=0,
        #incidence
        bMarkedMi=F,
        bMarkedStroke=F,
        #recording the time
        tMiInc=NULL,
        tStrokeInc=NULL,
        
        # you can add a function to return current mortality as a function of time since incidence (retrunMiMortality())
        #'@PK - I added these functions below. I don't actually have monthly MI mortality estimates yet, so this is just a placeholder
        returnStrokeMortality=function(params){
          p.months.since.stroke=tnow-self$tStrokeInc
          if(p.months.since.stroke<60){
            p.stroke.mortality=stroke.monthly.mortality[p.months.since.stroke] # if less than 5 years after event, take monthly prob
          } else if(p.months.since.stroke>=60){
            p.stroke.mortality=stroke.monthly.mortality[60] # if 5+ years after event, take 5-year prob
          }
        },
        
        # don't actually have mi.monthly.mortality vector yet
        returnMiMortality=function(tnow){
          p.months.since.mi=tnow-self$tMiInc
          if(p.months.since.mi<60){
            p.mi.mortality=mi.monthly.mortality[p.months.since.mi] # if less than 5 years after event, take monthly prob
          } else if(p.months.since.mi>=60){
            p.mi.mortality=mi.monthly.mortality[60] # if 5+ years after event, take 5-year prob
          }
        },
        
        # risk of recurrent CVD events (maybe another function returnCVDrisk( first check the CVD histpry = return the original risk, return 2* risk))
        returnCVDrisk=function(){
          if(self$bMarkedMi==F && self$bMarkedStroke==F){ # if no history of CVD, return original risk 
            annualCvdRisk=selfl$annualCvdRisk
            monthlyCvdRisk=self$monthlyCvdRisk
          } else if(self$bMarkedMi==T || self$bMarkedStroke==T){ # if any history of CVD, return risk*multiplier
            annualCvdRisk=self$annualCvdRisk*pop$params$recurrent.event.risk.multiplier # right now 2x the risk, but can change in SA
            monthlyCvdRisk=self$monthlyCvdRisk*pop$params$recurrent.event.risk.multiplier
          }
        }
        
        annualCvdRisk=NULL,
        monthlyCvdRisk=NULL,
        
        
        tDiabDiag=NULL,
        tDiabTrt=NULL,
        tHypDiag=NULL,
        tHypTrt=NULL,
        
        # at a population level >> rCoreFunctions
        # we will loop through pop members, and evauate risk of CVD death for a person
        # check the CVD history: if no events, risk of CVD death is 0
        # if MI only (nMi==1 && nstroke==0)
        # if stroke only *nMi==0 && n Stroke==1)
        # if hae had recurent events .... nMi >=1 and/or nStroke >=1
        ### evaluate risk of death >> if true: mark to die
        
        
        
        
       
        
  
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


