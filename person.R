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
# we defined
# alice <- Agent$new(mc$SEX.FEMALE, 35)
Person<-R6Class("Person",
                clone=T,
      public=list(
        id=NULL,
        tborn=NULL,
        age=NULL,
        sex=NULL,
        
        hivState=mc$HIV.NEG,
        tHivInc=NULL,
        tHivDiag=NULL,
        tHivEng=NULL,
        tHivUneng=NULL,
        bMarkedHivInc=F,
        bMarkedHivDiag=F,
        bMarkedHivEng=F,
        bMarkedHivUneng=F,
        
        ncdState=mc$NCD.NEG, 
        
        bMarkedTransDiabHyp=FALSE, # True if person is chosen for transition to the new state
        bMarkedTransDiab=FALSE,
        bMarkedTransHyp=FALSE,
        
        
        annualCvdRisk=NULL,
        monthlyCvdRisk=NULL,
        
        tDiabInc=NULL,
        tDiabDiag=NULL,
        tDiabTrt=NULL,
        tHypInc=NULL,
        tHypDiag=NULL,
        tHypTrt=NULL,
        tDiabHypInc=NULL,
        
        cvdState=mc$CVD.NONE,
        nMi=0,
        nStroke=0,
        bMarkedMi=F,
        bMarkedStroke=F,
        
        bMarkedDead.hiv=F,
        bMarkedDead.non.hiv=F,
        bMarkedDead.cvd=F,
        bMarkedDead.ageout=F,
        
  
        ncdtrtState=NULL,
        bNcdscreened=F,
        tNcdscreened=F,
        ################################
        #define public functions here:
        initialize=function(id=NA,sex=NA,age=NA,tborn=0,hivState=NA,ncdState=NA,tDiabInc=NA,tHypInc=NA){
          self$id<-id
          self$sex<-sex
          self$age<-age
          self$tborn<-tborn
          self$hivState<-hivState
          self$ncdState<-ncdState
          self$tDiabInc<-tDiabInc
          self$tHypInc<-tHypInc
          },
        greet = function() {
          cat(paste0("Hello, my id is ", self$id,", age=",self$age,", sex=",self$sex," , hivState=",self$hivState,",ncdState=",self$ncdState, ".\n"))
        },
        #HIV transitions:
        hiv.getInfected=function(tnow){
          self$hivState=mc$HIV.UNDIAG
          self$tHivInc=tnow
          self$bMarkedHivInc=F
        },
        hiv.getDiagnosed=function(tnow){
          self$hivState=mc$HIV.UNENG
          self$tHivDiag=tnow
          self$bMarkedHivDiag=F
        },
        hiv.getEngaged=function(tnow){
          self$hivState=mc$HIV.ENG
          self$tHivEng=tnow
          self$bMarkedHivEng=F
        },
        hiv.getUnengage=function(tnow){
          self$hivState=mc$HIV.UNENG
          self$tHivUneng=tnow
          self$bMarkedHivUneng=FALSE
        },
        #NCD incidence 
        diab.getInfected=function(tnow){
          self$ncdState=mc$NCD.DIAB
          self$tDiabInc=tnow
          self$bMarkedTransDiab=F
        },
        hyp.getInfected=function(tnow){
          self$ncdState=mc$NCD.HYP
          self$tHypInc=tnow
          self$bMarkedTransHyp=F
        },
       diab.hyp.getInfected=function(tnow){
            self$ncdState=mc$NCD.DIAB_HYP
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
        agegroup=function(){ min(max(ceiling((self$age+1)/mc$AGE.INTERVAL),1),mc$DIM.AGE)}, #we limit the last agegroups to not drop below 1 nor exceed 17
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


