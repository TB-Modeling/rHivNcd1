#
#  R HIVNCD 2022
#  Person class
#  
#####################################

# The R6 library has a lightweight class
library(R6)
source("globalVariables.R")

# We create class instances with the Agent$new()
# function, which calls the 'initialize()' function
# we defined
# alice <- Agent$new(mc$SEX.FEMALE, 35)
Person<-R6Class("Person",
                clone=F,
      public=list(
        id=NULL,
        tborn=NULL,
        age=NULL,
        sex=NULL,
        
        hivState=mc$HIV.NEG,
        tHivInc=NULL,
        tHivDiag=NULL,
        tHivSupp=NULL,
        tHivDiseng=NULL,
        bMarkedHivInc=F,
        bMarkedHivDiag=F,
        bMarkedHivSupp=F,
        bMarkedHivDiseng=F,
        
        ncdState=mc$NCD.NEG, 
        tDiabInc=NULL,
        tDiabDiag=NULL,
        tDiabTrt=NULL,
        tHypInc=NULL,
        tHypDiag=NULL,
        tHypTrt=NULL,
        
        cvdState=mc$CVD.NONE,
        nMi=0,
        nStroke=0,
        bMarkedMi=F,
        bMarkedStroke=F,
        
        deathCause=NULL,
        bMarkedDead=F,
        ncdtrtState=NULL,
        bNcdscreened=F,
        tNcdscreened=F,
        ################################
        #define public functions here:
        initialize=function(id=NA,sex=NA,age=NA,tborn=0,ncdState=NA,tDiabInc=NA,tHypInc=NA){
          self$id<-id
          self$sex<-sex
          self$age<-age
          self$tborn<-tborn
          self$ncdState<-ncdState
          self$tDiabInc<-tDiabInc
          self$tHypInc<-tHypInc
          },
        greet = function() {
          cat(paste0("Hello, my id is ", self$id,", age=",self$age,", sex=",self$sex," , hivState=",self$hivState,",ncdState=",self$ncdState, ".\n"))
        },
        #HIV transitions:
        hiv.getInfected=function(tick){
          self$hivState=mc$HIV.UNDIAG
          tHivInc=tick
          bMarkedHivInc=F
        },
        hiv.getDiagnosed=function(tick){
          self$hivState=mc$HIV.DIAG_UNSUPP
          tHivDiag=tick
          bMarkedHivDiag=F
        },
        hiv.getSuppressed=function(tick){
          self$hivState=mc$HIV.SUPP
          tHivSupp=tick
          bMarkedHivSupp=F
        },
        hiv.getDisengage=function(tick){
          self$hivState=mc$HIV.DIAG_UNSUPP
          tHivDiseng=tick
          bMarkedHivDiseng=F
        },
        #NCD transitions
        diab.getInfected=function(tick){
          self$ncdState=mc$NCD.DIAB
          self$tDiabInc=tick
        },
        hyp.getInfected=function(tick){
          self$ncdState=mc$NCD.HYP
          self$tHypInc=tick
        }
      ),
      
      #Active fields are particularly useful in conjunction with private fields, 
      # because they make it possible to implement components that look like fields 
      # from the outside but provide additional checks.
      active=list(
        agegroup=function(){ ceiling(self$age/AGE.INTERVAL)},
        incAge=function(){ 
          self$age<-self$age+1
          invisible(self)}
      )
)


# p1<-Person$new(1,2,"male")        
# p1$age1year
# p1$greet()    
# p1$age<-10        
# p1$greet()    
# class(p1)
# names(p1)


