#
#  R HIVNCD 2022
#  Population class
#  
#####################################
print("Sourcing Population.R ... ")

# The R6 library has a lightweight class
# library(R6)

POPULATION<-R6Class("POPULATION",
                    public=list(
                      id=NULL,
                      members=NULL,
                      params=NULL, #model parameters
                      stats=NULL, #model statistics
                      ###
                      initialize=function(id=NULL,members=list(),params=list(),stats=list()){
                        self$id<-id
                        self$members<-members
                        self$params<-params
                        self$stats<-stats #generates a new general statistics
                      },
                      greet=function(){
                        cat("Pop id=",self$id,"created with ",length(self$members)," members.", 
                            "It has ",length(self$params), " parameters.", 
                            "Sum of all stats is ", sum(unlist(self$stats)),"\n")
                      },
                      increaseYear=function(){
                        self$params$YNOW<-self$params$YNOW+1
                        self$params$CYNOW<-self$params$CYNOW+1
                      },
                      increaseMonth=function(){
                        self$params$TNOW<-self$params$TNOW+1
                      },
                      addMembers=function(memberListNew=list()){
                        self$members<-c(self$members,memberListNew)
                      },
                      modelAging=function(){
                        invisible(lapply(self$members,function(p) {p$incAge}))
                      },
                      model.aging.out=function(){
                        vdead<-lapply(self$members,function(p) {
                          res=0; 
                          if (p$age>=MAX.AGE){ 
                            res=1;
                            self$record.deaths.ageout(p$agegroup,p$sex,p$hivState,p$ncdState)
                            }
                          return(res)
                        })
                        self$members <- self$members[!unlist(vdead)] #remove dead people
                        self$stats$n.deaths.ageout[self$params$YNOW]=sum(unlist(vdead))
                      },
                      ###
                      return.state.size.distribution=function(){
                        n=length(self$members)
                        state.sizes<-array(0,
                                           dim=c(DIM.AGE,DIM.SEX,DIM.HIV,DIM.NCD, 1),
                                           dimnames = list(DIM.NAMES.AGE,DIM.NAMES.SEX,DIM.NAMES.HIV,DIM.NAMES.NCD, as.character(self$params$CYNOW)))
                        invisible(lapply(self$members,function(p){
                          state.sizes[  p$agegroup,
                                        p$sex, 
                                        p$hivState,
                                        p$ncdState,1] <<- state.sizes[  p$agegroup,
                                                                        p$sex, 
                                                                        p$hivState, 
                                                                        p$ncdState,1] +1}))
                        state.sizes
                      },
                      ###
                      record.annual.stats=function(){
                        ynow=self$params$YNOW
                        self$stats$pop.size[ynow] <-length(self$members) #population size
                        self$stats$n.state.sizes[,,,,ynow] <- self$return.state.size.distribution() #state sizes
                      },
                      
                      #record HIV events
                      record.hiv.inc=function(age,sex,hiv,ncd){
                        self$stats$n.hiv.inc[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.hiv.inc[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      record.hiv.diag=function(age,sex,hiv,ncd){
                        self$stats$n.hiv.diag[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.hiv.diag[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      record.hiv.eng=function(age,sex,hiv,ncd){
                        self$stats$n.hiv.eng[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.hiv.eng[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      record.hiv.uneng=function(age,sex,hiv,ncd){
                        self$stats$n.hiv.uneng[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.hiv.uneng[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      
                      #record NCD events
                      record.diab.hyp.inc=function(age,sex,hiv,ncd){
                        self$stats$n.diab.hyp.inc[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.diab.hyp.inc[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      record.diab.inc=function(age,sex,hiv,ncd){
                        self$stats$n.diab.inc[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.diab.inc[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      record.hyp.inc=function(age,sex,hiv,ncd){
                        self$stats$n.hyp.inc[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.hyp.inc[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      
                      #record CVD events
                      record.mi.inc=function(age,sex,hiv,ncd){
                        self$stats$n.mi.inc[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.mi.inc[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      record.stroke.inc=function(age,sex,hiv,ncd){
                        self$stats$n.stroke.inc[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.stroke.inc[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      
                      ############### INTERVENTION ##################
                      # record HIV new diagnosis and treatment initiation through intervention
                      record.hiv.diag.int=function(age,sex,hiv,ncd){
                        self$stats$n.hiv.diag.int[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.hiv.diag.int[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      record.hiv.trt.int=function(age,sex,hiv,ncd){
                        self$stats$n.hiv.trt.int[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.hiv.trt.int[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      
                      
                      #record NCD diagnosis through intevention
                      record.diab.diag.int=function(age,sex,hiv,ncd){
                        self$stats$n.diab.diag.int[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.diab.diag.int[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      record.hyp.diag.int=function(age,sex,hiv,ncd){
                        self$stats$n.hyp.diag.int[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.hyp.diag.int[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      record.diab.hyp.diag.int=function(age,sex,hiv,ncd){
                        self$stats$n.diab.hyp.diag.int[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.diab.hyp.diag.int[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      
                      #record NCD treatment initiation through intervention
                      record.diab.trt.int=function(age,sex,hiv,ncd){
                        self$stats$n.diab.trt.int[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.diab.trt.int[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      record.hyp.trt.int=function(age,sex,hiv,ncd){
                        self$stats$n.hyp.trt.int[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.hyp.trt.int[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      record.diab.hyp.trt.int=function(age,sex,hiv,ncd){
                        self$stats$n.diab.hyp.trt.int[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.diab.hyp.trt.int[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      
                      #record deaths
                      record.deaths.hiv=function(age,sex,hiv,ncd){
                        self$stats$n.deaths.hiv[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.deaths.hiv[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      record.deaths.cvd=function(age,sex,hiv,ncd){
                        self$stats$n.deaths.cvd[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.deaths.cvd[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      record.deaths.ageout=function(age,sex,hiv,ncd){
                        self$stats$n.deaths.ageout[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.deaths.ageout[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      },
                      record.deaths.non.hiv=function(age,sex,hiv,ncd){
                        self$stats$n.deaths.non.hiv[age,sex,hiv,ncd,as.character(self$params$CYNOW)] <- 
                          self$stats$n.deaths.non.hiv[age,sex,hiv,ncd,as.character(self$params$CYNOW)]+1
                      }
                      
                    )
                    
)

