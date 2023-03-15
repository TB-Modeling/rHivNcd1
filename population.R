#
#  R HIVNCD 2022
#  Population class
#  
#####################################
print("Sourcing Population.R ... ")

# The R6 library has a lightweight class
library(R6)

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
                      remove.dead.ageout=function(){
                        vdead <- unlist(invisible(lapply(self$members,function(p) {return(p$bMarkedDead.ageout)})))
                        self$members <- self$members[!vdead] #remove dead people
                        return(sum(vdead))
                      },
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
                      }
                    )
                    
)


# source("globalVariables.R")
# source("stats.R")
# source("person.R")
# GP
# 
# p1<-PERSON$new(id=1,sex="male",age="10")
# p2<-PERSON$new(id=2,sex="female",age="20")
# p1$greet();p2$greet()
# 
# pop<-POPULATION$new(id=1,members=list(p1,p2))
# pop$greet()
# pop$stats$pop.size<-rep(1,10)
#  
# pop$members[1]

