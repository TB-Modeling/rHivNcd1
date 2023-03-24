# https://adv-r.hadley.nz/vectors-chap.html
# https://gallery.rcpp.org/articles/handling-R6-objects-in-rcpp/

#package is outdates, so instead we install from github
# install.packages("pkgapi") 

# library(devtools)
# install_github("r-lib/pkgapi")

# library(pkgapi)
# pkgapi::map_package("/Users/Trinity/Library/R/x86_64/4.1/library/base")
# .libPaths()
# 
# debugonce(lapply)
# lapply(c(1:10),x)
# undebug()
# 
# ?base::lapply
# showMethods("lapply")
# selectMethod("lapply", "ANY") 
# showMethods("sapply")


start_time <- Sys.time()

# please inlcude all libraries here and not in other scripts
library(R6)
library(Rcpp)
library(ggplot2)

print("Sourcing Driver.R ... ")
{
  source("globalEnvironment.R")
  source("person.R")
  source("population.R")
  source("rHelperFunctions.R")
  source("rCoreFunctions.R")
  source("plots.R")
  source("testing_ncd_prevalences.R")
  source("ncdTestFunctions.R")
}
sourceCpp(file = "cCoreFunctions.cpp",env = globalenv())
################################################################################################
create.initial.population <- function( id=0,
                                       n=0 # number of people if not specified as in mc
){
  # 1- create an empty population
  pop<-POPULATION$new(id = id,
                      members = list(),
                      params = generate.new.modelParameter(),
                      stats =  generate.new.stat())
  
  # 2- create member list of persons for this population 
  #subset the first n row to create n persons
  sim.pop=pop$params$step.dataset
  if (n>nrow(sim.pop)) stop ("Requested size of initial population is greater than simpop data in 2015")
  sim.pop=sim.pop[1:n,]
  #set up attributes for each row
  sim.pop$age[sim.pop$age==0] = 0.5 #we need this so that the first agegroups is set to 1
  sim.pop$age[sim.pop$age>85] = 85 
  sim.pop$sex[sim.pop$sex=="MALE"] = MALE 
  sim.pop$sex[sim.pop$sex=="FEMALE"] = FEMALE
  sim.pop$ncd[sim.pop$hypertension==0 & sim.pop$diabetes==0] = NCD.NEG #1, neither 
  sim.pop$ncd[sim.pop$hypertension==0 & sim.pop$diabetes==1] = NCD.DIAB #2, diabetic
  sim.pop$ncd[sim.pop$hypertension==1 & sim.pop$diabetes==0] = NCD.HYP #3, hypertensive
  sim.pop$ncd[sim.pop$hypertension==1 & sim.pop$diabetes==1] = NCD.DIAB_HYP #4, both
  #vector of attributes for agents
  vIds = c(1:n) #@MS: the person id is a unique number that we use to differentiate people. 
  # we need to start this from 0 and add it over time. 
  # otherwise, the future values may overlap with ones you're reading from the step dataset sim.pop$X
  vAges = sim.pop$age
  vSexes = as.numeric(sim.pop$sex)
  vNcdState = sim.pop$ncd
  vHivState = rep(HIV.NEG,n)
  vtDiabInc= c(sim.pop$ncd%in%c(NCD.DIAB))*0
  vtHypInc= c(sim.pop$ncd%in%c(NCD.HYP))*0
  vtDiabHypInc= c(sim.pop$ncd%in%c(NCD.DIAB_HYP))*0
  tborn=pop$params$TNOW
  #### option1: R
  # memberList = (mapply(PERSON$new,
  #                      tborn,
  #                      vIds,
  #                      vSexes,
  #                      vAges,
  #                      vHivState,
  #                      vNcdState,
  #                      vtDiabInc,
  #                      vtHypInc,
  #                      vtDiabHypInc
  # ))
  # memberList = unlist(memberList)
  # pop$members<-memberList
  
  #### option2: RCPP
  memberList=rcpp_initialize_population(
    tborn,
    vIds,
    vSexes,
    vAges,
    vHivState,
    vNcdState,
    vtDiabInc,
    vtHypInc,
    vtDiabHypInc
  )
  memberList = unlist(memberList)
  pop$members<-memberList
  
  pop$greet()
  
  pop
}

library(profvis)
profvis({
  {
    # Create the population in year 2014; save the stats and move the clock to 2015
    # bDebugMode=T
    set.seed(1)
    pop<-create.initial.population(id = 1,n = POP.SIZE)
    # 
    # # setting up person attributes
    # pop<-invisible(set.initial.hiv.status(pop ))
    # pop<-invisible(set.cvd.risk(pop))
    # pop$record.annual.stats()
    # pop$increaseYear()
    # # run
    # while(pop$params$CYNOW<= END.YEAR)
    #   pop<-run.one.year(pop)
    
    #saving population
    # saveRDS(pop,file = "outputs/pop1",compress = F)
  }
})


# total time
# 2590 R
# 2640 Rcpp