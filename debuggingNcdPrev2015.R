{
  n=100000
  pop<-POPULATION$new(id = 0,
                      members = list(),
                      params = generate.new.modelParameter(),
                      stats =  generate.new.stat())
  
  # 2- create member list of persons for this population 
  #subset the first n row to create n persons
  sim.pop=pop$params$step.dataset
  if (n>nrow(sim.pop)) stop ("Requested size of initial population is greater than simpop data in 2015")
  sim.pop=sim.pop[1: n,]
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
  memberList = (mapply(PERSON$new,
                       tborn,
                       vIds,
                       vSexes,
                       vAges,
                       vHivState,
                       vNcdState,
                       vtDiabInc,
                       vtHypInc,
                       vtDiabHypInc
  ))
  memberList = unlist(memberList)
  pop$members<-memberList
  
  pop$greet()
}

current.ncd.states = filter.5D.stats.by.field(pop$return.state.size.distribution(),
                                              years = as.character(pop$params$CYNOW),
                                              keep.dimensions = c('age','sex','ncd.status','year'))
current.ncd.states=current.ncd.states[,,,1] #to remove year dimension
current.ncd.props<-return.prop.sex.age(vFreq = current.ncd.states)

#######
par(mfrow=c(2,2))
plot(pop$params$target.ncd.props[,"MALE","NCD.DIAB"],type="l",ylab="",main="diab.prev male",xlab="agegroups")
lines(current.ncd.props[,"MALE","NCD.DIAB"],col="red")

plot(pop$params$target.ncd.props[,"FEMALE","NCD.DIAB"],type="l",ylab="",main="diab.prev female",xlab="agegroups")
lines(current.ncd.props[,"FEMALE","NCD.DIAB"],col="red")

plot(pop$params$target.ncd.props[,"MALE","NCD.HYP"],ylim=c(0, 0.6), type="l",ylab="",main="hyp.prev male",xlab="agegroups")
lines(current.ncd.props[,"MALE","NCD.HYP"],col="red")

plot(pop$params$target.ncd.props[,"FEMALE","NCD.HYP"],ylim=c(0, 0.6), type="l",ylab="",main="hyp.prev female",xlab="agegroups")
lines(current.ncd.props[,"FEMALE","NCD.HYP"],col="red")
