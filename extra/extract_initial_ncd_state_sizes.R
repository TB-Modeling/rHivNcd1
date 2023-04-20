#@MS:I wrote the code below to extract the initial NCD prevalences from the STEP survey. 
# I then saved it externally so that we read it into the model everytime. you can remove this after reviewing it
#add agegroups
step.dataset$agegroup[step.dataset$agegroup<1]<-1
step.dataset$agegroup[step.dataset$agegroup>DIM.AGE]<-DIM.AGE
#add ncd state
step.dataset$ncdstate=step.dataset$diabetes+2*step.dataset$hypertension+1
#loop through and count the state sizes
ncd.state.sizes<-array(0,
                       dim=c(DIM.AGE,DIM.SEX,DIM.NCD),
                       dimnames = list(DIM.NAMES.AGE,DIM.NAMES.SEX,DIM.NAMES.NCD))
invisible(lapply(1:nrow(step.dataset),function(x){
  p<-step.dataset[x,]
  ncd.state.sizes[p$agegroup,
                  p$sex,
                  p$ncdstate] <<- ncd.state.sizes[   p$agegroup,
                                                     p$sex,
                                                     p$ncdstate] +1
  }))
write.csv(ncd.state.sizes,file = "data/ncd.state.sizes.2015.csv")
