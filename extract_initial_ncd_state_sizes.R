#@MS:I wrote the code below to extract the initial NCD prevalences from the STEP survey. 
# I then saved it externally so that we read it into the model everytime. you can remove this after reviewing it
#add agegroups
step.dataset$agegroup[step.dataset$agegroup<1]<-1
step.dataset$agegroup[step.dataset$agegroup>mc$DIM.AGE]<-mc$DIM.AGE
#add ncd state
step.dataset$ncdstate=step.dataset$hypertension+2*step.dataset$diabetes+1
#loop through and count the state sizes
ncd.state.sizes<-array(0,
                       dim=c(mc$DIM.AGE,mc$DIM.SEX,mc$DIM.NCD),
                       dimnames = list(mc$DIM.NAMES.AGE,mc$DIM.NAMES.SEX,mc$DIM.NAMES.NCD))
invisible(lapply(1:nrow(step.dataset),function(x){
  p<-step.dataset[x,]
  ncd.state.sizes[p$agegroup,
                  p$sex,
                  p$ncdstate] <<- ncd.state.sizes[   p$agegroup,
                                                     p$sex,
                                                     p$ncdstate] +1
  }))
write.csv(ncd.state.sizes,file = "ncd.state.sizes.2015.csv")