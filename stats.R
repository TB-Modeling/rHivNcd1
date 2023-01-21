#
#  R HIVNCD 2022
#  Stat.R
#  
#####################################
print("Sourcing Stat.R ... ")


#global statistics
DIM.N=mc$END.YEAR-mc$INITIAL.YEAR+2
DIM.NAMES.N=c(mc$INITIAL.YEAR:(mc$END.YEAR+1))

#temporary empty arrays to initialize stats
#1D
v1temp=rep(0,DIM.N,
           dim=DIM.N,
           dimnames = list(year=DIM.NAMES.N))
#3D
v3temp=array(rep(0,mc$DIM.AGE*mc$DIM.SEX*DIM.N),  
             dim = c(mc$DIM.AGE,
                     mc$DIM.SEX,
                     DIM.N),
             dimnames=list(age = mc$DIM.NAMES.AGE,
                           sex = mc$DIM.NAMES.SEX,
                           year = DIM.NAMES.N))
#4D
v4temp=array(rep(0,mc$DIM.AGE*mc$DIM.SEX*mc$DIM.HIV*DIM.N),  
             dim = c(mc$DIM.AGE,
                     mc$DIM.SEX,
                     mc$DIM.HIV,
                     DIM.N),
             dimnames=list(age = mc$DIM.NAMES.AGE,
                           sex = mc$DIM.NAMES.SEX,
                           hiv.status = mc$DIM.NAMES.HIV,
                           year = DIM.NAMES.N))
#5D
v5temp=array(rep(0,mc$DIM.AGE*mc$DIM.SEX*mc$DIM.HIV*mc$DIM.NCD*DIM.N),  
             dim = c(mc$DIM.AGE,
                     mc$DIM.SEX,
                     mc$DIM.HIV,
                     mc$DIM.NCD,
                     DIM.N),
             dimnames=list(age = mc$DIM.NAMES.AGE,
                           sex = mc$DIM.NAMES.SEX,
                           hiv.status = mc$DIM.NAMES.HIV,
                           ncd.status = mc$DIM.NAMES.NCD,
                           year = DIM.NAMES.N))


gss<-list(
  #1D arrays for entire population over time
  pop.size=v1temp,
  n.births=v1temp,
  n.births.non.hiv=v1temp,
  n.births.hiv=v1temp,
  n.deaths.hiv=v1temp,
  n.deaths.cvd=v1temp,
  n.deaths.ageout=v1temp,
  n.deaths.non.hiv=v1temp,
  
  #3D arrays by age, sex over time
  n.hiv.inc=v3temp,#'@MS: redundant?
  n.hiv.diag=v3temp,#'@MS: redundant?
  n.hiv.eng=v3temp,#'@MS: redundant?
  n.hiv.uneng=v3temp,#'@MS: redundant?
  
  # 4D arrays by age, sex and hiv state over time 
  #'@MS: should we use the new n.state.sizes instead of this one to extract both HIV and NCD outputs?
  #'@MS: redundant?
  n.hiv.prev=v4temp,
  
  # 5D arrays by age, sex, ncd & hiv state over time
  n.state.sizes=v5temp
)

reset.gss<-function(){
  gss$pop.size=v1temp
  gss$n.births=v1temp
  gss$n.births.non.hiv=v1temp
  gss$n.births.hiv=v1temp
  gss$n.deaths.hiv=v1temp
  gss$n.deaths.cvd=v1temp
  gss$n.deaths.ageout=v1temp
  gss$n.deaths.non.hiv=v1temp
  
  gss$n.hiv.inc=v3temp
  gss$n.hiv.diag=v3temp
  gss$n.hiv.eng=v3temp
  gss$n.hiv.uneng=v3temp
  
  gss$n.hiv.prev=v4temp   #'@MS: redundant?
  
  gss$n.state.sizes=v5temp
  
}
#return specific Stats
return.gss<-function(statName){
  print(eval(parse(text = deparse(substitute(statName)))))
}

#return hiv.state sizes by age and sex
return.gss.hiv.state.sizes<-function(pop){   #'@MS: redundant?
  n=length(pop)
  hiv.state.sizes<-array(0,
                         dim=c(mc$DIM.AGE,mc$DIM.SEX,mc$DIM.HIV),
                         dimnames = list(mc$DIM.NAMES.AGE,mc$DIM.NAMES.SEX,mc$DIM.NAMES.HIV))
  invisible(lapply(c(1:n),function(x){
    hiv.state.sizes[  pop[[x]]$agegroup,pop[[x]]$sex, pop[[x]]$hivState] <<- hiv.state.sizes[  pop[[x]]$agegroup,pop[[x]]$sex, pop[[x]]$hivState]+1  }))
  
  hiv.state.sizes
}



# extracts hiv & ncd state sizes by age and sex for a given population at this time
# called at the end of each year to record the value in the gss:n.state.sizes
extract.pop.state.size.distribution<-function(pop){
  n=length(pop)
  state.sizes<-array(0,
                     dim=c(mc$DIM.AGE,mc$DIM.SEX,mc$DIM.HIV,mc$DIM.NCD),
                     dimnames = list(mc$DIM.NAMES.AGE,mc$DIM.NAMES.SEX,mc$DIM.NAMES.HIV,mc$DIM.NAMES.NCD))
  invisible(lapply(c(1:n),function(x){
    state.sizes[  pop[[x]]$agegroup,
                  pop[[x]]$sex, 
                  pop[[x]]$hivState,
                  pop[[x]]$ncdState] <<- state.sizes[  pop[[x]]$agegroup,
                                                       pop[[x]]$sex, 
                                                       pop[[x]]$hivState, 
                                                       pop[[x]]$ncdState] +1  }))
  
  state.sizes
}

# returns the state size distributions for a given simulation
# can be called at all times to review the state sizes
return.gss.state.size.distribution<- function(sim,
                                              ages = mc$DIM.NAMES.AGE, 
                                              sexes = mc$DIM.NAMES.SEX,
                                              hiv.status = mc$DIM.NAMES.HIV,
                                              ncd.status = mc$DIM.NAMES.NCD,
                                              years=as.character(DIM.NAMES.N),
                                              keep.dimensions = 'year' # collapse all other dimensions & report the data as total value over this dimension
){
  if(all(keep.dimensions!='year'))  
    stop("must keep year dimension")
  
  #full names of all dimensions
  full.dim.names = list(
    age = ages,
    sex = sexes,
    hiv.status = hiv.status,
    ncd.status = ncd.status,
    year = years
  )
  #filtering unwanted dimensions out
  keep.dim.names = full.dim.names[keep.dimensions]
  x = sim$gss$n.state.sizes
  x = x[ages,sexes,hiv.status,ncd.status,years] 
  
  # have to add dimnames back in just in case any dimension was reduced down to just one stratum (R then drops that dimension)
  dim.names = list(age=ages,
                   sex=sexes,
                   hiv.status=hiv.status,
                   ncd.status=ncd.status,
                   year=years)
  dim(x)=sapply(dim.names,length)
  dimnames(x)=dim.names
  
  #'@MS: does the order of dimensions in keep.dimension matter? ("year", "sex"?)
  #summing over dimensions that are to keep
  rv = apply(x, keep.dimensions, sum)
  #adjusting dimension names and 
  dim(rv) = sapply(keep.dim.names, length)
  dimnames(rv) = keep.dim.names
  rv
}


extract.pop.ncd.distribution<-function(pop){
  n=length(pop)
  ncd.state.sizes<-array(0,
                         dim=c(mc$DIM.AGE,mc$DIM.SEX,mc$DIM.NCD),
                         dimnames = list(mc$DIM.NAMES.AGE,mc$DIM.NAMES.SEX,mc$DIM.NAMES.NCD))
  invisible(lapply(c(1:n),function(x){
    ncd.state.sizes[  pop[[x]]$agegroup,
                      pop[[x]]$sex, 
                      pop[[x]]$ncdState] <<- ncd.state.sizes[  pop[[x]]$agegroup,
                                                               pop[[x]]$sex,  
                                                               pop[[x]]$ncdState] +1  }))
  
  ncd.state.sizes
}


return.prop.sex.age<-function(vFreq){
  vProp=vFreq
  invisible(sapply(1:length(mc$DIM.NAMES.SEX), function(sex){
    sapply(1:length(mc$DIM.NAMES.AGE), function(age){
      vProp[age,sex,]<<-vProp[age,sex,]/sum(vFreq[age,sex,]) 
    })
  }))
  vProp[vProp=="NaN"] = 0 # to remove NaN values that were introduced by dividing by 0 
  vProp
}


#list of annual statistics that are collected throughout the simulation
# astats<-list(
#   pop.size=0,
#   hiv.inc=0,
#   hiv.diag=0,
#   art.initiation=0,
#   art.disengagement=0,
#   
#   diab.inc=0,
#   hyp.inc=0
# )

# rest.annual.stats=function(){
#   annual.stats$hiv.inc <<- 0
#   annual.stats$hiv.diag <<- 0
#   annual.stats$art.initiation <<- 0
#   annual.stats$art.disengagement <<- 0
#   annual.stats$diab.inc <<-0
#   annual.stats$hyp.inc<<-0
# }