#
#  R HIVNCD 2022
#  R helper functions
#  
#####################################
print("Reading R Helper Functions... ")

# Transform 1D data on HIV state sizes (hiv.pop) to proportion of people in different HIV states by age/sex
print("loading function get.hiv.state.proportions")
transform.hiv.data.1d.to.3d = function(hiv.pop){
  #transform 1D data to correct array dimensions
  ages = DIM.NAMES.AGE
  sexes = DIM.NAMES.SEX
  hiv.status = DIM.NAMES.HIV
  
  hiv.dim.names = list(hiv.status = hiv.status,
                       age = ages,
                       sex = sexes)
  
  #compute proportions of HIV states in each age/sex subgroup
  hiv.probs = 
    sapply(1:length(hiv.dim.names$sex), function(sex){
      sapply(1:length(hiv.dim.names$age), function(age){
        hiv.pop[,age,sex]/sum(hiv.pop[,age,sex])
      })
    })
  
  dim(hiv.probs) = sapply(hiv.dim.names,length)
  dimnames(hiv.probs) = hiv.dim.names
  
  hiv.probs
}


#filters the state sizes to appropriate dimensions
print("loading function filter.stateSizes.by.field")
filter.5D.stats.by.field<- function(stateSizes, # a 5D array
                                    ages = DIM.NAMES.AGE, 
                                    sexes = DIM.NAMES.SEX,
                                    hiv.status = DIM.NAMES.HIV,
                                    ncd.status = DIM.NAMES.NCD,
                                    years=as.character(DIM.NAMES.YEAR),
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
  x = stateSizes
  x = x[ages,sexes,hiv.status,ncd.status,years] 
  
  # have to add dimnames back in just in case any dimension was reduced down to just one stratum (R then drops that dimension)
  dim.names = list(age=ages,
                   sex=sexes,
                   hiv.status=hiv.status,
                   ncd.status=ncd.status,
                   year=years)
  dim(x)=sapply(dim.names,length)
  dimnames(x)=dim.names
  
  #summing over dimensions that are to keep
  rv = apply(x, keep.dimensions, sum)
  #adjusting dimension names and 
  dim(rv) = sapply(keep.dim.names, length)
  dimnames(rv) = keep.dim.names
  rv
}

# transforms the frequencies into proportions 
print("loading function return.prop.sex.age")
return.prop.sex.age<-function(vFreq){
  vProp=vFreq
  if (length(dim(vFreq))==4){
    print("detected 4 dimensions")
    invisible(sapply(1:length(DIM.NAMES.SEX), function(sex){
      sapply(1:length(DIM.NAMES.AGE), function(age){
        vProp[age,sex,,]<<-vProp[age,sex,,]/sum(vFreq[age,sex,,]) 
      })}))
  }
  if (length(dim(vFreq))==3){
    print("detected 3 dimensions")
    invisible(sapply(1:length(DIM.NAMES.SEX), function(sex){
      sapply(1:length(DIM.NAMES.AGE), function(age){
        vProp[age,sex,]<<-vProp[age,sex,]/sum(vFreq[age,sex,]) 
      })}))
  }
  
  vProp[vProp=="NaN"] = 0 # to remove NaN values that were introduced by dividing by 0 
  vProp
}

#'@MS: to discuss:How to transform annual probability (AP) to monthly probability (MP):
#'This is applicable to prob of first CVD event (10-year to annual, and annual to monthly)
#case 1- if the first and subsequent events are independant: MP=AP/12
#case 2-if the probability indicate the chance to the first event occurance: then it follows a geometric distribution where prob(x)=(1-p)^(x-1)*p
# the cumulative prob for geom dist is 1-(1-p)^x
# AP= 1-(1-MP)^12 >> MP= 1-(1-AP)^(1/12)
print("loading function return.monthly.prob")
return.monthly.prob<-function(annual.prob=1){
  return(1-(1-annual.prob)^(1/12))
}
