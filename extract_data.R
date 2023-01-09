
#'@MS: I suggest dividing it into two seperate functions, one for the NCD model (see rHelperFunctions:return.sim.state.size.distribution) and another one for your HIV model
#'we want to maximize the functions' usability accross the code and minimize redundant definitions 
#'can be moved to either rHelperfunctions or plot.R depending on how generalizable the function is 
#'@MS: please add a description for this function (and others if missing): document what each function input is
#'@MS: can you extend this function to include NCD status too?
extract.data = function(sim,
                        model,
                        data.type,
                        ages = mc$DIM.NAMES.AGE, 
                        sexes = mc$DIM.NAMES.SEX,
                        hiv.status = mc$DIM.NAMES.HIV,
                        # ncd.status = mc$DIM.NAMES.NCD,
                        years=as.character(DIM.NAMES.N),
                        keep.dimensions = 'year' # collapse all other dimensions & report the data as total value over this dimension
){
  if(all(keep.dimensions!='year'))
    stop("must keep year dimension")
  
  if(data.type=="population")
    rv = extract.population(sim,
                            model=model,
                            ages=ages,
                            sexes=sexes,
                            hiv.status=hiv.status,
                            # ncd.status=ncd.status,
                            years=years, 
                            keep.dimensions=keep.dimensions)
  
  else if(data.type=="hiv.prevalence")
    rv = extract.population(sim,
                            model=model,
                            ages=ages,
                            sexes=sexes,
                            hiv.status=mc$DIM.NAMES.HIV[-1], # remove HIV negative
                            # ncd.status=ncd.status,
                            years=years, 
                            keep.dimensions=keep.dimensions)
  else
    stop("data type doesn't exist yet")
  rv
  
}

extract.population = function(sim,
                              model,
                              ages = mc$DIM.NAMES.AGE, 
                              sexes = mc$DIM.NAMES.SEX,
                              hiv.status = mc$DIM.NAMES.HIV,
                              # ncd.status = mc$DIM.NAMES.NCD,
                              years=as.character(DIM.NAMES.N), 
                              keep.dimensions = 'year'){
  
  #full names of all dimensions
  full.dim.names = list(
    age = ages,
    sex = sexes,
    hiv.status = hiv.status,
    # ncd.status = ncd.status,
    year = years
  )
  
  #filtering unwanted dimensions out
  keep.dim.names = full.dim.names[keep.dimensions]
  
  if(model=="ncd"){
    x = sim$gss$n.hiv.prev
    x = x[ages,sexes,hiv.status,years]
  } else if(model=="hiv"){
    x=hiv.output.for.ncd$population # this doesn't actually use the hiv.sim object, uses the hiv.output.for.ncd object that corresponds to the sim - might be a problem later
    x=x[years,hiv.status,ages,sexes]
    x=aperm(x,c(3,4,2,1)) # reorder to have same dimension order as ncd output: age, sex, hiv.status, year
  } else stop("must specify model (hiv or ncd)")


  #summing over dimensions that are to keep
  rv = apply(x, keep.dimensions, sum)
  #adjusting dimension names and 
  dim(rv) = sapply(keep.dim.names, length)
  dimnames(rv) = keep.dim.names
  
  rv
}
