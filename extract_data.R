


extract.data = function(sim,
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
                            ages=ages,
                            sexes=sexes,
                            hiv.status=hiv.status,
                            # ncd.status=ncd.status,
                            years=years, 
                            keep.dimensions=keep.dimensions)
  
  if(data.type=="hiv.prevalence")
    rv = extract.population(sim,
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
  
  x = sim$gss$n.hiv.prev
  x = x[ages,sexes,hiv.status,years]

  #summing over dimensions that are to keep
  rv = apply(x, keep.dimensions, sum)
  #adjusting dimension names and 
  dim(rv) = sapply(keep.dim.names, length)
  dimnames(rv) = keep.dim.names
  
  rv
}
