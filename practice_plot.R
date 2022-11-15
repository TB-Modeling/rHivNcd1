library(ggplot2)

plot.hiv.distribution = function(hiv.positive.population.only=T,
                                 strata="total",
                                 years=as.character(c(2015:2030))){
  
  # HIV model output 
  if(hiv.positive.population.only) {
    pop.from.hiv.model = hiv.output.for.ncd$population[years,-1,,]  
    hiv.status = mc$DIM.NAMES.HIV[-1]
  } else {
    pop.from.hiv.model = hiv.output.for.ncd$population[years,,,]
    hiv.status = mc$DIM.NAMES.HIV
  }
  
  # NCD model output
  if(hiv.positive.population.only) {
    pop.from.ncd.model = return.gss(gss$n.hiv.prev)
    pop.from.ncd.model = aperm(pop.from.ncd.model, c(4,3,1,2))
    pop.from.ncd.model = pop.from.ncd.model[,-1,,]
  } else {
    pop.from.ncd.model = return.gss(gss$n.hiv.prev)
    pop.from.ncd.model = aperm(pop.from.ncd.model, c(4,3,1,2))
  }
  
  years = years
  ages = mc$DIM.NAMES.AGE
  sexes = c("FEMALE","MALE")
  
  full.dim.names = list(year = years,
                        hiv.status = hiv.status,
                        age = ages,
                        sex = sexes)
  hiv.dim.names =  full.dim.names[-1]
  
  
  # HIV model probabilities
  hiv.distr.from.hiv.model = array(0,
                                   dim = sapply(full.dim.names,length),
                                   dimnames = full.dim.names)
  
  for(i in 1:length(years)){
    hiv.probs.from.hiv.model = 
      sapply(1:length(hiv.dim.names$age), function(age){
        sapply(1:length(hiv.dim.names$sex), function(sex){
          rowSums(pop.from.hiv.model[i,,,],1)/sum(pop.from.hiv.model[i,,,])
        })
      })
    
    dim(hiv.probs.from.hiv.model) = sapply(hiv.dim.names,length)
    dimnames(hiv.probs.from.hiv.model) = hiv.dim.names
    
    # test
    # colSums(hiv.probs,1)
    
    hiv.distr.from.hiv.model[i,,,] = round(hiv.probs.from.hiv.model,5)
  }
  
  # NCD model probabilities
  hiv.distr.from.ncd.model = array(0,
                                   dim = sapply(full.dim.names,length),
                                   dimnames = full.dim.names)
  
  for(i in 1:length(years)){
    hiv.probs.from.ncd.model = 
      sapply(1:length(hiv.dim.names$age), function(age){
        sapply(1:length(hiv.dim.names$sex), function(sex){
          rowSums(pop.from.ncd.model[i,,,],1)/sum(pop.from.ncd.model[i,,,])
        })
      })
    
    dim(hiv.probs.from.ncd.model) = sapply(hiv.dim.names,length)
    dimnames(hiv.probs.from.ncd.model) = hiv.dim.names
    
    # test
    # colSums(hiv.probs,1)
    
    hiv.distr.from.ncd.model[i,,,] = round(hiv.probs.from.ncd.model,5)
  }
  
  hiv.df.for.plot = reshape2::melt(hiv.distr.from.hiv.model)
  hiv.df.for.plot$model = "hiv"
  
  ncd.df.for.plot = reshape2::melt(hiv.distr.from.ncd.model)
  ncd.df.for.plot$model = "ncd"
  
  df.for.plot = rbind(hiv.df.for.plot,ncd.df.for.plot)
  
  if(strata=="total"){
    # total
    plot = ggplot(df.for.plot, aes(fill=hiv.status, x = year, y = value)) + 
      geom_bar(position="fill", stat="identity") 
  }
  
  if(strata=="model"){
    # total
    plot = ggplot(df.for.plot, aes(fill=hiv.status, x = year, y = value)) + 
      geom_bar(position="fill", stat="identity")  + 
      facet_wrap(~model, scales = "free_y")
  }
  
  if(strata=="age"){
    # age
    plot = ggplot(df.for.plot, aes(fill=hiv.status, x = year, y = value)) + 
      geom_bar(position="fill", stat="identity") + 
      facet_wrap(~age, scales = "free_y")
  }
  
  if(strata=="sex"){
    # sex
    plot = ggplot(df.for.plot, aes(fill=hiv.status, x = year, y = value)) + 
      geom_bar(position="fill", stat="identity") + 
      facet_wrap(~sex, scales = "free_y")
  }
  
  print(plot)
}


plot.hiv.distribution(strata="model")

#return hiv state sizes for the current year
current.year.hiv.state.sizes = return.gss.hiv.state.sizes(pop)
#return hiv state sizes for the all years
all.years.hiv.state.sizes = return.gss(gss$n.hiv.prev)

