library(ggplot2)

plot.hiv.distribution = function(hiv.positive.population.only=T,
                                 compare.models=T,
                                 strata="total",
                                 graph.type="line",
                                 years=as.character(c(2015:2030))){
  
  
  if(hiv.positive.population.only) {
    # HIV model output 
    pop.from.hiv.model = hiv.output.for.ncd$population[years,-1,,]  
    
    # NCD model output 
    pop.from.ncd.model = return.gss(sim$gss$n.hiv.prev)
    pop.from.ncd.model = aperm(pop.from.ncd.model, c(4,3,1,2))
    pop.from.ncd.model = pop.from.ncd.model[,-1,,]
    
    hiv.status = mc$DIM.NAMES.HIV[-1]
    
  } else {
    # HIV model output
    pop.from.hiv.model = hiv.output.for.ncd$population[years,,,]
    
    # NCD model output
    pop.from.ncd.model = return.gss(sim$gss$n.hiv.prev)
    pop.from.ncd.model = aperm(pop.from.ncd.model, c(4,3,1,2))
    
    hiv.status = mc$DIM.NAMES.HIV
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
  
  # PLOTS# 
  # COMPARING MODELS # 
  if(compare.models){
    
    # LINE PLOTS # 
    if(graph.type=="line"){
      if(strata=="total"){
        plot = ggplot() + 
          geom_line(data=df.for.plot, aes(x = year, y = value ,color=model)) + 
          facet_wrap(~hiv.status, scales = "free_y") + 
          ylim(0,NA)
      } else if(strata=="age"){
        plot = ggplot() + 
          geom_line(data=df.for.plot, aes(x = year, y = value ,color=model)) + 
          facet_wrap(~hiv.status+age, scales = "free_y") + 
          ylim(0,NA)
      } else if(strata=="sex"){
        plot = ggplot() + 
          geom_line(data=df.for.plot, aes(x = year, y = value ,color=model)) + 
          facet_wrap(~hiv.status+sex, scales = "free_y") + 
          ylim(0,NA)
      }
      
      # BAR PLOTS # 
    } else if(graph.type=="bar"){
      if(strata=="total"){
        plot = ggplot(df.for.plot, aes(fill=hiv.status, x = year, y = value)) + 
          geom_bar(position="fill", stat="identity")  + 
          facet_wrap(~model, scales = "free_y")
      } else if(strata=="age"){
        plot = ggplot(df.for.plot, aes(fill=hiv.status, x = year, y = value)) + 
          geom_bar(position="fill", stat="identity")  + 
          facet_wrap(~model+age, scales = "free_y")
      } else if(strata=="sex"){
        plot = ggplot(df.for.plot, aes(fill=hiv.status, x = year, y = value)) + 
          geom_bar(position="fill", stat="identity")  + 
          facet_wrap(~model+sex, scales = "free_y")
      }
    }
    
    # NOT COMPARING MODELS # 
  } else { 
    
    # LINE PLOTS # 
    if(graph.type=="line"){
      if(strata=="total"){
        plot = ggplot() + 
          geom_line(data=df.for.plot, aes(x = year, y = value)) + 
          facet_wrap(~hiv.status, scales = "free_y") + 
          ylim(0,NA)
      } else if(strata=="age"){
        plot = ggplot() + 
          geom_line(data=df.for.plot, aes(x = year, y = value)) + 
          facet_wrap(~hiv.status+age, scales = "free_y") + 
          ylim(0,NA)
      } else if(strata=="sex"){
        plot = ggplot() + 
          geom_line(data=df.for.plot, aes(x = year, y = value)) + 
          facet_wrap(~hiv.status+sex, scales = "free_y") + 
          ylim(0,NA)
      }
      
      # BAR PLOTS # 
    } else if(graph.type=="bar"){
      if(strata=="total"){
        # total
        plot = ggplot(df.for.plot, aes(fill=hiv.status, x = year, y = value)) + 
          geom_bar(position="fill", stat="identity") 
      } else if(strata=="age"){
        # age
        plot = ggplot(df.for.plot, aes(fill=hiv.status, x = year, y = value)) + 
          geom_bar(position="fill", stat="identity") + 
          facet_wrap(~age, scales = "free_y")
      } else if(strata=="sex"){
        # sex
        plot = ggplot(df.for.plot, aes(fill=hiv.status, x = year, y = value)) + 
          geom_bar(position="fill", stat="identity") + 
          facet_wrap(~sex, scales = "free_y")
      }
    }
  }
  plot
}

# don't know how to suppres the return.gss output otherwise 
print.plot.only = capture.output({
  
  # LINE PLOTS 
  # HIV positive population only 
  plot1 = plot.hiv.distribution(hiv.positive.population.only=T,
                                compare.models=T,
                                strata="total",
                                graph.type="line")
  
  plot2 = plot.hiv.distribution(hiv.positive.population.only=T,
                                compare.models=T,
                                strata="sex",
                                graph.type="line")
  
  plot3 = plot.hiv.distribution(hiv.positive.population.only=T,
                                compare.models=T,
                                strata="age",
                                graph.type="line")
  
  # Including HIV negative 
  plot4 = plot.hiv.distribution(hiv.positive.population.only=F,
                                compare.models=T,
                                strata="total",
                                graph.type="line")
  
  plot5 = plot.hiv.distribution(hiv.positive.population.only=F,
                                compare.models=T,
                                strata="sex",
                                graph.type="line")
  
  plot6 = plot.hiv.distribution(hiv.positive.population.only=F,
                                compare.models=T,
                                strata="age",
                                graph.type="line")
  
  # BAR PLOTS
  # HIV positive population only 
  plot7 = plot.hiv.distribution(hiv.positive.population.only=T,
                                compare.models=T,
                                strata="total",
                                graph.type="bar")
  
  plot8 = plot.hiv.distribution(hiv.positive.population.only=T,
                                compare.models=T,
                                strata="sex",
                                graph.type="bar")
  
  plot9 = plot.hiv.distribution(hiv.positive.population.only=T,
                                compare.models=T,
                                strata="age",
                                graph.type="bar")
  
  # Including HIV negative 
  plot10 = plot.hiv.distribution(hiv.positive.population.only=F,
                                compare.models=T,
                                strata="total",
                                graph.type="bar")
  
  plot11 = plot.hiv.distribution(hiv.positive.population.only=F,
                                compare.models=T,
                                strata="sex",
                                graph.type="bar")
  
  plot12 = plot.hiv.distribution(hiv.positive.population.only=F,
                                compare.models=T,
                                strata="age",
                                graph.type="bar")
})

# can cycle through plots 1-12 here, suppresses gss output 

if(1==2){
  # Line plots
  # HIV positive population only 
  print(plot1) #total
  print(plot2) #sex
  print(plot3) #age
  # Including HIV negative 
  print(plot4) #total
  print(plot5) #sex
  print(plot6) #age
  
  #Bar plots - less helpful for comparing 
  # HIV positive population only 
  print(plot7) #total
  print(plot8) #sex
  print(plot9) #age
  # Including HIV negative 
  print(plot10) #total
  print(plot11) #sex
  print(plot12) #age
}



