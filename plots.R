#
#  R HIVNCD 2022
#  Plots
#  
#####################################
print("Sourcing Plot.R ... ")
source("extract_data.R")

library(ggplot2)


simplot = function(...,
                   years = as.character(2015:2030),
                   data.types = c("population"),
                   facet.by = NULL,
                   split.by = NULL,
                   ages = mc$DIM.NAMES.AGE, 
                   sexes = mc$DIM.NAMES.SEX,
                   hiv.status = mc$DIM.NAMES.HIV
){
  sims = list(...)
  keep.dimensions = union('year',union(facet.by, split.by))
  

  ##----------------------##
  ##----- SIM OUTPUT -----##
  ##----------------------##
  
  df.sim = NULL
  for(d in data.types){
    for(i in 1:length(sims)){

      if(is(sims[[i]],"simset")) # if this is an MCMC results, i.e., a simset
        sims.for.i = simset@simulations
      
      # for plotting single simulations
      else {
        sim = sims[[i]]
        sims.for.i = list(sim)}
      
      for(j in 1:length(sims.for.i)){
        
        sim = sims.for.i[[j]]
        
        # Assign model type based on whether the class of the sim contains "hiv" in it
        if(grepl("hiv",class(sim))){
          model="hiv"
        } else
          model="ncd"
        
        # Extract the data from simulation
        value = extract.data(sim,
                             model=model,
                             years = years, 
                             age=ages, 
                             sex = sexes, 
                             hiv.status = hiv.status, 
                             data.type=d, 
                             keep.dimensions = keep.dimensions)
        
        # set up a dataframe with columns: year, value, sim id, data.type 
        one.df = reshape2::melt(value) 
        one.df$sim.id = i
        one.df$sim.number = j
        one.df$data.type = d
        
        df.sim = rbind(df.sim, one.df)   
      }
      
    }
  }
  
  df.sim$sim.id = as.character(df.sim$sim.id)
  df.sim$group.id = paste0("sim ",df.sim$sim.id,"_",df.sim$sim.number)

  for(s in split.by){
    df.sim$group.id = paste0(df.sim$group.id,", ",s,"=",df.sim[,s])
  }
  
  # setting up facet.by
  facet_string = '~data.type'
  if(length(facet.by)>0)
    facet_string = paste0(facet_string, '+', paste0(facet.by,collapse = '+'))
  facet_formula = as.formula(facet_string)
  
  ggplot() + 
    geom_line(data = df.sim, aes(x = year, y = value, color = sim.id, group = group.id)) +
    facet_wrap(facet_formula, scales = "free_y") + 
    ylim(0,NA)

}




if(1==2){
  #'@PK - sample code below.
  simplot(sim1,sim2)
  # Plot population for multiple simulations by total, age, and sex
  simplot(sim1,sim2,hiv.sim,facet.by = "sex")
  simplot(sim1,sim2,data.types = "hiv.prevalence",facet.by="age")
  simplot(sim1,sim2,hiv.sim,data.types = "hiv.prevalence")
  
  simplot(sim1,sim2,hiv.sim,facet.by = "age")
  simplot(hiv.sim,data.types = "hiv.prevalence",facet.by="sex")
  
}









if(1==2){
##----------------------------------##
##------- OLD PLOTTING CODE --------##
##----------------------------------##

# CURRENTLY SPLITTING INTO TWO FUNCTIONS 
extract.data.for.comparison.plots = function(sim,
                                             data.type,
                                             hiv.positive.population.only, # T removes HIV negative population from plots; F keeps them in
                                             strata,
                                             years){
  if(data.type=="hiv.distribution"){
    # 1- extract the data from HIV model and the NCD model ----------------
    if(hiv.positive.population.only) {
      # HIV model output 
      pop.from.hiv.model = hiv.output.for.ncd$population[years,-1,,]  
      
      # NCD model output 
      pop.from.ncd.model = sim$gss$n.hiv.prev
      pop.from.ncd.model = aperm(pop.from.ncd.model, c(4,3,1,2))
      pop.from.ncd.model = pop.from.ncd.model[,-1,,]
      
      hiv.status = mc$DIM.NAMES.HIV[-1]
      
    } else {
      # HIV model output
      pop.from.hiv.model = hiv.output.for.ncd$population[years,,,]
      
      # NCD model output
      pop.from.ncd.model = sim$gss$n.hiv.prev
      pop.from.ncd.model = aperm(pop.from.ncd.model, c(4,3,1,2))
      
      hiv.status = mc$DIM.NAMES.HIV
    }

    
    
    
    # 2- prepare freq vs proportions for plotting -----------------
    {
      years = years
      ages = mc$DIM.NAMES.AGE
      sexes = mc$DIM.NAMES.SEX
      full.dim.names = list(year = years,
                            hiv.status = hiv.status,
                            age = ages,
                            sex = sexes)
      one.year.dim.names =  full.dim.names[-1]  #'@MS: so exluding years? (YES - to loop through years)
      
      # TOTAL PROPORTIONS
      if (setequal(strata, c('total'))){
        
        dim.names = list(year = years,
                         hiv.status = hiv.status) 
        one.year.dim.names = list(hiv.status = hiv.status)
        
        # HIV model probabilities
        hiv.distr.from.hiv.model = array(0,
                                         dim = sapply(dim.names,length),
                                         dimnames = dim.names)
        
        # NCD model probabilities
        hiv.distr.from.ncd.model = array(0,
                                         dim = sapply(dim.names,length),
                                         dimnames = dim.names)
        
        for(i in 1:length(years)){
          # HIV
          hiv.probs.from.hiv.model = 
            rowSums(pop.from.hiv.model[i,,,],1)/sum(pop.from.hiv.model[i,,,])
          dim(hiv.probs.from.hiv.model) = sapply(one.year.dim.names,length)
          dimnames(hiv.probs.from.hiv.model) = one.year.dim.names 
          hiv.distr.from.hiv.model[i,] = hiv.probs.from.hiv.model
          
          #NCD
          hiv.probs.from.ncd.model = 
            rowSums(pop.from.ncd.model[i,,,],1)/sum(pop.from.ncd.model[i,,,])
          dim(hiv.probs.from.ncd.model) = sapply(one.year.dim.names,length)
          dimnames(hiv.probs.from.ncd.model) = one.year.dim.names
          hiv.distr.from.ncd.model[i,] = hiv.probs.from.ncd.model
        }
      }
      
      
      
      
      
      # PROPORTIONS BY AGE AND SEX
      if (setequal(strata, c('age','sex'))){
        
        # HIV model probabilities
        hiv.distr.from.hiv.model = array(0,
                                         dim = sapply(full.dim.names,length),
                                         dimnames = full.dim.names)
        
        # NCD model probabilities
        hiv.distr.from.ncd.model = array(0,
                                         dim = sapply(full.dim.names,length),
                                         dimnames = full.dim.names)
        
        for(i in 1:length(years)){
          # HIV
          hiv.probs.from.hiv.model = 
            sapply(1:length(one.year.dim.names$age), function(age){
              sapply(1:length(one.year.dim.names$sex), function(sex){
                pop.from.hiv.model[i,,age,sex]/sum(pop.from.hiv.model[i,,age,sex])
              })
            })
          dim(hiv.probs.from.hiv.model) = sapply(one.year.dim.names,length)
          dimnames(hiv.probs.from.hiv.model) = one.year.dim.names #'@MS: I think that this is incorrect - (I think I fixed it?)
          hiv.distr.from.hiv.model[i,,,] = hiv.probs.from.hiv.model
          
          #NCD
          hiv.probs.from.ncd.model = 
            sapply(1:length(one.year.dim.names$age), function(age){
              sapply(1:length(one.year.dim.names$sex), function(sex){
                pop.from.ncd.model[i,,age,sex]/sum(pop.from.ncd.model[i,,age,sex])
              })
            })
          dim(hiv.probs.from.ncd.model) = sapply(one.year.dim.names,length)
          dimnames(hiv.probs.from.ncd.model) = one.year.dim.names
          hiv.distr.from.ncd.model[i,,,] = hiv.probs.from.ncd.model
        }
      }
    }
    
    # prepare data for plotting
    hiv.df.for.plot = reshape2::melt(hiv.distr.from.hiv.model)
    hiv.df.for.plot$model = "hiv"
    
    ncd.df.for.plot = reshape2::melt(hiv.distr.from.ncd.model)
    ncd.df.for.plot$model = "ncd"
    
    df.for.plot = rbind(hiv.df.for.plot,ncd.df.for.plot)
    
  } else if(data.type=="population"){
    stop("not set up for population plots yet")
  }
  
  
  
}


plot.model.comparisons = function(sim,
                                  data.type,
                                  hiv.positive.population.only=T, # T removes HIV negative population from plots; F keeps them in
                                  strata="total", # total, age, or sex
                                  years=as.character(c(2015:2030)),
                                  ylim.from.zero=T){
  if(data.type=="hiv.distribution"){
    df.for.plot = extract.data.for.comparison.plots(sim = sim,
                                                    data.type = data.type,
                                                    hiv.positive.population.only = hiv.positive.population.only,
                                                    strata = strata,
                                                    years=years)
    
  } else if(data.type=="population"){
    stop("not set up for population plots yet")
  }

  
  # PLOTS# 
  if (setequal(strata, c('total'))){
    plot = ggplot() + 
      geom_line(data=df.for.plot, aes(x = year, y = value ,color=model)) + 
      facet_wrap(~hiv.status, scales = "free_y") 
  } else if (setequal(strata, c('age','sex'))){
    plot = ggplot() + 
      geom_line(data=df.for.plot, aes(x = year, y = value ,color=model)) + 
      facet_wrap(~hiv.status+age+sex, scales = "free_y") 
  } else if (setequal(strata, c('age'))){
    plot = ggplot() + 
      geom_line(data=df.for.plot, aes(x = year, y = value ,color=model)) + 
      facet_wrap(~hiv.status+age, scales = "free_y") 
  } else if (setequal(strata, c('sex'))){
    plot = ggplot() + 
      geom_line(data=df.for.plot, aes(x = year, y = value ,color=model)) + 
      facet_wrap(~hiv.status+sex, scales = "free_y")  
  }
  if (ylim.from.zero==T) plot=plot+ylim(0,NA)
  
  plot
}

plot.model.comparisons(sim=sim1,
                       data.type = "hiv.distribution",
                       hiv.positive.population.only=T,
                       strata="total")

if(1==2){
  print.plot.only = capture.output({
    
    # LINE PLOTS 
    # HIV positive population only 
    plot1 = plot.hiv.distribution(sim=sim1,
                                  hiv.positive.population.only=T,
                                  compare.models=T,
                                  strata="total",
                                  graph.type="line")
    
    
    plot2 = plot.hiv.distribution(sim=sim1,
                                  hiv.positive.population.only=T,
                                  compare.models=T,
                                  strata="sex",
                                  graph.type="line")
    
    plot3 = plot.hiv.distribution(sim=sim1,
                                  hiv.positive.population.only=T,
                                  compare.models=T,
                                  strata="age",
                                  graph.type="line")
    
    # Including HIV negative 
    plot4 = plot.hiv.distribution(sim=sim1,
                                  hiv.positive.population.only=F,
                                  compare.models=T,
                                  strata="total",
                                  graph.type="line")
    
    plot5 = plot.hiv.distribution(sim=sim1,
                                  hiv.positive.population.only=F,
                                  compare.models=T,
                                  strata="sex",
                                  graph.type="line")
    
    plot6 = plot.hiv.distribution(sim=sim1,
                                  hiv.positive.population.only=F,
                                  compare.models=T,
                                  strata="age",
                                  graph.type="line")
    
})
}
  

# don't know how to supress the return.gss output otherwise 

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
}


# old code for bar plots
if(1==2){
  
  
  # BAR PLOTS # 
  if(graph.type=="bar"){
    if (setequal(strata, c('total'))){
      plot = ggplot(df.for.plot, aes(fill=hiv.status, x = year, y = value)) + 
        geom_bar(position="fill", stat="identity")  + 
        facet_wrap(~model, scales = "free_y")
    } else if (setequal(strata, c('age'))){
      plot = ggplot(df.for.plot, aes(fill=hiv.status, x = year, y = value)) + 
        geom_bar(position="fill", stat="identity")  + 
        facet_wrap(~model+age, scales = "free_y")
    } else if (setequal(strata, c('sex'))){
      plot = ggplot(df.for.plot, aes(fill=hiv.status, x = year, y = value)) + 
        geom_bar(position="fill", stat="identity")  + 
        facet_wrap(~model+sex, scales = "free_y")
    }
  }
  
  # BAR PLOTS
  # HIV positive population only 
  plot7 = plot.hiv.distribution(sim=sim1,
                                hiv.positive.population.only=T,
                                compare.models=T,
                                strata="total",
                                graph.type="bar")
  
  plot8 = plot.hiv.distribution(sim=sim1,
                                hiv.positive.population.only=T,
                                compare.models=T,
                                strata="sex",
                                graph.type="bar")
  
  plot9 = plot.hiv.distribution(sim=sim1,
                                hiv.positive.population.only=T,
                                compare.models=T,
                                strata="age",
                                graph.type="bar")
  
  # Including HIV negative 
  plot10 = plot.hiv.distribution(sim=sim1,
                                 hiv.positive.population.only=F,
                                 compare.models=T,
                                 strata="total",
                                 graph.type="bar")
  
  plot11 = plot.hiv.distribution(sim=sim1,
                                 hiv.positive.population.only=F,
                                 compare.models=T,
                                 strata="sex",
                                 graph.type="bar")
  
  plot12 = plot.hiv.distribution(sim=sim1,
                                 hiv.positive.population.only=F,
                                 compare.models=T,
                                 strata="age",
                                 graph.type="bar")
  
  
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
  
  
}