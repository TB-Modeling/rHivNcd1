#
#  R HIVNCD 2022
#  Plots
#  
#####################################
print("Sourcing Plot.R ... ")
source("extract_data.R")

library(ggplot2)

#'@PK - moved this function from extract data; only HIV model output now

# Function to extract data from HIV model (khm) output object - either a simset or a single sim (for now, if it's a simset, take only one sim)
# Specify the khm.output object, what ages/sexes/hiv statuses/years to include, and then what dimensions to report by 
return.khm.state.size.distribution = function(khm.output, # object from khm model including all state sizes for all years
                                              ages = mc$DIM.NAMES.AGE, # ages to return
                                              sexes = mc$DIM.NAMES.SEX, # sexes to return 
                                              hiv.status = mc$DIM.NAMES.HIV, # hiv status to return
                                              ncd.status = mc$DIM.NAMES.NCD,
                                              years=as.character(DIM.NAMES.N), # years to return 
                                              keep.dimensions = 'year') # collapse all other dimensions & report the data as total value over this dimension
{ 
  
  if(all(keep.dimensions!='year'))
    stop("must keep year dimension")
  
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
  
  # simset object
  if(is(khm.output,"khm_simulation_output")) {
    rv = list()
    
    for(i in 1:length(khm.output)){
      x=khm.output[[i]]$population 
      x=aperm(x,c(3,4,2,1)) # reorder to have same dimension order as ncd output: age, sex, hiv.status, year
      x=x[ages,sexes,hiv.status,years]
      
      # have to add dimnames back in just in case any dimension was reduced down to just one stratum (R then drops that dimension)
      dim.names = list(age=ages,
                       sex=sexes,
                       hiv.status=hiv.status,
                       year=years)
      dim(x)=sapply(dim.names,length)
      dimnames(x)=dim.names
      
      #summing over dimensions that are to keep
      rv[[i]] = apply(x, keep.dimensions, sum)
      #adjusting dimension names and 
      dim(rv[[i]]) = sapply(keep.dim.names, length)
      dimnames(rv[[i]]) = keep.dim.names
    }
    
  } else {
    
    x=khm.output$population 
    x=aperm(x,c(3,4,2,1)) # reorder to have same dimension order as ncd output: age, sex, hiv.status, year
    x=x[ages,sexes,hiv.status,years]
    
    # have to add dimnames back in just in case any dimension was reduced down to just one stratum (R then drops that dimension)
    dim.names = list(age=ages,
                     sex=sexes,
                     hiv.status=hiv.status,
                     year=years)
    dim(x)=sapply(dim.names,length)
    dimnames(x)=dim.names
    
    #summing over dimensions that are to keep
    rv = apply(x, keep.dimensions, sum)
    #adjusting dimension names and 
    dim(rv) = sapply(keep.dim.names, length)
    dimnames(rv) = keep.dim.names
  }
  
  rv
}


simplot = function(...,
                   years = as.character(2015:2030),
                   data.types = c("population"),
                   scale.population = F,
                   facet.by = NULL,
                   split.by = NULL,
                   ages = mc$DIM.NAMES.AGE, 
                   sexes = mc$DIM.NAMES.SEX,
                   hiv.status = mc$DIM.NAMES.HIV,
                   ncd.status = mc$DIM.NAMES.NCD
){
  sims = list(...)
  keep.dimensions = union('year',union(facet.by, split.by))
  
  df.sim = NULL
    for(i in 1:length(sims)){
      sim = sims[[i]]
      
      if(is(sim,"hiv_simulation")){
        sim = list(sim)
        class(sim) = "hiv_simulation"
      }
      
      
      ##----------------------##
      ##----- HIV OUTPUT -----##
      ##----------------------##
      # HIV SIMSET OBJECT OR INDIVIDUAL HIV SIMULATION
      
      if(is(sim,"khm_simulation_output") | is(sim,"hiv_simulation")) { 
        
        for(j in 1:length(sim)){
          value = return.khm.state.size.distribution(khm.output=sim[[j]], 
                                                     ages = ages, 
                                                     sexes = sexes,
                                                     hiv.status = hiv.status, 
                                                     # ncd.status = ncd.status,
                                                     years=years, 
                                                     keep.dimensions = keep.dimensions)
          
          
          # For scale.population, divide by 2015 population size - have to do this separately for each combo of keep.dimensions
          if(scale.population){
            
            # Keep dimensions = year only 
            if(setequal(keep.dimensions,"year")){
              value = value/value[years=="2015"]
              
              # 2 keep dimensions 
            } else if(length(keep.dimensions)==2){
              value = sapply(1:dim(value)[2],function(j){
                sapply(1:dim(value)[1],function(i){
                  value[i,j]/value["2015",j]
                })
              })
              if (setequal(keep.dimensions, c('year','age'))){
                dimnames(value) = list(year=years,
                                       age=ages)
              } else if (setequal(keep.dimensions, c('year','sex'))){ 
                dimnames(value) = list(year=years,
                                       sex=sexes)
              } else if (setequal(keep.dimensions, c('year','hiv.status'))){
                dimnames(value) = list(year=years,
                                       hiv.status=hiv.status)
              } else stop("need to add these dimensions")
            

              # 3 keep dimensions
            } else if(length(keep.dimensions)==3){
              value = sapply(1:dim(value)[3],function(k){
                sapply(1:dim(value)[2],function(j){
                  sapply(1:dim(value)[1],function(i){
                    value[i,j,k]/value["2015",j,k]  
                  })
                })
              })
              if (setequal(keep.dimensions, c('year','age','sex'))){
                if(!all(keep.dimensions==c("year","age","sex")))
                  stop("keep.dimensions must be in the order: year, age, sex")
                dim.names = list("year"=years,
                                 "age"=ages,
                                 "sex"=sexes)
                dim(value) = sapply(dim.names,length)
                dimnames(value) = dim.names
              } else stop("need to add these dimensions")
            }
          }
          
          
          # set up a dataframe with columns: year, value, sim id, data.type 
          one.df = reshape2::melt(value) 
          one.df$sim.id = i
          one.df$sim.number = j
          # one.df$data.type = d
          
          df.sim = rbind(df.sim, one.df)  

        }
        
      
        ##----------------------##
        ##----- NCD OUTPUT -----##
        ##----------------------##
        # have to repeat everything because hiv sims are within a for loop of the length of sim (ncd sims have their own length)
      } else {
        value = return.gss.state.size.distribution(sim=sim,
                                                   years = years, 
                                                   ages=ages, 
                                                   sexes = sexes, 
                                                   hiv.status = hiv.status,
                                                   ncd.status=ncd.status,
                                                   keep.dimensions = keep.dimensions)
        
        # For scale.population, divide by 2015 population size - have to do this separately for each combo of keep.dimensions
        if(scale.population){
          
          if(setequal(keep.dimensions,"year")){
            value = value/value[years=="2015"]
            
          } else if (setequal(keep.dimensions, c('year','age'))){
            value = sapply(1:dim(value)[2],function(j){
              sapply(1:dim(value)[1],function(i){
                value[i,j]/value["2015",j]
              })
            })
            dimnames(value) = list(year=years,
                                   age=ages)
            
          } else if (setequal(keep.dimensions, c('year','sex'))){
            value = sapply(1:dim(value)[2],function(j){
              sapply(1:dim(value)[1],function(i){
                value[i,j]/value["2015",j]
              })
            })
            dimnames(value) = list(year=years,
                                   sex=sexes)
            
          } else if (setequal(keep.dimensions, c('year','hiv.status'))){
            value = sapply(1:dim(value)[2],function(j){
              sapply(1:dim(value)[1],function(i){
                value[i,j]/value["2015",j]
              })
            })
            dimnames(value) = list(year=years,
                                   hiv.status=hiv.status)
            
          } else if (setequal(keep.dimensions, c('year','age','sex'))){
            
            if(!all(keep.dimensions==c("year","age","sex")))
              stop("keep.dimensions must be in the order: year, age, sex")
            
            value = sapply(1:dim(value)[3],function(k){
              sapply(1:dim(value)[2],function(j){
                sapply(1:dim(value)[1],function(i){
                  value[i,j,k]/value["2015",j,k]  
                })
              })
            })
            
            dim.names = list("year"=years,
                             "age"=ages,
                             "sex"=sexes)
            dim(value) = sapply(dim.names,length)
            dimnames(value) = dim.names
            
          } else stop("need to add these dimensions")
        }
        
        # set up a dataframe with columns: year, value, sim id, data.type 
        one.df = reshape2::melt(value) 
        one.df$sim.id = i
        one.df$sim.number = 1 # hard coding 1 for sim number for ncd since we don't do simsets
        # one.df$data.type = d
        
        df.sim = rbind(df.sim, one.df)   
        
      }


    }
      
  df.sim$sim.id = as.character(df.sim$sim.id)
  df.sim$group.id = paste0("sim ",df.sim$sim.id,"_",df.sim$sim.number)

  for(s in split.by){
    df.sim$group.id = paste0(df.sim$group.id,", ",s,"=",df.sim[,s])
  }
  
  # setting up facet.by
  
  if(length(facet.by)>0){
    facet_string = paste0("~",paste0(facet.by,collapse = '+'))
    facet_formula = as.formula(facet_string)
    plot = ggplot() + 
      geom_line(data = df.sim, aes(x = year, y = value, color = sim.id, group = group.id)) +
      facet_wrap(facet_formula, scales = "free_y") + 
      ylim(0,NA)
  } else{
    plot = ggplot() + 
      geom_line(data = df.sim, aes(x = year, y = value, color = sim.id, group = group.id)) +
      ylim(0,NA)
  }

  plot


}


if(1==2){
  #'@PK - sample code below
  #'For some reason the ncd simulation/return.gss.state.size.distribution function is returning all 0's?
  simplot(khm,sim1,facet.by="age",scale.population = T)
  simplot(khm,facet.by=c("age","sex"),sexes="MALE",scale.population = T)
  
  simplot(khm,sim1, facet.by="hiv.status")
  
}











if(1==2){
##----------------------------------##
##------- OLD PLOTTING CODE --------##
##----------------------------------##

  if(1==2){
    simplot(sim1,sim2)
    # Plot population for multiple simulations by total, age, and sex
    simplot(sim1,sim2,hiv.sim,facet.by = "sex")
    simplot(sim1,sim2,data.types = "hiv.prevalence",facet.by="age")
    simplot(sim1,sim2,hiv.sim,data.types = "hiv.prevalence")
    
    simplot(sim1,sim2,hiv.sim,facet.by = "age")
    simplot(hiv.sim,data.types = "hiv.prevalence",facet.by="sex")
    
    # CHECKING POPULATION GROWTH - CAN ONLY CHECK AT TOTAL LEVEL FOR NOW
    simplot(sim1,sim2,hiv.sim,scale.population=T)
    
  }
  
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