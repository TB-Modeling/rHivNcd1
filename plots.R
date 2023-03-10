#
#  R HIVNCD 2022
#  Plots
#  
#####################################
print("Sourcing Plot.R ... ")

library(ggplot2)

# Function to extract data from HIV model (khm) output object - either a simset or a single sim (for now, if it's a simset, take only one sim)
# Specify the khm.output object, what ages/sexes/hiv statuses/years to include, and then what dimensions to report by 
return.khm.state.size.distribution = function(khm.output, # object from khm model including all state sizes for all years
                                              ages = DIM.NAMES.AGE, # ages to return
                                              sexes = DIM.NAMES.SEX, # sexes to return 
                                              hiv.status = DIM.NAMES.HIV, # hiv status to return
                                              ncd.status = DIM.NAMES.NCD,
                                              years=as.character(DIM.NAMES.YEARS), # years to return 
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
                   ages = DIM.NAMES.AGE, 
                   sexes = DIM.NAMES.SEX,
                   hiv.status = DIM.NAMES.HIV,
                   ncd.status = DIM.NAMES.NCD
){
  sims = list(...)
  keep.dimensions = union('year',union(facet.by, split.by))
  
  df.sim = NULL
  
    for(i in 1:length(sims)){
      sim = sims[[i]]

      ##----------------------##
      ##----- NCD OUTPUT -----##
      ##----------------------##
      # have to repeat everything because hiv sims are within a for loop of the length of sim (ncd sims have their own length)
      if("R6" %in% class(sim)){
        value = filter.stateSizes.by.field(sim$stats$n.state.sizes, 
                                           years = years,
                                           ages = ages, 
                                           sexes = sexes,
                                           hiv.status = hiv.status,
                                           ncd.status = ncd.status,
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
            } else if (setequal(keep.dimensions, c('year','ncd.status'))){
              dimnames(value) = list(year=years,
                                     ncd.status=ncd.status)
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
        one.df$sim.number = 1 # hard coding 1 for sim number for ncd since we don't do simsets
        # one.df$data.type = d
        
        df.sim = rbind(df.sim, one.df)   
        
        
        ##----------------------##
        ##----- HIV OUTPUT -----##
        ##----------------------##
        
        # HIV SIMSET OBJECT OR INDIVIDUAL HIV SIMULATION
      } else {
        
        # if this is a single simulation, need to make it a list with one element
        if(class(sim[[1]])=="array"){
          sim = list(sim)     
        }
        
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

  suppressWarnings(print(plot))


}


if(1==2){
  simplot(pop$params$khm.full,pop,scale.population = T)
  simplot(pop$params$khm.full,pop,scale.population = T, facet.by = "age")
  simplot(pop$params$khm.full,pop,scale.population = T, facet.by = "sex")
  simplot(pop$params$khm.full,pop,scale.population = T, facet.by="hiv.status")
  simplot(pop,scale.population = T, facet.by="ncd.status") 
  
}


# plot.pop.barchart=function(pop,
#                       facet.by = NULL,
#                       ages = DIM.NAMES.AGE, 
#                       sexes = DIM.NAMES.SEX,
#                       hiv.status = DIM.NAMES.HIV,
#                       ncd.status = DIM.NAMES.NCD){
#   
#   keep.dimensions = union('year',facet.by)
#   
#   value = filter.stateSizes.by.field(pop$stats$n.state.sizes, 
#                                          years = pop$params$YNOW,
#                                          ages = ages, 
#                                          sexes = sexes,
#                                          hiv.status = hiv.status,
#                                          ncd.status = ncd.status,
#                                          keep.dimensions = keep.dimensions)
#       
#   # set up a dataframe with columns: year, value, sim id, data.type 
#   df = reshape2::melt(value) 
#   
# 
#   if(length(facet.by)>0){
#     facet_string = paste0("~",paste0(facet.by,collapse = '+'))
#     facet_formula = as.formula(facet_string)
#     plot = ggplot() + 
#       geom_point(data = df, aes(x=year,y = value,fill="red")) +
#       facet_wrap(facet_formula, scales = "free_y")+ 
#       ylim(0,NA)
#   } else{
#     plot = ggplot() + 
#       geom_point(data = df, aes(x=year,y = value,fill="red"))+
#       ylim(0,NA)
#   }
#   
#   plot
# }
# plot.pop.barchart(pop,facet.by = c("sex"))
# plot.pop.barchart(pop,facet.by = c("sex","age"))


# plot.pop.hist.1D=function(pop,
#                            by.dimension = "age",
#                            ages = DIM.NAMES.AGE, 
#                            sexes = DIM.NAMES.SEX,
#                            hiv.status = DIM.NAMES.HIV,
#                            ncd.status = DIM.NAMES.NCD){
#   
#   if (length(by)!=1) {
#     print("please only specify one by.dimension")
#     return (0)
#   }
#   
#   keep.dimensions = union('year',by.dimension)
#   value = filter.stateSizes.by.field(pop$stats$n.state.sizes, 
#                                      years = pop$params$YNOW,
#                                      ages = ages, 
#                                      sexes = sexes,
#                                      hiv.status = hiv.status,
#                                      ncd.status = ncd.status,
#                                      keep.dimensions = keep.dimensions)
#   
#   # set up a dataframe with columns: year, value, sim id, data.type 
#   df = reshape2::melt(value) 
#     plot = ggplot() + 
#       geom_bar(data = df, aes(x=???,y = value),stat="identity")
#       # facet_wrap(facet_formula, scales = "free_y")+ 
#       # ylim(0,NA)
#     plot
# }