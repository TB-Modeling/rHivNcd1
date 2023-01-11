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
      
      # if this is a single simulation, need to make it a list with one element
      if(class(sim[[1]])=="array"){
        sim = list(sim)
      }
      
      
      ##----------------------##
      ##----- HIV OUTPUT -----##
      ##----------------------##
      # HIV SIMSET OBJECT OR INDIVIDUAL HIV SIMULATION
      
      # wanted to say if(sim[[1]]$model=="hiv"), but that doesn't work for ncd sims
      if("hiv" %in% sim[[1]]) { 
        
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
  simplot(khm,sim1,facet.by="age",scale.population = T)
  simplot(khm,facet.by=c("age","sex"),sexes="MALE",scale.population = T)
  
  simplot(khm,facet.by=c("age","sex"),ages="10-14",scale.population = T)
  
  simplot(khm,sim1, facet.by="hiv.status")
  
}