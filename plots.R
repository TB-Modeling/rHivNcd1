#
#  R HIVNCD 2022
#  Plots
#  
#####################################
print("Sourcing Plot.R ... ")

# library(ggplot2)

# Function to extract data from HIV model (khm) output object - either a simset or a single sim (for now, if it's a simset, take only one sim)
# Specify the khm.output object, what ages/sexes/hiv statuses/years to include, and then what dimensions to report by 
return.khm.data = function(khm.output, # object from khm model including all state sizes for all years
                           data.type,
                           ages = DIM.NAMES.AGE, # ages to return
                           sexes = DIM.NAMES.SEX, # sexes to return 
                           hiv.status = DIM.NAMES.HIV, # hiv status to return
                           ncd.status = DIM.NAMES.NCD,
                           years=as.character(DIM.NAMES.YEARS), # years to return 
                           keep.dimensions = 'year') # collapse all other dimensions & report the data as total value over this dimension
{ 
  
  if(all(keep.dimensions!='year'))
    stop("Must keep year dimension")
  
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
      x=khm.output[[i]][[data.type]] 
      if(data.type=="population"){
        x=aperm(x,c(3,4,2,1)) # reorder to have same dimension order as ncd output: age, sex, hiv.status, year
        x=x[ages,sexes,hiv.status,years]  
        
        # have to add dimnames back in just in case any dimension was reduced down to just one stratum (R then drops that dimension)
        dim.names = list(age=ages,
                         sex=sexes,
                         hiv.status=hiv.status,
                         year=years)
      } else {
        x=aperm(x,c(2,3,1)) # reorder to have same dimension order as ncd output: age, sex, year
        x=x[ages,sexes,years] 
        
        dim.names = list(age=ages,
                         sex=sexes,
                         year=years)
      }
      dim(x)=sapply(dim.names,length)
      dimnames(x)=dim.names
      
      #summing over dimensions that are to keep
      rv[[i]] = apply(x, keep.dimensions, sum)
      #adjusting dimension names and 
      dim(rv[[i]]) = sapply(keep.dim.names, length)
      dimnames(rv[[i]]) = keep.dim.names
    }
    
  } else {
    
    x=khm.output[[data.type]] 
    if(data.type=="population"){
      x=aperm(x,c(3,4,2,1)) # reorder to have same dimension order as ncd output: age, sex, hiv.status, year
      x=x[ages,sexes,hiv.status,years]  
      
      # have to add dimnames back in just in case any dimension was reduced down to just one stratum (R then drops that dimension)
      dim.names = list(age=ages,
                       sex=sexes,
                       hiv.status=hiv.status,
                       year=years)
    } else {
      x=aperm(x,c(2,3,1)) # reorder to have same dimension order as ncd output: age, sex, year
      x=x[ages,sexes,years] 
      
      dim.names = list(age=ages,
                       sex=sexes,
                       year=years)
    }
    
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
                   data.type = c("population"),
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
  
  ncd.data.types = list("population"="n.state.sizes",
                        "incidence"="n.hiv.inc",
                        "prevalence"="n.state.sizes",
                        "hyp.inc" = "n.hyp.inc",
                        "diab.inc" = "n.diab.inc",
                        "diab.hyp.inc" = "n.diab.hyp.inc",
                        "mi.inc" = "n.mi.inc",
                        "stroke.inc" = "n.stroke.inc")
  
  df.sim = NULL
  
  for(i in 1:length(sims)){
    sim = sims[[i]]
    
    ##----------------------##
    ##----- NCD OUTPUT -----##
    ##----------------------##
  
    # if this is a single simulation, need to make it a list with one element
    if("R6" %in% class(sim)){
      sim = list(sim)   
    }
    
    if("R6" %in% class(sim[[1]])){
      for(j in 1:length(sim)){
      
        ncd.data.type.x = ncd.data.types[[data.type]]
        if(is.null(ncd.data.type.x))
          stop("Haven't set up to pull this data type from ncd model")
        
        if(data.type %in% c("population","incidence","mi.inc","stroke.inc","hyp.inc","diab.inc","diab.hyp.inc")){
          value = filter.5D.stats.by.field(sim[[j]]$stats[[ncd.data.type.x]], 
                                           years = years,
                                           ages = ages, 
                                           sexes = sexes,
                                           hiv.status = hiv.status,
                                           ncd.status = ncd.status,
                                           keep.dimensions = keep.dimensions)
        } else if(data.type=="prevalence"){
          hiv.status = DIM.NAMES.HIV[-1]
          # note that this overrides any hiv.status specified in the arguments
          value = filter.5D.stats.by.field(sim[[j]]$stats$n.state.sizes, 
                                           years = years,
                                           ages = ages, 
                                           sexes = sexes,
                                           hiv.status = DIM.NAMES.HIV[-1], # extract population, remove HIV negative
                                           # note that this overrides any hiv.status specified in the arguments
                                           ncd.status = ncd.status,
                                           keep.dimensions = keep.dimensions)
        } else 
          stop("Haven't set up to pull this data type from ncd model")
        
        # For scale.population, divide by 2015 population size - have to do this separately for each combo of keep.dimensions
        # RIGHT NOW, IF 2015 VALUE IS 0, MANUALLY SETTING TO 1# 
        if(scale.population){
          
          # Keep dimensions = year only 
          if(setequal(keep.dimensions,"year")){
            if(value[years=="2015"]==0) # can't divide by 0, set to 1 for now 
              value[years=="2015"]=1
            value = value/value[years=="2015"]
            
            # 2 keep dimensions 
          } else if(length(keep.dimensions)==2){
            value = sapply(1:dim(value)[2],function(j){
              sapply(1:dim(value)[1],function(i){
                if(value["2015",j]==0) # can't divide by 0, set to 1 for now 
                  value["2015",j]=1
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
            } else stop("Need to add these dimensions")
            
            
            # 3 keep dimensions
          } else if(length(keep.dimensions)==3){
            value = sapply(1:dim(value)[3],function(k){
              sapply(1:dim(value)[2],function(j){
                sapply(1:dim(value)[1],function(i){
                  if(value["2015",j,k]==0) # can't divide by 0, set to 1 for now 
                    value["2015",j,k]=1
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
            } else stop("Need to add these dimensions")
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
      ##----- HIV OUTPUT -----##
      ##----------------------##
      
      # HIV SIMSET OBJECT OR INDIVIDUAL HIV SIMULATION
    } else {
      
      if("ncd.status" %in% keep.dimensions)
        stop("Can only facet.by ncd status for NCD model")
      if(data.type %in% c("hyp.inc","diab.inc","diab.hyp.inc","mi.inc","stroke.inc"))
        stop("Can only plot NCD/CVD incidence for NCD model")
      
      if(data.type=="incidence" & "hiv.status" %in% keep.dimensions)
        stop("Can't facet incidence by hiv.status")
      
      # if this is a single simulation, need to make it a list with one element
      if(class(sim[[1]])=="array"){
        sim = list(sim)     
      }
      
      for(j in 1:length(sim)){
        if(data.type=="prevalence"){
          hiv.status = DIM.NAMES.HIV[-1] # note that this overrides any hiv.status specified in the arguments
          value = return.khm.data(khm.output=sim[[j]], 
                                  data.type = "population",
                                  ages = ages, 
                                  sexes = sexes,
                                  hiv.status = DIM.NAMES.HIV[-1], # extract population, remove HIV negative 
                                  # note that this overrides any hiv.status specified in the arguments
                                  years=years, 
                                  keep.dimensions = keep.dimensions)
        } else {
          value = return.khm.data(khm.output=sim[[j]], 
                                  data.type = data.type,
                                  ages = ages, 
                                  sexes = sexes,
                                  hiv.status = hiv.status, 
                                  years=years, 
                                  keep.dimensions = keep.dimensions)          
        }

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
            } else stop("Need to add these dimensions")
            
            
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
            } else stop("Need to add these dimensions")
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
      labs(title = paste0(data.type))+
      ylim(0,NA)
  } else{
    plot = ggplot() + 
      geom_line(data = df.sim, aes(x = year, y = value, color = sim.id, group = group.id)) +
      labs(title = paste0(data.type))+
      ylim(0,NA)
  }
  
  suppressWarnings(print(plot))
  
  
}


if(1==2){
  khm.full = simset[[1]]$params$khm.full # HIV simset 
  ncd.simset = simset # NCD simset
  
  # Plot type 1: population - SEE STANDARD PLOTS LIST (WORD DOC)
  simplot(khm.full,ncd.simset,scale.population = T)
  simplot(khm.full,ncd.simset,scale.population = T, facet.by = "age")
  simplot(khm.full,ncd.simset,scale.population = T, facet.by = "sex")
  simplot(khm.full,ncd.simset,scale.population = T, facet.by="hiv.status")
  simplot(ncd.simset,facet.by="ncd.status")
  
  # Plot type 2: HIV incidence
  simplot(khm.full, ncd.simset, data.type = "incidence", scale.population = T)
  simplot(khm.full, ncd.simset, data.type = "incidence", scale.population = T, facet.by = "age")
  simplot(khm.full, ncd.simset, data.type = "incidence", scale.population = T, facet.by = "sex")
  simplot(ncd.simset, data.type = "incidence")
  simplot(ncd.simset, data.type = "incidence", facet.by = "ncd.status")
  
  # Plot type 3: HIV prevalence
  simplot(khm.full, ncd.simset, data.type = "prevalence", scale.population = T, facet.by = "age")
  simplot(khm.full, ncd.simset, data.type = "prevalence", scale.population = T, facet.by = "sex")
  simplot(khm.full, ncd.simset, data.type = "prevalence", scale.population = T, facet.by = "hiv.status")
  simplot(ncd.simset, data.type = "prevalence")
  simplot(ncd.simset, data.type = "prevalence", facet.by = "ncd.status")
  
  # Plot type 4: Diabetes incidence
  simplot(ncd.simset, data.type = "diab.inc")
  simplot(ncd.simset, data.type = "diab.inc", facet.by = "age")
  simplot(ncd.simset, data.type = "diab.inc", facet.by = "sex")
  simplot(ncd.simset, data.type = "diab.inc", facet.by = "hiv.status")
  
  # Plot type 5: Hypertension incidence
  simplot(ncd.simset, data.type = "hyp.inc")
  simplot(ncd.simset, data.type = "hyp.inc", facet.by = "age")
  simplot(ncd.simset, data.type = "hyp.inc", facet.by = "sex")
  simplot(ncd.simset, data.type = "hyp.inc", facet.by = "hiv.status")
  
  # Plot type 6: Diabetes + Hypertension incidence 
  simplot(ncd.simset, data.type = "diab.hyp.inc")
  simplot(ncd.simset, data.type = "diab.hyp.inc", facet.by = "age")
  simplot(ncd.simset, data.type = "diab.hyp.inc", facet.by = "sex")
  simplot(ncd.simset, data.type = "diab.hyp.inc", facet.by = "hiv.status")
  # simplot(ncd.simset, data.type = "diab.hyp.inc", facet.by = "ncd.status")
  
  # Plot type 7: MI and stroke incidence 
  simplot(ncd.simset, data.type = "mi.inc")
  simplot(ncd.simset, data.type = "mi.inc", facet.by = "age")
  simplot(ncd.simset, data.type = "mi.inc", facet.by = "sex")
  simplot(ncd.simset, data.type = "mi.inc", facet.by = "ncd.status")
  simplot(ncd.simset, data.type = "mi.inc", facet.by = "hiv.status")
  simplot(ncd.simset, data.type = "stroke.inc")
  simplot(ncd.simset, data.type = "stroke.inc", facet.by = "age")
  simplot(ncd.simset, data.type = "stroke.inc", facet.by = "sex")
  simplot(ncd.simset, data.type = "stroke.inc", facet.by = "ncd.status")
  simplot(ncd.simset, data.type = "stroke.inc", facet.by = "ncd.status", scale.population = T)
  simplot(ncd.simset, data.type = "stroke.inc", facet.by = "hiv.status")
  
  # Plot type X: not yet completed 
  simplot(khm.full, data.type = "engagement") # need to make this a proportion 
  simplot(khm.full, data.type = "suppression") # need to make this a proportion 
  simplot(khm.full, data.type = "hiv.mortality") 
  
  
  # Testing that error messages work correctly
  # gives an error because we can't show CVD incidence for HIV model (this is correct)
  simplot(khm.full, ncd.simset, data.type = "mi.inc") 
  # gives an error because we can't facet by ncd.status for HIV model (this is correct)
  simplot(khm.full, ncd.simset, data.type = "prevalence", facet.by = "ncd.status") 
  # facet incidence by HIV status - shouldn't work; but actually does work for NCD model...
  simplot(khm.full, ncd.simset, data.type = "incidence", scale.population = T, facet.by = "hiv.status")
  simplot(ncd.simset, data.type = "incidence", scale.population = T, facet.by = "hiv.status")
  
}

