library(ggplot2)

plot.hiv.distribution = function(hiv.positive.population.only=T,
                                 strata="total"){
  
  if(hiv.positive.population.only) {
    pop.from.hiv.model = hiv.output.for.ncd$population[,-1,,]  
    hiv.status = mc$DIM.NAMES.HIV[-1]
  } else {
    pop.from.hiv.model = hiv.output.for.ncd$population
    hiv.status = mc$DIM.NAMES.HIV
  }
  
  years = dimnames(pop.from.hiv.model)[[1]]
  ages = mc$DIM.NAME.AGEGROUP
  sexes = c("FEMALE","MALE")
  
  full.dim.names = list(year = years,
                        hiv.status = hiv.status,
                        age = ages,
                        sex = sexes)
  hiv.dim.names =  full.dim.names[-1]
  
  hiv.distr = array(0,
                    dim = sapply(full.dim.names,length),
                    dimnames = full.dim.names)
  
  for(i in 1:length(years)){
    hiv.probs = 
      sapply(1:length(hiv.dim.names$age), function(age){
        sapply(1:length(hiv.dim.names$sex), function(sex){
          rowSums(pop.from.hiv.model[i,,,],1)/sum(pop.from.hiv.model[i,,,])
        })
      })
    
    dim(hiv.probs) = sapply(hiv.dim.names,length)
    dimnames(hiv.probs) = hiv.dim.names
    
    # test
    # colSums(hiv.probs,1)
    
    hiv.distr[i,,,] = hiv.probs
  }
  
  df.for.plot = reshape2::melt(hiv.distr)
  
  if(strata=="total"){
    # total
    plot = ggplot(df.for.plot, aes(fill=hiv.status, x = year, y = value)) + 
      geom_bar(position="fill", stat="identity") 
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


array(unlist(cReturnSexAgeDist(pop)),dim = c(17,2),dimnames = list(
  mc$DIM.NAME.AGEGROUP,
  mc$DIM.NAMES.SEX))

array(unlist(cReturnHivNcdStates(pop)),dim = c(4,4),dimnames = list(
  mc$DIM.NAMES.NCD,
  mc$DIM.NAMES.HIV))

array(cReturnHivStates(pop),dimnames = list(mc$DIM.NAMES.HIV))


#' @PK I'm trying to write function to return age/sex/HIV - need help 

# // [[Rcpp::export]]
# vector<vector<double>> cReturnSexAgeHivDist(List &pop){
#   vector<vector<vector<double>>> res(2*NUM_HIV_STATES,vector<double>(NUM_AGE_GROUPS,0));
#   const int popsize = pop.size();
#   //Loop through the population
#   for (int i = 0; i < popsize; i++) {
#     Environment p = pop[i];
#     // if (as<int>(p["agegroup"])>NUM_AGE_GROUPS) throw logic_error(errAgeDist); //@JP: this doesnt really work
#     // else{
#       res[as<int>(p["sex"])-1][as<int>(p["agegroup"])-1][as<int>(p["hivState"])-1]++;
#     }
#     // }
#   return(res);
# }



