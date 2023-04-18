# # Reading populations back into a simset object

#10 pops with a 500K persons
r_path="~/OneDrive - Johns Hopkins/HIV NCD modeling/MELISSA/Model/rHivNcd/"
setwd(r_path)
setwd("outputs/")

ncd.simset=list()
invisible(lapply(c(11:20),function(rep){
  pop<-readRDS(file = sprintf("popList-c%g",rep))
  ncd.simset[[sprintf("pop%g",rep)]]<<-pop
}))
print(paste0("ncd.simset read with ",length(ncd.simset)," members"))
class(ncd.simset)="ncd_simulation_output"

setwd(r_path)

if(1==1){
  # ncd.simset  # NCD simset
  khm.simset = ncd.simset[[1]]$params$khm.full # HIV simset 
  
  
  ## PLOT TYPE 1 - HIV and NCD models (HIV data types only) ##
  data.types = c("population","hiv.incidence","hiv.prevalence")
  facet.by = list(NULL,"age","sex")
  for(d in data.types){
    for(f in facet.by){
      
      if(is.null(f))
        f.label="total"
      else
        f.label=f
      
      jpeg(file=paste0("plots/hiv/",d,"_",f.label,".jpeg"), width = 2500,height = 1500,res=200)
      simplot(khm.simset,ncd.simset,data.type=d,scale.population = T, facet.by = f)
      # simplot(list(ncd.simset=ncd.simset,
      #              khm.simset=khm.simset),data.type=d,scale.population = T, facet.by = f)
      dev.off()    
    }
  }
  
  ## PLOT TYPE 2 - NCD MODEL (NCD data types only) ##
  data.types = c("diab.inc","diab.prev",
                 "hyp.inc","hyp.prev",
                 "diab.hyp.inc","diab.hyp.prev")
  facet.by = list(NULL,"age","sex","hiv.status")
  for(d in data.types){
    for(f in facet.by){
      
      if(is.null(f))
        f.label="total"
      else
        f.label=f
      
      jpeg(file=paste0("plots/ncd/",d,"_",f.label,".jpeg"), width = 2500,height = 1500,res=200)
      simplot(ncd.simset,data.type=d,scale.population = F, facet.by = f)
      dev.off()    
    }
  }
  
  
  ## PLOT TYPE 3 - NCD MODEL (CVD events only) ##
  data.types = c("mi.inc","stroke.inc")
  facet.by = list(NULL,"age","sex","ncd.status","hiv.status")
  for(d in data.types){
    for(f in facet.by){
      
      if(is.null(f))
        f.label="total"
      else
        f.label=f
      
      jpeg(file=paste0("plots/cvd/",d,"_",f.label,".jpeg"), width = 2500,height = 1500,res=200)
      simplot(ncd.simset,data.type=d,scale.population = F, facet.by = f)
      dev.off()    
    }
  }
}

if(1==1){
  
  # Plot type 1: population - SEE STANDARD PLOTS LIST (WORD DOC) - Commented out means it's saved in the above for loops 
  # simplot(khm.simset,ncd.simset,data.type = "population",scale.population = T)
  # simplot(khm.simset,ncd.simset,data.type = "population",scale.population = T, facet.by = "age")
  # simplot(khm.simset,ncd.simset,data.type = "population",scale.population = T, facet.by = "sex")
  simplot(khm.simset,ncd.simset,data.type = "population",scale.population = T, facet.by="hiv.status")
  
  # Plot type 2: HIV hiv.incidence
  # simplot(khm.simset, ncd.simset, data.type = "hiv.incidence", scale.population = T)
  # simplot(khm.simset, ncd.simset, data.type = "hiv.incidence", scale.population = T, facet.by = "age")
  # simplot(khm.simset, ncd.simset, data.type = "hiv.incidence", scale.population = T, facet.by = "sex")
  simplot(ncd.simset, data.type = "hiv.incidence")
  simplot(ncd.simset, data.type = "hiv.incidence", facet.by = "ncd.status")
  
  # Plot type 3: HIV prevalence
  # simplot(khm.simset, ncd.simset, data.type = "hiv.prevalence", scale.population = T, facet.by = "age")
  # simplot(khm.simset, ncd.simset, data.type = "hiv.prevalence", scale.population = T, facet.by = "sex")
  # simplot(khm.simset, ncd.simset, data.type = "hiv.prevalence", scale.population = T, facet.by = "hiv.status")
  simplot(ncd.simset, data.type = "hiv.prevalence", facet.by = "age")
  simplot(ncd.simset, data.type = "hiv.prevalence", facet.by = "ncd.status")
  
  # Plot type 4: Diabetes incidence & prevalence
  # simplot(ncd.simset, data.type = "diab.inc")
  # simplot(ncd.simset, data.type = "diab.inc", facet.by = "age")
  # simplot(ncd.simset, data.type = "diab.inc", facet.by = "sex")
  # simplot(ncd.simset, data.type = "diab.inc", facet.by = "hiv.status")
  # simplot(ncd.simset, data.type = "diab.prev")
  # simplot(ncd.simset, data.type = "diab.prev", facet.by = "age")
  # simplot(ncd.simset, data.type = "diab.prev", facet.by = "sex")
  # simplot(ncd.simset, data.type = "diab.prev", facet.by = "hiv.status")
  
  # Plot type 5: Hypertension incidence & prevalence
  # simplot(ncd.simset, data.type = "hyp.inc")
  # simplot(ncd.simset, data.type = "hyp.inc", facet.by = "age")
  # simplot(ncd.simset, data.type = "hyp.inc", facet.by = "sex")
  # simplot(ncd.simset, data.type = "hyp.inc", facet.by = "hiv.status")
  # simplot(ncd.simset, data.type = "hyp.prev")
  # simplot(ncd.simset, data.type = "hyp.prev", facet.by = "age")
  # simplot(ncd.simset, data.type = "hyp.prev", facet.by = "sex")
  # simplot(ncd.simset, data.type = "hyp.prev", facet.by = "hiv.status")
  
  # Plot type 6: Diabetes + Hypertension incidence & prevalence
  # simplot(ncd.simset, data.type = "diab.hyp.inc")
  # simplot(ncd.simset, data.type = "diab.hyp.inc", facet.by = "age")
  # simplot(ncd.simset, data.type = "diab.hyp.inc", facet.by = "sex")
  # simplot(ncd.simset, data.type = "diab.hyp.inc", facet.by = "hiv.status")
  # simplot(ncd.simset, data.type = "diab.hyp.prev")
  # simplot(ncd.simset, data.type = "diab.hyp.prev", facet.by = "age")
  # simplot(ncd.simset, data.type = "diab.hyp.prev", facet.by = "sex")
  # simplot(ncd.simset, data.type = "diab.hyp.prev", facet.by = "hiv.status")
  
  # Plot type 7: MI and stroke incidence 
  # simplot(ncd.simset, data.type = "mi.inc")
  # simplot(ncd.simset, data.type = "mi.inc", facet.by = "age")
  # simplot(ncd.simset, data.type = "mi.inc", facet.by = "sex")
  # simplot(ncd.simset, data.type = "mi.inc", facet.by = "ncd.status")
  # simplot(ncd.simset, data.type = "mi.inc", facet.by = "hiv.status")
  # simplot(ncd.simset, data.type = "stroke.inc")
  # simplot(ncd.simset, data.type = "stroke.inc", facet.by = "age")
  # simplot(ncd.simset, data.type = "stroke.inc", facet.by = "sex")
  # simplot(ncd.simset, data.type = "stroke.inc", facet.by = "ncd.status")
  # simplot(ncd.simset, data.type = "stroke.inc", facet.by = "ncd.status", scale.population = T)
  # simplot(ncd.simset, data.type = "stroke.inc", facet.by = "hiv.status")
  
  # Plot type X: not yet completed 
  simplot(khm.simset, data.type = "engagement") # need to make this a proportion 
  simplot(khm.simset, data.type = "suppression") # need to make this a proportion 
  simplot(khm.simset, data.type = "hiv.mortality") 
  
  
  # Testing that error messages work correctly
  # gives an error because we can't show CVD incidence for HIV model (this is correct)
  simplot(khm.simset, ncd.simset, data.type = "mi.inc") 
  # gives an error because we can't facet by ncd.status for HIV model (this is correct)
  simplot(khm.simset, ncd.simset, data.type = "hiv.prevalence", facet.by = "ncd.status") 
  # facet incidence by HIV status - shouldn't work; but actually does work for NCD model...
  simplot(khm.simset, ncd.simset, data.type = "hiv.incidence", scale.population = T, facet.by = "hiv.status")
  simplot(ncd.simset, data.type = "hiv.incidence", scale.population = T, facet.by = "hiv.status")
  
}
