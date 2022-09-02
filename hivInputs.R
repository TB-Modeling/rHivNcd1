#
#  R HIVNCD 2022
#  HIV inputs from compartmental model
#  
#####################################
print("Reading HIV inputs... ")

DIM.SEX=2
DIM.AGE=17
DIM.HIV=4
DIM.NCD=4

# t0: initial prev of HIV states
# t1...end: we can use the transition probabilities in each year to model inc, diag, supp and diseng; 
# however, that may result in divergent outcomes over time
# another way to do this is to report prev of HIV states in eahc year, compute diff between ABS pop and target values, and estimate the prob of transitions from there

# reporting deaths


# intial population at year 0
mat=array(c(1: (DIM.SEX*DIM.AGE)),
          dim = c(DIM.SEX,DIM.AGE))
# dimnames(mat)<-list("sex","age","hiv","year")
mat;sum(mat);dim(mat)
mat.initial.pop<-mat;print("mat.initial.pop")

# intial prevalence of HIV
mat=array(mapply(rep,c(0.1,.2,.3,.4),DIM.SEX*DIM.AGE ),
          dim = c(DIM.SEX,DIM.AGE,DIM.HIV ))
mat;sum(mat);dim(mat)
mat.hiv.initial.prevalence.ratio<-mat; print("mat.hiv.initial.prevalence.ratio")

# intial prevalence of NCD
mat=array(mapply(rep,c(0.5,.2,.2,.1),DIM.SEX*DIM.AGE ),
          dim = c(DIM.SEX,DIM.AGE,DIM.NCD ))
mat;sum(mat);dim(mat)
mat.ncd.initial.prevalence.ratio<-mat; print("mat.ncd.initial.prevalence.ratio")

