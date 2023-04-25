
library("foreign") # to read SPSS file 
library("mice")
library("ggplot2")
library("dplyr")
library("tidyr")
library("reshape2")

set.seed(1234) 

##-----------------------------------##
##----- IMPORT/CLEAN STEPS DATA -----##
##-----------------------------------##

full.data = read.spss("data/2015 STEPS Data.sav", to.data.frame = TRUE)
# write.csv(full.data,file = "full.data.csv")

variables = attributes(full.data)$variable.labels
variable.names = unname(variables)
variables.to.keep = names(variables[c(1,15,20,64,85,188:209,271:280,289,294:297)])
variables.to.keep.names = unname(variables[variables.to.keep])
df = full.data[,variables.to.keep]
attributes(df)$variable.labels = variables.to.keep.names

# Clean sex data
df$C1 = trimws(df$C1)
df$C1[df$C1=="Female"] = "FEMALE"
df$C1[df$C1=="Male"] = "MALE"

# Clean cholesterol to remove values at or below 2 and at or above 12 
df$B8[!is.na(df$B8) & df$B8<=2] = NA
df$B8[!is.na(df$B8) & df$B8>=12] = NA

# Clean SBP to remove values at or below 40 and at or above 300 
df$M4a[!is.na(df$M4a) & (df$M4a<=40 | df$M4a>=300)] = NA #first reading
df$M5a[!is.na(df$M5a) & (df$M5a<=40 | df$M5a>=300)] = NA #second reading
df$M6a[!is.na(df$M6a) & (df$M6a<=40 | df$M6a>=300)] = NA #third reading

# Define SBP - need two valid readings 
df$SBP = NA
ids = !is.na(df$M5a) & !is.na(df$M6a) # If second and third are valid, ignore first (whether or not it's valid)
df[ids,"SBP"] = (df[ids,"M5a"]+df[ids,"M6a"])/2
ids = !is.na(df$M4a) & is.na(df$M5a) & !is.na(df$M6a) # first and third valid
df[ids,"SBP"] = (df[ids,"M4a"]+df[ids,"M6a"])/2
ids = !is.na(df$M4a) & !is.na(df$M5a) & !is.na(df$M6a) # first and second valid
df[ids,"SBP"] = (df[ids,"M4a"]+df[ids,"M5a"])/2

# Remove individual readings 
drop = c("M4a","M5a","M6a")
df = df[,!(names(df) %in% drop)]

# Clean DBP to remove values below 30 and above 200 
df$M4b[!is.na(df$M4b) & (df$M4b<=30 | df$M4b>=200)] = NA #first reading
df$M5b[!is.na(df$M5b) & (df$M5b<=30 | df$M5b>=200)] = NA #second reading
df$M6b[!is.na(df$M6b) & (df$M6b<=30 | df$M6b>=200)] = NA #third reading

# Define DBP - need two valid readings 
df$DBP = NA
ids = !is.na(df$M5b) & !is.na(df$M6b) # If second and third are valid, ignore first (whether or not it's valid)
df[ids,"DBP"] = (df[ids,"M5b"]+df[ids,"M6b"])/2
ids = !is.na(df$M4a) & is.na(df$M5b) & !is.na(df$M6b) # first and third valid
df[ids,"DBP"] = (df[ids,"M4b"]+df[ids,"M6b"])/2
ids = !is.na(df$M4b) & !is.na(df$M5b) & !is.na(df$M6b) # first and second valid
df[ids,"DBP"] = (df[ids,"M4b"]+df[ids,"M5b"])/2

# Remove individual readings 
drop = c("M4b","M5b","M6b")
df = df[,!(names(df) %in% drop)]



WB.data = read.csv("data/WorldBank_PopulationbyAgeSex_Data.csv")
var = WB.data[,1]
ages = substr(var,17,21)
ages = unique(ages)
ages[length(ages)] = "80 and over"

female.age.dist = (WB.data[1:length(ages),grepl("2015",colnames(WB.data))]/100)
names(female.age.dist) = as.character(ages)

male.age.dist = (WB.data[(length(ages)+1):nrow(WB.data),grepl("2015",colnames(WB.data))]/100)
names(male.age.dist) = as.character(ages)

age.sex = cbind(female.age.dist,male.age.dist)
# 
# plot(female.age.dist)
# plot(male.age.dist)


pop.size = 35000
female.prop = .503269965150629 
n.female = rbinom(1,pop.size,female.prop)
n.male = pop.size-n.female

female.probs = rep(female.age.dist[-length(female.age.dist)], each=5)/5
female.probs = c(female.probs, (rep(female.age.dist[length(female.age.dist)],21)/21))
names(female.probs) = as.character(c(0:100))
sim.female = sample(x = 0:(length(female.probs)-1), size = n.female, replace = TRUE, prob = female.probs)

male.probs = rep(male.age.dist[-length(male.age.dist)], each=5)/5
male.probs = c(male.probs, (rep(male.age.dist[length(male.age.dist)],21)/21))
names(male.probs) = as.character(c(0:100))
sim.male = sample(x = 0:(length(male.probs)-1), size = n.male, replace = TRUE, prob = male.probs)



##-----------------------------------------------------##
##----- CREATE BLANK SIM.POP WITH CORRECT AGE/SEX -----##
##-----------------------------------------------------##
sim.pop = df[1:pop.size,]
sim.pop[] = NA
sim.pop[1:n.female,"C3"] = sim.female
sim.pop[1:n.female,"C1"] = "FEMALE"
sim.pop[(n.female+1):pop.size,"C3"] = sim.male
sim.pop[(n.female+1):pop.size,"C1"] = "MALE"

sim.pop = sim.pop[,c("C1","C3")]
sim.pop$age.group = ceiling((sim.pop$C3+1)/5)

qplot(sim.pop$age.group[sim.pop$C1=="FEMALE"], binwidth=1)
qplot(sim.pop$age.group[sim.pop$C1=="MALE"], binwidth=1)


simplot(ncd.simset,data.type = "population",scale.population = T, facet.by = c("age","sex"), sexes = "MALE")
