library(ggplot2)

# Adoukonou stroke mortality data - UP UNTIL 1 YEAR
times.1 = c(0,(1/12),(3/12),(6/12))
mortality.1 = c(0,.241,.223,.226)
survival.1 = 1-mortality.1

df.stroke.survival.1 = data.frame(x=times.1,y=survival.1)

# set up projection 
project.times.1 = seq(0,5,by=(1/12))
max.cumulative.hazard = 0.55
cumulative.hazard.1 = -log(survival.1)

transformed.data.1 = data.frame(y=log(1-(cumulative.hazard.1/max.cumulative.hazard)),
                                x=times.1)

fit.1 = glm(y~x+0, data = transformed.data.1,family=gaussian) # same as linear regression 
theta.1 = exp(fit.1$coefficients[1])

projected.cumulative.hazard.1 = max.cumulative.hazard*(1-(theta.1^project.times.1))
projected.survival.1 = data.frame(x=project.times.1,
                                  y=exp(-projected.cumulative.hazard.1)) 

monthly.mortality.1 = projected.survival.1$y[-length(project.times.1)]-projected.survival.1$y[-1]

# Adoukonou stroke mortality data - 1 YEAR ONWARD
times.2 = c(1,3,5)
mortality.2 = c(.332,.401,.394)
survival.2 = 1-mortality.2

df.stroke.survival.2 = data.frame(x=times.2,y=survival.2)

project.times.2 = seq(0,5,by=(1/12))
cumulative.hazard.2 = -log(survival.2) 

transformed.data.2 = data.frame(y=log(1-(cumulative.hazard.2/max.cumulative.hazard)),
                                x=times.2)

fit.2 = glm(y~x+0, data = transformed.data.2,family=gaussian) # same as linear regression 
theta.2 = exp(fit.2$coefficients[1])

projected.cumulative.hazard.2 = max.cumulative.hazard*(1-(theta.2^project.times.2))
projected.survival.2 = data.frame(x=project.times.2,
                                  y=exp(-projected.cumulative.hazard.2))

monthly.mortality.2 = projected.survival.2$y[-length(project.times.2)]-projected.survival.2$y[-1]


if(1==2){
  qplot(project.times.1[-1],c(monthly.mortality.1[1:11],monthly.mortality.2[12:60]))
  
  sum(monthly.mortality.1[1:11]) # first 11 months; paper says 1 year = 33%
  sum(monthly.mortality.1[1:11],monthly.mortality.2[12:60]) # 5 years; paper says = 40% 
  
  # original data for plotting 
  times = c(0,(1/12),(3/12),(6/12),1,3,5)
  mortality = c(0,.241,.223,.226,.332,.401,.394)
  survival = 1-mortality
  df.stroke.survival = data.frame(x=times,y=survival)
  
  ggplot() + 
    geom_line(data=projected.survival.1[1:12,],aes(x,y)) + 
    geom_line(data=projected.survival.2[12:60,],aes(x,y)) + 
    geom_point(data=df.stroke.survival,aes(x,y)) + 
    ylim(0,1)
}

stroke.monthly.mortality = array(c(monthly.mortality.1[1:11],monthly.mortality.2[12:60]),
                                 dim=length(project.times.1[-1]),
                                 dimnames=list(month=1:60))

save(stroke.monthly.mortality,file="data/monthly.stroke.mortality.Rdata")
