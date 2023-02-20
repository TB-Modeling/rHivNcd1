
# Law et al 2002 MI mortality data 

times = c(0, # baseline
          0.01, # before hospital
          0.1, # during hospital
          1, # one year after discharge 
          5, # five years after 
          10) # 10 years after
mortality = c(0, # baseline
              0.23, # before hospital
              0.16, # during hospital
              (0.23+0.16+0.103), # one year after discharge; 0.493
              (0.23+0.16+0.28), # five years after; 0.67
              (0.23+0.16+0.45)) # 10 years after; 0.84

df.mi.survival = data.frame(x=times,y=1-(mortality))

survival = 1-mortality

# set up projection 
project.times = seq(0,10,by=(1/12))
max.cumulative.hazard = 1
cumulative.hazard = -log(survival) # this is a fact 

transformed.data = data.frame(y=log(pmax(0.0001,(1-(cumulative.hazard/max.cumulative.hazard)))),
                              x=times)

# +0 means don't fit an intercept
fit = glm(y~x+0, data = transformed.data,family=gaussian) # same as linear regression 
theta = exp(fit$coefficients[1])

# as above, h = k(1-(theta^t))
projected.cumulative.hazard = max.cumulative.hazard*(1-(theta^project.times))

projected.survival = data.frame(x=project.times,
                                y=exp(-projected.cumulative.hazard)) # h = -log(S) --> S = exp(-h)

monthly.mortality = projected.survival$y[-length(project.times)]-projected.survival$y[-1]
# qplot(project.times[-1],monthly.mortality)
# cbind(project.times[-1],monthly.mortality)
mi.monthly.mortality = array(monthly.mortality,
                                 dim=length(project.times[-1]),
                                 dimnames=list(month=1:120))

100*sum(mi.monthly.mortality[1:12]) # year 1: 48.77%
100*sum(mi.monthly.mortality[13:24]) # year 2: 10.2%
100*sum(mi.monthly.mortality[25:36]) # year 3: 2.9%
100*sum(mi.monthly.mortality[37:48]) # year 4: 0.91%
100*sum(mi.monthly.mortality[49:60]) # year 5: 0.30%
100*sum(mi.monthly.mortality[1:60]) # --> at 5 years, cumulative is 63%
100*sum(mi.monthly.mortality[1:120]) # --> at 10 years, cumulative is still 63%

ggplot() + 
  geom_line(data=projected.survival,aes(x,y)) + 
  geom_point(data=df.mi.survival,aes(x,y)) + 
  ylim(0,1)


projected.survival.one.year = projected.survival[1:13,]

mi.annual.mortality = 0.053
mi.monthly.mortality.post.year.one = 0.053/12
years.2.to.10.monthly.times = project.times[14:length(project.times)]

projected.survival.post.one.year = projected.survival.one.year[13,2] - mi.monthly.mortality.post.year.one*c(1:length(years.2.to.10.monthly.times))
projected.survival.post.one.year = data.frame(x=years.2.to.10.monthly.times,
                                              y=projected.survival.post.one.year)

new.projected.survival = data.frame(x=project.times,
                                    y=c(projected.survival.one.year$y,projected.survival.post.one.year$y))

new.monthly.mortality = new.projected.survival$y[-length(project.times)]-new.projected.survival$y[-1]
new.mi.monthly.mortality = array(new.monthly.mortality,
                                 dim=length(project.times[-1]),
                                 dimnames=list(month=1:120))

qplot(project.times[-1],new.mi.monthly.mortality)

ggplot() + 
  geom_line(data=projected.survival.one.year,aes(x,y)) + 
  geom_line(data=projected.survival.post.one.year,aes(x,y)) + 
  geom_point(data=df.mi.survival,aes(x,y)) + 
  ylim(0,1)

mi.monthly.mortality = new.mi.monthly.mortality

save(mi.monthly.mortality,file="data/monthly.mi.mortality.Rdata")
