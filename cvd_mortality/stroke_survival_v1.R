library(ggplot2)

# Adoukonou stroke mortality data 
times = c(0,(1/12),(3/12),(6/12),1,3,5)
mortality = c(0,.241,.223,.226,.332,.401,.394)
survival = 1-mortality
# qplot(times,-log(survival))

df.stroke.survival = data.frame(x=times,y=survival)

# in terms of cumulative hazard - this is the version we did together: 
# cumulative hazard, H
# max cumulative hazard, k
# constant, theta (smaller --> faster dying off )
# time, t
# H = k(1-(theta^t))

# solve to get a function of t (something*time)
# log(1-(H/k)) = log(theta)*t
# so, y = log(1-(H/k))
# x = t
# coefficient is theta 

# set up projection 
project.times = seq(0,5,by=(1/12))
max.cumulative.hazard = 0.55
cumulative.hazard = -log(survival) # this is a fact 

transformed.data = data.frame(y=log(1-(cumulative.hazard/max.cumulative.hazard)),
                              x=times)
# transformed.data=transformed.data[-length(times),] # remove last data point to maybe fit better

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
stroke.monthly.mortality = array(monthly.mortality,
                                 dim=length(project.times[-1]),
                                 dimnames=list(month=1:60))

100*sum(stroke.monthly.mortality[1:12]) # year 1: 22.5%
100*sum(stroke.monthly.mortality[13:24]) # year 2: 9.9%
100*sum(stroke.monthly.mortality[25:36]) # year 3: 4.8%
100*sum(stroke.monthly.mortality[37:48]) # year 4: 2.4% 
100*sum(stroke.monthly.mortality[49:60]) # year 5: 1.3% 
100*stroke.monthly.mortality[60]*12 # every year after year 5: 0.9% 
sum(stroke.monthly.mortality)*100 # --> at 5 years, cumulative is 40%
(sum(stroke.monthly.mortality) + stroke.monthly.mortality[60]*(12*15))*100 # maxes out at 55% in the longer term (i.e., 15 years)
# IF I MAKE MAX.CUMULATIVE.HAZARD = 1, slightly less in the first year (15% instead); 44% by year 5; 4% each year after 

save(stroke.monthly.mortality,file="data/monthly.cvd.mortality.Rdata")

ggplot() + 
  geom_line(data=projected.survival,aes(x,y)) + 
  geom_point(data=df.stroke.survival,aes(x,y)) + 
  ylim(0,1)

qplot(transformed.data$x,transformed.data$y)
qplot(project.times,theta^project.times)
qplot(project.times,cumulative.hazard)



# my previous attempt 
fit = glm(y ~ x + 0, data = df.stroke.survival,family = poisson)

new.data=data.frame(x=c(times,6:20),
                    y=c(survival,rep(NA,15)))

predictions = predict(fit, newdata = new.data, type="response")
predictions.df = data.frame(x=c(times,6:20),y=predictions)

ggplot() + 
  geom_point(data = df.stroke.survival, aes(x,y)) +
  geom_line(data = predictions.df, aes(x,y)) + 
  ylim(0,1)




# old test code
if(1==2){
  
  S = 1-.394
  t=5
  
  # S = exp(-ht)
  h = -log(S)/t
  
  # S = exp((-ht)^p)
  
  # (1-M) = exp(-ht)
  # M = 1-(exp(-ht))
  
  times.test = c(1:100)
  survival.test = exp(-h*times)
  mortality.test = 1-exp(-h*times)
  
  df.survival.test = data.frame(times.test,survival.test)
  # plot(df.survival.test)
  # abline(h=(1-.394))
  # abline(v=5)
  # 
  # df.mortality.test = data.frame(times.test,mortality.test)
  # plot(df.mortality.test)
  # abline(h=(.394))
  # abline(v=5)
  
  fit.test=glm(survival.test~times.test,data=df.survival.test,family=poisson)
  predictions.test = predict(fit.test,type="response")
  predictions.df.test = data.frame(times.test,predictions.test)
  
  ggplot() + 
    geom_point(data = df.survival.test, aes(times.test,survival.test)) +
    geom_line(data = predictions.df.test, aes(times.test,predictions.test)) + 
    ylim(0,1)
}



