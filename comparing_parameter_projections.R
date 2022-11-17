years = as.character(c(2015:2030))

# compare diagnosis probability
prob.diag.estimated = hiv.output.for.ncd$diagnosis[years,,]/(hiv.output.for.ncd$diagnosis[years,,] + hiv.output.for.ncd$population[years,"undiagnosed",,])
prob.diag.from.params = target.probabilities$prob.diag[years,,]

prob.diag.estimated/prob.diag.from.params

avg.estimated.diag = apply(prob.diag.estimated,1,mean)
avg.from.params.diag = apply(prob.diag.from.params,1,mean)

# compare average probability of diagnosis from estimated method versus from parameters
qplot(c(2015:2030,2015:2030),c(avg.estimated.diag,avg.from.params.diag),
      color=rep(c("est","params"),each=length(avg.estimated.diag)),geom="line") + ylim(0,NA)


# compare engagement probability
prob.eng.estimated = hiv.output.for.ncd$engagement[years,,]/(hiv.output.for.ncd$engagement[years,,] + hiv.output.for.ncd$population[years,"diagnosed_unengaged",,])
prob.eng.from.params = target.probabilities$prob.eng[years,,]

avg.estimated.eng = apply(prob.eng.estimated,1,mean)
avg.from.params.eng = apply(prob.eng.from.params,1,mean)

# compare average probability of engagement from estimated method versus from parameters
qplot(c(2015:2030,2015:2030),c(avg.estimated.eng,avg.from.params.eng),
      color=rep(c("est","params"),each=length(avg.estimated.eng)),geom="line") + ylim(0,NA)

# incidence probability is way smaller
prob.inc.estimated = hiv.output.for.ncd$incidence[years,,]/(hiv.output.for.ncd$incidence[years,,] + hiv.output.for.ncd$population[years,"hiv_negative",,])
avg.estimated.inc = apply(prob.inc.estimated,1,mean)
round(avg.estimated.inc,5)

# testing relative sizes 
apply(hiv.output.for.ncd$incidence,1,mean)
apply(hiv.output.for.ncd$incidence + hiv.output.for.ncd$population[,"hiv_negative",,],1,mean)

apply(hiv.output.for.ncd$diagnosis,1,mean)
apply(hiv.output.for.ncd$diagnosis + hiv.output.for.ncd$population[,"undiagnosed",,],1,mean)

apply(hiv.output.for.ncd$engagement,1,mean)
apply(hiv.output.for.ncd$engagement + hiv.output.for.ncd$population[,"diagnosed_unengaged",,],1,mean)






