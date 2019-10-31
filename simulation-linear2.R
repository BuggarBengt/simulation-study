
library(ggplot2)
library(mediation)

data(jobs) #let's use this as our "true" scenario
model_exposure <- glm(treat ~ age, data = jobs, family = binomial(link = "probit"))
model_mediator <- glm(job_seek ~ treat + age, data = jobs)
model_outcome  <- glm(depress2 ~ treat * job_seek + age , data = jobs)
coef(model_exposure) 
coef(model_mediator) 
coef(model_outcome) #true correlation = 0.0653195710 
test.my <- sensmediation(med.model=model_mediator, out.model=model_outcome, Rho=0, progress=FALSE, exp.name = "treat", med.name = "job_seek")
test.my$NIE
test.my$NDE
test.my$CI$CI.nie[,1]
test.my$CI$CI.nie[,2]
test.my$CI$CI.nde[,1]
test.my$CI$CI.nde[,2]
#coefficents to be used as true
exposure.coefs = c(a0 = 0.35512882, a1 = 0.00206941)
mediator.coefs = c(b0 = 3.827541111, b1 = 0.065751007, b2 = 0.004576608, b3 = 0) 
outcome.coefs = c(t0 = 2.8405520021, t1 = -0.3108865465, t2 = -0.2717585224, t3=corr.coef[i], t4 = 0.0007960718, t5 = 0, t6 = 0, t7 = 0)

summary(jobs) # we want a closeish resemblance of age in jobs using the gamma function
summary(rgamma(2000,shape=8,scale=4.5))
summary(jobs$age)
hist(rgamma(200,shape=8,scale=4.5)) # we put this as a generator for age when running simulations
hist(jobs$age)

set.seed(4235)
corr.coef = seq(0, 1, 0.02)
to.plot = matrix(nrow = length(corr.coef), ncol = 11) #preallocate vector to plot
colnames(to.plot) = c("interaction.coefficient", "true.nde", "true.nie", "est.nde", "est.nie", "nde.bias", "nie.bias", "nde.percent.bias", "nie.percent.bias","nde.coverage", "nie.coverage")
result = vector(mode = "list", length = length(corr.coef))  #preallocate result list
for (i in 1:length(result)) { # run simulations while increasing interaction effect
  result[[i]] = runSimulation(S = 100, n = 1000, exposure.coefs = c(a0 = -3.416096, a1 = 0.036231), 
                              mediator.coefs = c(b0 = 50, b1 = 5, b2 = 0.11, b3 = 0), 
                              outcome.coefs = c(t0 = 50, t1 = 5, t2 = 5, t3=corr.coef[i], t4 = 0.35, t5 = 0, t6 = 0, t7 = 0))
  to.plot[i, 1] = corr.coef[i]
  to.plot[i, 2] = mean(result[[i]]$true.nde)
  to.plot[i, 3] = mean(result[[i]]$true.nie)
  to.plot[i, 4] = mean(result[[i]]$est.nde)
  to.plot[i, 5] = mean(result[[i]]$est.nie)
  to.plot[i, 6] = mean(result[[i]]$est.nde - result[[i]]$true.nde)
  to.plot[i, 7] = mean(result[[i]]$est.nie) - result[[i]]$true.nie
  to.plot[i, 8] = mean((result[[i]]$est.nde - result[[i]]$true.nde)/result[[i]]$true.nde)
  to.plot[i, 9] = mean((result[[i]]$est.nie - result[[i]]$true.nie)/result[[i]]$true.nie)
  to.plot[i, 10] = mean(ifelse(result[[i]]$true.nde < result[[i]]$CI.nde.upper & result[[i]]$true.nde > result[[i]]$CI.nde.lower, 1, 0))
  to.plot[i, 11] = mean(ifelse(result[[i]]$true.nie < result[[i]]$CI.nie.upper & result[[i]]$true.nie > result[[i]]$CI.nie.lower, 1, 0))
}

ggplot() + #mycket högre effekt på true nde av att öka korrelationen
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = true.nde, col = "NDE")) +
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = true.nie, col = "NIE")) +
  xlab('correalation.coef') +
  ylab('est.effect')
ggplot() + 
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = est.nde, col = "est NDE")) +
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = true.nde, col = "true NDE")) +
  xlab('correalation.coef') +
  ylab('est.effect')
ggplot() + 
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = est.nie, col = "est NIE")) +
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = true.nie, col = "true NIE")) +
  xlab('correalation.coef') +
  ylab('est.effect')

ggplot(data.frame(to.plot), aes(x=interaction.coefficient, y = est.nde)) +
  geom_point() +
  geom_line()
ggplot(data.frame(to.plot), aes(x=interaction.coefficient, y = est.nie)) +
  geom_point() +
  geom_line()
ggplot(data.frame(to.plot), aes(x=interaction.coefficient, y = nde.bias)) +
  geom_point() +
  geom_line()
ggplot(data.frame(to.plot), aes(x=interaction.coefficient, y = nie.bias)) +
  geom_point() +
  geom_line()
ggplot(data.frame(to.plot), aes(x=interaction.coefficient, y = nie.bias + nde.bias)) +
  geom_point() +
  geom_line()
ggplot(data.frame(to.plot), aes(x=interaction.coefficient, y = nde.percent.bias)) +
  geom_point() +
  geom_line()
ggplot(data.frame(to.plot), aes(x=interaction.coefficient, y = nie.percent.bias)) +
  geom_point() +
  geom_line()
ggplot(data.frame(to.plot), aes(x=interaction.coefficient, y = nde.coverage)) +
  geom_point() +
  geom_line()
ggplot(data.frame(to.plot), aes(x=interaction.coefficient, y = nie.coverage)) +
  geom_point() +
  geom_line()




result2 = runSimulation(S = 1000, n = 1000, exposure.coefs = c(a0 = -3.416096, a1 = 0.036231), 
                        mediator.coefs = c(b0 = 10, b1 = 1.4, b2 = 0.011, b3 = 0), 
                        outcome.coefs = c(t0 = 10, t1 = 0.5, t2 = 0.15, t3=0.0, t4 = 0.035, t5 = 0, t6 = 0, t7 = 0))

mean(ifelse(result$true.nie < result$CI.nie.upper & result$true.nie > result$CI.nie.lower, 1, 0))
mean(ifelse(result$true.nde < result$CI.nde.upper & result$true.nde > result$CI.nde.lower, 1, 0))

NIE.string = paste("True NIE: ", round(mean(result$true.nie), 4), " Est. NIE: ", round(mean(result$est.nie), digits = 4), " Avg. CI width: (", round(mean(result$CI.nie.lower), digits = 4), "; ", round(mean(result$CI.nie.upper), digits = 4), ")", sep = "") 
NDE.string = paste("True NDE: ", round(mean(result$true.nde), 4), " Est. NDE: ", round(mean(result$est.nde), digits = 4), " Avg. CI width: (", round(mean(result$CI.nde.lower), digits = 4), "; ", round(mean(result$CI.nde.upper), digits = 4), ")", sep = "") 
NIE.string
NDE.string

