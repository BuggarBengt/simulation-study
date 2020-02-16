# linear
corr.coef = seq(-50,50, 2)/100
result = vector(mode = "list", length = length(corr.coef))  #preallocate simulation result list
true.effects = vector(mode = "list", length = length(corr.coef))  #preallocate list
true.effects = matrix(NA, nrow = 0, ncol = 3)  #preallocate result matrix
n.true.effect = 10000000
set.seed(4235)
for (i in 1:length(corr.coef)) { #Calculate true effects for given scenario while increasing interaction effect
  temp = cbind(ZM.corr = corr.coef[i], simulate.true.effects(n = n.true.effect,
                                            exposure.coefs =  c(I = -0.4, X = 0.01),
                                            mediator.coefs = c(I = 3, Z = 2, X = 0.05, ZX = 0),
                                            outcome.coefs = c(I = 5, Z = 1, M = 0.5, ZM = corr.coef[i], X = 0.05),
                                            outcome.mediator.type = "linear"))
  true.effects = rbind(true.effects, temp)
  print(i/length(corr.coef))
}
save(true.effects, file="Data/Data-linear/true.effects.RData")


# probit
corr.coef = seq(-0.5,0.5, 0.04)
result = vector(mode = "list", length = length(corr.coef))  #preallocate simulation result list
true.effects = vector(mode = "list", length = length(corr.coef))  #preallocate list
n.true.effect = 10000000
set.seed(4235)
for (i in 1:length(true.effects)) { #Calculate true effects for given scenario while increasing interaction effect
  true.effects[[i]] = simulate.true.effects(n = n.true.effect,
                                            covariate.models = "x-gamma",
                                            covariate.parameters = list(c(104, 8, 4.5)),
                                            exposure.coefs = c(I = -3.416096, X = 0.036231),
                                            mediator.coefs = c(I = -1.6507546, Z = 0.2683970, X = 0.0065543, ZX = 0),
                                            outcome.coefs = c(I = -3.7220626, Z = 0.2763912, M = 1.4729651, ZM = corr.coef[i], X = 0.0283196, ZX = 0, MX = 0, ZMX = 0),
                                            outcome.mediator.type = "probit")
  print(i)
}
save(true.effects, file="Data/Data-probit/true.effects.probit.RData") # store the true.effects

# probit negative
corr.coef = seq(-1,1, 0.08)
result = vector(mode = "list", length = length(corr.coef))  #preallocate simulation result list
true.effects = vector(mode = "list", length = length(corr.coef))  #preallocate list
n.true.effect = 10000000
set.seed(4235)
for (i in 1:length(true.effects)) { #Calculate true effects for given scenario while increasing interaction effect
  true.effects[[i]] = simulate.true.effects(n = n.true.effect,
                                            covariate.models = "x-gamma",
                                            covariate.parameters = list(c(104, 8, 4.5)),
                                            exposure.coefs = c(I = -3.416096, X = 0.036231),
                                            mediator.coefs = c(I = 1, Z = -0.4, X = -0.008, ZX = 0),
                                            outcome.coefs = c(I = 4, Z = -0.4, M = -2, ZM = corr.coef[i], X = -0.03, ZX = 0, MX = 0, ZMX = 0),
                                            outcome.mediator.type = "probit")
  print(i)
}
save(true.effects, file="Data/Data-probit/true.effects.probit.reverse.RData") # store the true.effects

# probit confounding
n.true.effect = 10000000
set.seed(4235)
# Calculate true effects for given scenario used for confounding
true.effects = simulate.true.effects(n = n.true.effect,
                                            covariate.models = "x-gamma",
                                            covariate.parameters = list(c(104, 8, 4.5)),
                                            exposure.coefs = c(I = -3.416096, X = 0.036231),
                                            mediator.coefs = c(I = -1.6507546, Z = 0.2683970, X = 0.0065543, ZX = 0),
                                            outcome.coefs = c(I = -3.7220626, Z = 0.2763912, M = 1.4729651, ZM = -0.2583784, X = 0.0283196, ZX = 0, MX = 0, ZMX = 0),
                                            outcome.mediator.type = "probit")
save(true.effects, file="Data/Data-probit/true.effects.confounding.probit.RData") # store the true.effects


# Random plots
temp = vector(length = 26)
index= 1
for (eff in true.effects) {
  temp[[index]] = eff[1]  
  index=index+1
}

temp2 = vector(length = 26)
index= 1
for (eff in true.effects2) {
  temp2[[index]] = eff[1]  
  index=index+1
}

ggplot() + 
  geom_line(data = data.frame(x=corr.coef, y=temp), aes(x=x, y = y, col = "est NIE")) +
  geom_line(data = data.frame(x=corr.coef, y=temp2), aes(x=x, y = y, col = "est NIE")) +
  xlab('correlation.coef') +
  ylab('total effect')

