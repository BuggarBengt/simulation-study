library(ggplot2)
library(rsimsum)
# 
# corr.coef = seq(-0.5,0.5, 0.04)
# result = vector(mode = "list", length = length(corr.coef))  #preallocate simulation result list
# true.effects = vector(mode = "list", length = length(corr.coef))  #preallocate list
# n.true.effect = 10000000
# set.seed(4235)
# for (i in 1:length(true.effects)) { #Calculate true effects for given scenario while increasing interaction effect
#   true.effects[[i]] = simulate.true.effects(n = n.true.effect,
#                                             covariate.models = "x-gamma",
#                                             covariate.parameters = list(c(104, 7, 4.5)),
#                                             exposure.coefs = c(I = -3.416096, X = 0.036231),
#                                             mediator.coefs = c(I = -1.6507546, Z = 0.2683970, X = 0.0065543, ZX = 0),
#                                             outcome.coefs = c(I = -3.7220626, Z = 0.2763912, M = 1.4729651, ZM = corr.coef[i], X = 0.0283196, ZX = 0, MX = 0, ZMX = 0),
#                                             outcome.mediator.type = "probit")
#   print(i)
# }
# save(true.effects, file="true.effects.probit.RData") # store the true.effects
# 

corr.coef = seq(-0.5,0.5, 0.04)
result = vector(mode = "list", length = length(corr.coef))  #preallocate simulation result list
set.seed(4235)
start_time = Sys.time()
for (i in 1:length(result)) { # run simulations while increasing interaction effect
  result[[i]] = run.simulation(iterations = 50000,
                               n = 1000,
                               covariate.models = c("x-gamma"),
                               covariate.parameters = list(c(104, 8, 4.5)),
                               true.exposure.coefs = c(I = -3.416096, X = 0.036231),
                               true.mediator.coefs = c(I = -1.6507546, Z = 0.2683970, X = 0.0065543, ZX = 0),
                               true.outcome.coefs = c(I = -3.7220626, Z = 0.2763912, M = 1.4729651, ZM = corr.coef[i], X = 0.0283196, ZX = 0, MX = 0, ZMX = 0),
                               outcome.mediator.type = "probit",
                               sd.exposure = 1,
                               sd.mediator = 1,
                               sd.outcome = 1,
                               misspecified.mediator.formula = "M~Z+X",
                               misspecified.outcome.formula = "Y~Z+M+X")
  print(i/length(result))
}
end_time = Sys.time()
end_time - start_time

save(result, file="Data/Data-probit/result.probit50000.RData") # store the result
load("Data/Data-probit/true.effects.probit.RData")# read true.effects
load("Data/Data-probit/result.probit50000.RData")# read result

result.summary.NDE = vector(mode = "list", length = length(result))
result.summary.NIE = vector(mode = "list", length = length(result))
for (i in 1:length(result)) { # get different statistics of our estimates using rsimsum for each scenario
  result.summary.NDE[[i]] <- simsum(data = data.frame(result[[i]]), estvarname = "est.nde", true = mean(true.effects[[i]][, 2]), se = "SE.nde")
  result.summary.NIE[[i]] <- simsum(data = data.frame(result[[i]]), estvarname = "est.nie", true = mean(true.effects[[i]][, 1]), se = "SE.nie")
}

to.plot = create.data.frame.for.plotting(result.summary.NDE, result.summary.NIE, corr.coef)
save(to.plot, file="Data/Data-probit/to-plot-probit50000.RData") # store the results
load("Data/Data-probit/to-plot-probit50000.RData")# read plot data
load("Data/Data-probit/to-plot-probit50000-wrong-true-effects.RData")# read old plot data
load("Data/Data-probit/to-plot-probit100.RData")# read plot data

ggplot() + #mycket högre effekt på true nde av att öka korrelationen
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = nde.true, col = "NDE")) +
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = nie.true, col = "NIE")) +
  xlab('correlation.coef') +
  ylab('effect') +
  expand_limits(y=0)
ggplot() + 
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = nde.est, col = "est NDE")) +
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = nde.true, col = "true NDE")) +
  geom_ribbon(data = data.frame(to.plot), aes(x=interaction.coefficient, ymin=nde.est-nde.emp.SE, ymax=nde.est+nde.emp.SE), linetype=2, alpha=0.1) +
  xlab('correlation.coef') +
  ylab('effect')
ggplot() + 
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = nie.est, col = "est NIE")) +
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = nie.true, col = "true NIE")) +
  geom_ribbon(data = data.frame(to.plot), aes(x=interaction.coefficient, ymin=nie.est-nie.emp.SE, ymax=nie.est+nie.emp.SE), linetype=2, alpha=0.1) +
  xlab('correlation.coef') +
  ylab('effect')
ggplot() + 
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = nde.coverage, col = "NDE")) +
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = nie.coverage, col = "NIE")) +
  xlab('correlation.coef') +
  ylab('coverage') +
  ggtitle("Coverage of nominal 95% CI. Nominal = based on delta-SE?")
ggplot() + 
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = nde.emp.SE, col = "emp. SE")) +
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = nde.model.SE, col = "model. SE")) +
  xlab('correlation.coef') +
  ylab('SE') +
  ggtitle("Comparison of NDE SE's")
ggplot() + 
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = nie.emp.SE, col = "emp. SE")) +
  geom_line(data = data.frame(to.plot), aes(x=interaction.coefficient, y = nie.model.SE, col = "model. SE")) +
  xlab('correlation.coef') +
  ylab('SE') +
  ggtitle("Comparison of NIE SE's")

knitr::kable(to.plot, col.names = c("interaction.coefficient", "true.nde", "true.nie", "est.nde", "est.nie", 
                                    "nde.emp.SE", "nie.emp.SE", "nde.model.SE", "nie.model.SE", "nde.coverage", "nie.coverage"))












n <- 1000 # Size of data
S <- 100 # Number of replicates
rho <- 0 # Correlation used for analysis

# Storage of results
effekter.nie <- rep(NA, S) # Estimated NIE 
effekter.nde <- rep(NA, S) # Estimated NDE 
CI.nie.lower <- rep(NA, S) # Lower bound of 95% CI of NIE 
CI.nie.upper <- rep(NA, S) # Upper bound of 95% CI of NIE
CI.nde.lower <- rep(NA, S) # Lower bound of 95% CI of NDE
CI.nde.upper <- rep(NA, S) # Upper bound of 95% CI of NDE

nie.S <- numeric(S) # True NIE 
nde.S <- numeric(S) # True NDE

#coefficients used to generate "real data"
Z.coefs = c(a0 = -1, a1 = 0.01) 
M.coefs = c(b0 = 20, b1 = 40, b2 = 1, b3 = 0)
Y.coefs = c(t0 = 10, t1 = 30, t2 = 2, t3=2, t4 = 2, t5 = 0, t6 = 0, t7 = 0)

# Simulations:
set.seed(4352)
for(i in 1:S){
  if(i %% 50 == 0)
    print(i)
  
  z.epsilon <- rnorm(n) # Error terms for each model.
  m.epsilon <- rnorm(n) 
  y.epsilon <- rnorm(n) 
  
  X <- 104-rgamma(n,shape=7,scale=4.5) # "age" 
  
  # Generate exposure, mediator and outcome (True models):
  Z.star <- Z.coefs["a0"] + Z.coefs["a1"]*X + z.epsilon
  Z <- ifelse(Z.star>0, 1, 0)
  M.star <- M.coefs["b0"] + M.coefs["b1"]*Z + M.coefs["b2"]*X + m.epsilon
  M <- ifelse(M.star > 0, 1, 0)
  Y.star <- Y.coefs["t0"] + Y.coefs["t1"]*Z + Y.coefs["t2"]*M + Y.coefs["t3"]*Z*M + Y.coefs["t4"]*X + y.epsilon
  Y <- ifelse(Y.star > 0, 1, 0)
  
  # Estimated models (y.model misspecified without Z-M correlation):
  m.model <- glm(M ~ Z + X, family=binomial(link='probit')) 
  y.model <- glm(Y ~ Z + M + X, family=binomial(link='probit'))
  
  # Estimation of effects:
  test.my <- sensmediation(med.model=m.model, out.model=y.model, Rho=rho, progress=FALSE, exp.name = "Z", med.name = "M")
  
  # Storage of results
  effekter.nie[i] <- test.my$NIE
  effekter.nde[i] <- test.my$NDE
  CI.nie.lower[i] <- test.my$CI$CI.nie[,1]
  CI.nie.upper[i] <- test.my$CI$CI.nie[,2]
  CI.nde.lower[i] <- test.my$CI$CI.nde[,1]
  CI.nde.upper[i] <- test.my$CI$CI.nde[,2]
  
  nie.S[i] <- mean(calc.nie.probit(X, M.coefs, Y.coefs))
  nde.S[i] <- mean(calc.nde.probit(X, M.coefs, Y.coefs))
}

mean(ifelse(nie.S < CI.nie.upper & nie.S > CI.nie.lower, 1, 0))
mean(ifelse(nde.S < CI.nde.upper & nde.S > CI.nde.lower, 1, 0))

NIE.string = paste("True NIE: ", round(mean(nie.S), 4), " Est. NIE: ", round(mean(effekter.nie), digits = 4), " Avg. CI width: (", round(mean(CI.nie.lower), digits = 4), "; ", round(mean(CI.nie.upper), digits = 4), ")", sep = "") 
NDE.string = paste("True NDE: ", round(mean(nde.S), 4), " Est. NDE: ", round(mean(effekter.nde), digits = 4), " Avg. CI width: (", round(mean(CI.nde.lower), digits = 4), "; ", round(mean(CI.nde.upper), digits = 4), ")", sep = "") 
NIE.string
NDE.string

m.model <- glm(M ~ Z + X, family=binomial(link='probit')) 
y.model <- glm(Y ~ Z + M + M*Z+ X, family=binomial(link='probit'))

# Estimation of effects:
test.my <- sensmediation(med.model=m.model, out.model=y.model, Rho=rho, progress=FALSE, exp.name = "Z", med.name = "M")
test.my$NDE
test.my$NIE
mean(calc.nde.probit(X, M.coefs, Y.coefs))


