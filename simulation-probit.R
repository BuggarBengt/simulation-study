a#############################################
######## Simulations, my-confounding ########
#### Continuous mediator, binary outcome ####
#############################################

library(sensmediation)
library(mvtnorm)
#NDE för en massa dudes med olika ålder
calc.nde.probit = function(x, b, t) { #ekv. 11 in article
  first.term = (pnorm(t[1] + t[2] + (t[5] + t[6])*x) - pnorm(t[1] + t[5] * x)) * (1 - pnorm(b[1] + b[3] * x))
  second.term = (pnorm(t[1] + t[2] + t[3] + t[4] + (t[5] + t[6] + t[7] + t[8]) * x) - pnorm(t[1]+t[3] + (t[5] + t[7])*x)) * pnorm(b[1] + b[3] * x)
  return(first.term+second.term)
}

calc.nie.probit = function(x, b, t) { #ekv. 12 in article
  first.factor = pnorm(t[1] + t[2] + t[3] + t[4] + (t[5] + t[6] + t[7] + t[8]) * x) - pnorm(t[1]+t[2] + (t[5] + t[6])*x)
  second.factor = pnorm(b[1] + b[2] + (b[3] + b[4]) * x) - pnorm(b[1] + b[3] * x)
  return(first.factor * second.factor)
}

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

Z.coefs = c(a0 = -3.416096, a1 = 0.036231) 
M.coefs = c(b0 = -1.6507546, b1 = 0.2683970, b2 = 0.0065543, b3 = 0)
Y.coefs = c(t0 = -3.7220626, t1 = 0.2763912, t2 = 1.4729651, t3=-1, t4 = 0.0283196, t5 = 0, t6 = 0, t7 = 0)

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


Z.coefs = c(a0 = -3.416096, a1 = 0.036231) 
M.coefs = c(b0 = -1.6507546, b1 = 0.2683970, b2 = 0.0065543, b3 = 0)
Y.coefs = c(t0 = -3.7220626, t1 = 0.2763912, t2 = 1.4729651, t3=-1, t4 = 0.0283196, t5 = 0, t6 = 0, t7 = 0)

Z.star <- Z.coefs["a0"] + Z.coefs["a1"]*X + z.epsilon
Z <- ifelse(Z.star>0, 1, 0)
M.star <- M.coefs["b0"] + M.coefs["b1"]*Z + M.coefs["b2"]*X + m.epsilon
M <- ifelse(M.star > 0, 1, 0)
Y.star <- Y.coefs["t0"] + Y.coefs["t1"]*Z + Y.coefs["t2"]*M + Y.coefs["t3"]*Z*M + Y.coefs["t4"]*X + y.epsilon
Y <- ifelse(Y.star > 0, 1, 0)

m.model <- glm(M ~ Z + X, family=binomial(link='probit')) 
y.model <- glm(Y ~ Z + M + M*Z+ X, family=binomial(link='probit'))

# Estimation of effects:
test.my <- sensmediation(med.model=m.model, out.model=y.model, Rho=rho, progress=FALSE, exp.name = "Z", med.name = "M")
test.my$NDE
test.my$NIE
mean(calc.nde.probit(X, M.coefs, Y.coefs))


