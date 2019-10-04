library(sensmediation)

calc.nde.linear = function(z.from, z.to, x, b, t) { #explanation in causal inference /Vanderweele
  return((t[1] + t[4]*b[1] + t[4]*b[2]*z.from + t[4]*b[3]*x)*(z.to-z.from))
}

calc.nie.linear = function(z.from, z.to, b, t) {
  return((t[3]*b[2] + t[4]*b[2]*z.to)*(z.to-z.from))
}

n <- 1000 # Size of data 
S <- 1000 # Number of replicates

# Storage of results
effekter.nie <- rep(NA, S) # Estimated NIE 
effekter.nde <- rep(NA, S) # Estimated NDE 
CI.nie.lower <- rep(NA, S) # Lower bound of 95% CI of NIE  
CI.nie.upper <- rep(NA, S) # Upper bound of 95% CI of NIE
CI.nde.lower <- rep(NA, S) # Lower bound of 95% CI of NDE
CI.nde.upper <- rep(NA, S) # Upper bound of 95% CI of NDE

nie.S <- numeric(S) # True NIE 
nde.S <- numeric(S) # True NDE

Z.coefs = c(a0 = 10, a1 = 0.3) 
M.coefs = c(b0 = 10, b1 = 1.4, b2 = 0.011, b3 = 0)
Y.coefs = c(t0 = 10, t1 = 0.5, t2 = 0.15, t3=0.0, t4 = 0.035, t5 = 0, t6 = 0, t7 = 0)

#true.NIE = M.coefs["b1"]*Y.coefs["t2"] + M.coefs["b1"]*Y.coefs["t3"] 
#true.NDE = Y.coefs["t1"]+Y.coefs["t3"]

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
  M <- M.coefs["b0"] + M.coefs["b1"]*Z + M.coefs["b2"]*X + m.epsilon
  Y <- Y.coefs["t0"] + Y.coefs["t1"]*Z + Y.coefs["t2"]*M + Y.coefs["t3"]*Z*M + Y.coefs["t4"]*X + y.epsilon
  
  nie.S[i] = calc.nie.linear(z.from = 0, z.to = 1, b = M.coefs, t = Y.coefs)
  nde.S[i] = calc.nde.linear(z.from = 0, z.to = 1, b = M.coefs, t = Y.coefs, x = X)
  
  # Estimated models (y.model misspecified without Z-M correlation):
  m.model <- glm(M ~ Z + X) 
  y.model <- glm(Y ~ Z + M + X)
  
  # Estimation of effects:
  test.my <- sensmediation(med.model=m.model, out.model=y.model, Rho=0, progress=FALSE, exp.name = "Z", med.name = "M")
  
  # Storage of results
  effekter.nie[i] <- test.my$NIE
  effekter.nde[i] <- test.my$NDE
  CI.nie.lower[i] <- test.my$CI$CI.nie[,1]
  CI.nie.upper[i] <- test.my$CI$CI.nie[,2]
  CI.nde.lower[i] <- test.my$CI$CI.nde[,1]
  CI.nde.upper[i] <- test.my$CI$CI.nde[,2]
}
mean(ifelse(nie.S < CI.nie.upper & nie.S > CI.nie.lower, 1, 0))
mean(ifelse(nde.S < CI.nde.upper & nde.S > CI.nde.lower, 1, 0))

NIE.string = paste("True NIE: ", round(mean(nie.S), 4), " Est. NIE: ", round(mean(effekter.nie), digits = 4), " Avg. CI width: (", round(mean(CI.nie.lower), digits = 4), "; ", round(mean(CI.nie.upper), digits = 4), ")", sep = "") 
NDE.string = paste("True NDE: ", round(mean(nde.S), 4), " Est. NDE: ", round(mean(effekter.nde), digits = 4), " Avg. CI width: (", round(mean(CI.nde.lower), digits = 4), "; ", round(mean(CI.nde.upper), digits = 4), ")", sep = "") 
NIE.string
NDE.string


