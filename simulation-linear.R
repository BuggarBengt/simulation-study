library(sensmediation)

calc.nde.linear = function(z.from, z.to, x, b, t) { # explanation in causal inference /Vanderweele
  return((t[2] + t[4]*b[1] + t[4]*b[2]*z.from + t[4]*b[3]*x)*(z.to-z.from))
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

Z.coefs = c(a0 = -1, a1 = 0.01) 
M.coefs = c(b0 = 20, b1 = 40, b2 = 1, b3 = 0)
Y.coefs = c(t0 = 10, t1 = 30, t2 = 2, t3=2, t4 = 2, t5 = 0, t6 = 0, t7 = 0)

nie.S <- calc.nie.linear(z.from = 0, z.to = 1, b = M.coefs, t = Y.coefs) # True NIE 
nde.S <- numeric(S) # True NDE

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
  
  nde.S[i] = mean(calc.nde.linear(z.from = 0, z.to = 1, b = M.coefs, t = Y.coefs, x = X))
  
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


