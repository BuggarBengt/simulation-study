library(sensmediation)

calc.nde.linear = function(z.from, z.to, x, b, t) {
  return((t[2] + t[4]*b[1] + t[4]*b[2]*z.from + t[4]*b[3]*x)*(z.to-z.from))
}

calc.nie.linear = function(z.from, z.to, b, t) {
  return((t[3]*b[2] + t[4]*b[2]*z.to)*(z.to-z.from))
}

runSimulation = function(S, n, exposure.coefs, mediator.coefs, outcome.coefs, exposure.error.sd = 1, mediator.error.sd = 1, outcome.error.sd = 1) {
  # Storage of results
  effekter.nie <- rep(NA, S) # Estimated NIE 
  effekter.nde <- rep(NA, S) # Estimated NDE 
  CI.nie.lower <- rep(NA, S) # Lower bound of 95% CI of NIE  
  CI.nie.upper <- rep(NA, S) # Upper bound of 95% CI of NIE
  CI.nde.lower <- rep(NA, S) # Lower bound of 95% CI of NDE
  CI.nde.upper <- rep(NA, S) # Upper bound of 95% CI of NDE
  true.NDE <- numeric(S) # True NDE
  nie.S = calc.nie.linear(z.from = 0, z.to = 1, b = mediator.coefs, t = outcome.coefs) # True NIE 
  
  for(i in 1:S){
    if(i %% 50 == 0)
      print(i)
    
    z.epsilon <- rnorm(n, sd = 1) # Error terms for each model. Since z is probit, it should have 1
    m.epsilon <- rnorm(n, sd = mediator.error.sd) 
    y.epsilon <- rnorm(n, sd = outcome.error.sd) 
    
    X <- rgamma(n,shape=8,scale=4.5) # "age" 
    
    # Generate exposure, mediator and outcome (True models):
    Z.star <- exposure.coefs["a0"] + exposure.coefs["a1"]*X + z.epsilon
    Z <- ifelse(Z.star>0, 1, 0)
    M <- mediator.coefs["b0"] + mediator.coefs["b1"]*Z + mediator.coefs["b2"]*X + m.epsilon
    Y <- outcome.coefs["t0"] + outcome.coefs["t1"]*Z + outcome.coefs["t2"]*M + outcome.coefs["t3"]*Z*M + outcome.coefs["t4"]*X + y.epsilon
    
    true.NDE[i] = mean(calc.nde.linear(z.from = 0, z.to = 1, b = mediator.coefs, t = outcome.coefs, x = X))
    
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
  
  return(list(true.nie = nie.S,
              true.nde = true.NDE,
              est.nie = effekter.nie, 
              est.nde = effekter.nde, 
              CI.nie.lower = CI.nie.lower,
              CI.nie.upper = CI.nie.upper,
              CI.nde.lower = CI.nde.lower,
              CI.nde.upper = CI.nde.upper))
}
