library(sensmediation)

calc.nde.linear = function(z.from, z.to, x, b, t) {
  return((t["Z"] + t["ZM"]*b["I"] + t["ZM"]*b["Z"]*z.from + t["ZM"]*b["X"]*x)*(z.to-z.from))
}

calc.nie.linear = function(z.from, z.to, b, t) {
  return((t["M"]*b["Z"] + t["ZM"]*b["Z"]*z.to)*(z.to-z.from))
}


generate.data = function(n,
                         covariate.models = c("gamma"),
                         covariate.parameters = list(c(8, 4.5)),
                         exposure.coefs,
                         mediator.coefs, 
                         outcome.coefs,
                         outcome.mediator.type = "linear",
                         sd.exposure = 1,
                         sd.mediator = 1,
                         sd.outcome = 1) {
  X = matrix(nrow = n, ncol = 0) # all covariates in matrix
  for (x.index in 1:length(covariate.models)) {
    if (covariate.models[x.index] == "gamma") {  
      X = cbind(X, rgamma(n,shape=covariate.parameters[[x.index]][1],scale=covariate.parameters[[x.index]][2]))
    }
  }
  
  z.epsilon <- rnorm(n, sd = sd.exposure) # Error terms for each model. Since z is probit, it should have 1
  m.epsilon <- rnorm(n, sd = sd.mediator) 
  y.epsilon <- rnorm(n, sd = sd.outcome) 
  
  if (outcome.mediator.type == "linear") {
    # Generate exposure, mediator and outcome (True models):
    Z.star <- exposure.coefs["I"] + exposure.coefs["X"]*X + z.epsilon
    Z <- ifelse(Z.star>0, 1, 0)
    M <- mediator.coefs["I"] + mediator.coefs["Z"]*Z + mediator.coefs["X"]*X + m.epsilon
    Y <- outcome.coefs["I"] + outcome.coefs["Z"]*Z + outcome.coefs["M"]*M + outcome.coefs["ZM"]*Z*M + outcome.coefs["X"]*X + y.epsilon
  }
  
  return(data.frame(Z=Z, M=M, Y=Y, X=X))
}


run.simulation = function(iterations = 1000,
                          n = 1000,
                          covariate.models = c("gamma"),
                          covariate.parameters = list(c(8, 4.5)),
                          true.exposure.coefs,
                          true.mediator.coefs, 
                          true.outcome.coefs,
                          outcome.mediator.type = "linear",
                          sd.exposure = 1,
                          sd.mediator = 1,
                          sd.outcome = 1,
                          misspecified.mediator.formula = "Y~M+X", 
                          misspecified.outcome.formula = "Y~Z+M+X") {
  effekter.nie <- rep(NA, iterations) # Estimated NIE 
  effekter.nde <- rep(NA, iterations) # Estimated NDE 
  CI.nie.lower <- rep(NA, iterations) # Lower bound of 95% CI of NIE  
  CI.nie.upper <- rep(NA, iterations) # Upper bound of 95% CI of NIE
  CI.nde.lower <- rep(NA, iterations) # Lower bound of 95% CI of NDE
  CI.nde.upper <- rep(NA, iterations) # Upper bound of 95% CI of NDE
  true.NDE <- numeric(iterations) # True NDE
  true.NIE = ifelse(outcome.mediator.type == "linear",   # True NIE
                    calc.nie.linear(0, 1, b = true.mediator.coefs, t = true.outcome.coefs), 
                    numeric(iterations))
  
  for(i in 1:iterations){
    data = generate.data(n, covariate.models, covariate.parameters, true.exposure.coefs, true.mediator.coefs,
                         true.outcome.coefs,  outcome.mediator.type, sd.exposure, sd.mediator, sd.outcome)
    
    true.NDE[i] = mean(calc.nde.linear(z.from = 0, z.to = 1, b = true.mediator.coefs, t = true.outcome.coefs, x = data[, "X"]))
    
    if (outcome.mediator.type == "linear") {
      # Misspecified models:
      m.model <- glm(misspecified.mediator.formula, data = data) 
      y.model <- glm(misspecified.outcome.formula, data = data) 
    }
    
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
  
  return(list(true.nie = true.NIE,
              true.nde = true.NDE,
              est.nie = effekter.nie, 
              est.nde = effekter.nde, 
              CI.nie.lower = CI.nie.lower,
              CI.nie.upper = CI.nie.upper,
              CI.nde.lower = CI.nde.lower,
              CI.nde.upper = CI.nde.upper))
}















