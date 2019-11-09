library(sensmediation)

calc.nde.linear = function(z.from, z.to, x, b, t) {
  return((t["Z"] + t["ZM"]*b["I"] + t["ZM"]*b["Z"]*z.from + t["ZM"]*b["X"]*x)*(z.to-z.from))
}

calc.nie.linear = function(z.from, z.to, b, t) {
  return((t["M"]*b["Z"] + t["ZM"]*b["Z"]*z.to)*(z.to-z.from))
}

#Estimate true NIE and true NDE. Use high amount of iterations to reduce SE.
simulate.true.effects = function(n = 1000000,
                                 covariate.models = c("gamma"),
                                 covariate.parameters = list(c(8, 4.5)),
                                 exposure.coefs,
                                 mediator.coefs, 
                                 outcome.coefs,
                                 outcome.mediator.type = "linear",
                                 sd.exposure = 1,
                                 sd.mediator = 1,
                                 sd.outcome = 1) {
  data = generate.data(n, covariate.models, covariate.parameters, exposure.coefs, mediator.coefs,
                       outcome.coefs,  outcome.mediator.type, sd.exposure, sd.mediator, sd.outcome)
  if (outcome.mediator.type == "linear") {
    true.NDE = calc.nde.linear(z.from = 0, z.to = 1, b = mediator.coefs, t = outcome.coefs, x = data[, "X"]) 
    true.NIE = calc.nie.linear(0, 1, b = mediator.coefs, t = outcome.coefs)
  }
  else {
    true.NDE = 0
    true.NIE = 0
  }
  
  return(cbind(true.nie = mean(true.NIE), true.nde = mean(true.NDE)))
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
  
  Z.star <- exposure.coefs["I"] + exposure.coefs["X"]*X + z.epsilon
  Z <- ifelse(Z.star>0, 1, 0)
  if (outcome.mediator.type == "linear") {
    # Generate exposure, mediator and outcome (True models):
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
  est.nie <- rep(NA, iterations) # Estimated NIE 
  est.nde <- rep(NA, iterations) # Estimated NDE 
  SE.nie <- rep(NA, iterations) # SE NIE
  SE.nde <- rep(NA, iterations) # SE NDE
  
  for(i in 1:iterations){
    data = generate.data(n, covariate.models, covariate.parameters, true.exposure.coefs, true.mediator.coefs,
                         true.outcome.coefs,  outcome.mediator.type, sd.exposure, sd.mediator, sd.outcome)

    if (outcome.mediator.type == "linear") {
      # Misspecified models:
      m.model <- glm(misspecified.mediator.formula, data = data) 
      y.model <- glm(misspecified.outcome.formula, data = data) 
    }
    
    # Estimation of effects:
    est <- sensmediation(med.model=m.model, out.model=y.model, Rho=0, progress=FALSE, exp.name = "Z", med.name = "M")
    
    # Storage of results
    est.nie[i] <- est$NIE
    est.nde[i] <- est$NDE
    SE.nie[i] <- est$std.errs$se.nie
    SE.nde[i] <- est$std.errs$se.nde
  }  
  
  return(cbind(est.nie = est.nie, 
               est.nde = est.nde, 
               SE.nie = SE.nie,
               SE.nde = SE.nde))
}

create.data.frame.for.plotting = function(result.summary.NDE, result.summary.NIE, corr.coef) {
  to.plot = matrix(nrow = length(result.summary.NDE), ncol = 9) #preallocate vector to plot
  colnames(to.plot) = c("interaction.coefficient", "true.nde", "true.nie", "est.nde", "est.nie", "nde.emp.SE", "nie.emp.SE", 
                        "nde.coverage", "nie.coverage", "nde.mean.delta.SE", "nie.mean.delta.SE")
  to.plot[, 1] = corr.coef
  for (i in 1:length(result.summary.NDE)) {
    to.plot[i, 2] = result.summary.NDE[[i]]$true
    to.plot[i, 3] = result.summary.NIE[[i]]$true
    to.plot[i, 4] = result.summary.NDE[[i]]$summ[2 ,2]
    to.plot[i, 5] = result.summary.NIE[[i]]$summ[2 ,2]
    to.plot[i, 6] = result.summary.NDE[[i]]$summ[7 ,2]
    to.plot[i, 7] = result.summary.NIE[[i]]$summ[7 ,2]
    to.plot[i, 8] = result.summary.NDE[[i]]$summ[12 ,2]
    to.plot[i, 9] = result.summary.NIE[[i]]$summ[12 ,2]
  }
  return(to.plot)
}













