library(sensmediation)
library(mvtnorm)

calc.nde.linear = function(z.from, z.to, x, b, t) {
  return((t["Z"] + t["ZM"]*b["I"] + t["ZM"]*b["Z"]*z.from + t["ZM"]*b["X"]*x)*(z.to-z.from))
}

calc.nie.linear = function(z.from, z.to, b, t) {
  return((t["M"]*b["Z"] + t["ZM"]*b["Z"]*z.to)*(z.to-z.from))
}

calc.nde.probit = function(x, b, t) { #ekv. 11 in article
  first.term = (pnorm(t["I"] + t["Z"] + (t["X"] + t["ZX"])*x) - pnorm(t["I"] + t["X"] * x)) * (1 - pnorm(b["I"] + b["X"] * x))
  second.term = (pnorm(t["I"] + t["Z"] + t["M"] + t["ZM"] + (t["X"] + t["ZX"] + t["MX"] + t["ZMX"]) * x) - pnorm(t["I"] + t["M"] + (t["X"] + t["MX"])*x)) * pnorm(b["I"] + b["X"] * x)
  return(first.term+second.term)
}

calc.nie.probit = function(x, b, t) { #ekv. 12 in article
  first.factor = pnorm(t["I"] + t["Z"] + t["M"] + t["ZM"] + (t["X"] + t["ZX"] + t["MX"] + t["ZMX"]) * x) - pnorm(t["I"] + t["Z"] + (t["X"] + t["ZX"])*x)
  second.factor = pnorm(b["I"] + b["Z"] + (b["X"] + b["ZX"]) * x) - pnorm(b["I"] + b["X"] * x)
  return(first.factor * second.factor)
}

#Estimate true NIE and true NDE. Use high amount of iterations to reduce SE.
simulate.true.effects = function(n = 1000000,
                                 covariate.models = c("gamma"),
                                 covariate.parameters = list(c(8, 4.5)),
                                 exposure.coefs,
                                 mediator.coefs, 
                                 outcome.coefs,
                                 outcome.mediator.type = "linear",
                                 mediator.outcome.corr = 0,
                                 sd.exposure = 1,
                                 sd.mediator = 1,
                                 sd.outcome = 1) {
  data = generate.data(n, covariate.models, covariate.parameters, exposure.coefs, mediator.coefs,
                       outcome.coefs,  outcome.mediator.type, mediator.outcome.corr, sd.exposure, sd.mediator, sd.outcome)
  if (outcome.mediator.type == "linear") {
    true.NDE = calc.nde.linear(z.from = 0, z.to = 1, b = mediator.coefs, t = outcome.coefs, x = data[, "X"]) 
    true.NIE = calc.nie.linear(z.from = 0, z.to = 1, b = mediator.coefs, t = outcome.coefs)
  }
  else {
    true.NDE = calc.nde.probit(b = mediator.coefs, t = outcome.coefs, x = data[, "X"]) 
    true.NIE = calc.nie.probit(b = mediator.coefs, t = outcome.coefs, x = data[, "X"])
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
                         mediator.outcome.corr = 0,
                         sd.exposure = 1,
                         sd.mediator = 1,
                         sd.outcome = 1) {
  X = matrix(nrow = n, ncol = 0) # all covariates in matrix
  for (x.index in 1:length(covariate.models)) {
    if (covariate.models[x.index] == "gamma") {  
      X = cbind(X, rgamma(n,shape=covariate.parameters[[x.index]][1],scale=covariate.parameters[[x.index]][2]))
    }
    if (covariate.models[x.index] == "x-gamma") {  
      X = cbind(X, covariate.parameters[[x.index]][1]-rgamma(n,shape=covariate.parameters[[x.index]][2],scale=covariate.parameters[[x.index]][3]))
    }
  }
  
  z.epsilon <- rnorm(n, sd = sd.exposure) # Error terms for each model. Since z is probit, it should have 1
  corr.matrix <- cbind(c(sd.mediator,mediator.outcome.corr),c(mediator.outcome.corr,sd.outcome)) #correlation matrix for error terms 
  epsilon <- rmvnorm(n,sigma=corr.matrix) # Correlated error terms, mediator and outcome models

  Z.star <- exposure.coefs["I"] + exposure.coefs["X"]*X + z.epsilon
  Z <- ifelse(Z.star>0, 1, 0)
  if (outcome.mediator.type == "linear") {
    # Generate exposure, mediator and outcome (True models):
    M <- mediator.coefs["I"] + mediator.coefs["Z"]*Z + mediator.coefs["X"]*X + mediator.coefs["ZX"]*Z*X + epsilon[, 1]
    Y <- outcome.coefs["I"] + outcome.coefs["Z"]*Z + outcome.coefs["M"]*M + outcome.coefs["ZM"]*Z*M + 
      outcome.coefs["X"]*X + outcome.coefs["ZX"]*Z*X + outcome.coefs["MX"]*Z*X + outcome.coefs["ZMX"]*Z*X+ epsilon[, 2]
  }
  if (outcome.mediator.type == "probit") {
    # Generate exposure, mediator and outcome (True models):
    M.star <- mediator.coefs["I"] + mediator.coefs["Z"]*Z + mediator.coefs["X"]*X + mediator.coefs["ZX"]*Z*X + epsilon[, 1]
    M <- ifelse(M.star > 0, 1, 0)
    Y.star <- outcome.coefs["I"] + outcome.coefs["Z"]*Z + outcome.coefs["M"]*M + outcome.coefs["ZM"]*Z*M + 
      outcome.coefs["X"]*X + outcome.coefs["ZX"]*Z*X + outcome.coefs["MX"]*Z*X + outcome.coefs["ZMX"]*Z*X+ epsilon[, 2]
    Y <- ifelse(Y.star > 0, 1, 0)
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
                          mediator.outcome.corr = 0,
                          sd.exposure = 1,
                          sd.mediator = 1,
                          sd.outcome = 1,
                          misspecified.mediator.formula = "Y~M+X", 
                          misspecified.outcome.formula = "Y~Z+M+X",
                          misspecified.outcome.mediator.corr = 0) {
  est.nie <- rep(NA, iterations) # Estimated NIE 
  est.nde <- rep(NA, iterations) # Estimated NDE 
  SE.nie <- rep(NA, iterations) # SE NIE
  SE.nde <- rep(NA, iterations) # SE NDE
  
  for(i in 1:iterations){
    data = generate.data(n, covariate.models, covariate.parameters, true.exposure.coefs, true.mediator.coefs,
                         true.outcome.coefs,  outcome.mediator.type, mediator.outcome.corr, sd.exposure, sd.mediator, sd.outcome)

    if (outcome.mediator.type == "linear") {
      # Misspecified models:
      m.model <- glm(misspecified.mediator.formula, data = data) 
      y.model <- glm(misspecified.outcome.formula, data = data) 
    }
    if (outcome.mediator.type == "probit") {
      # Misspecified models:
      m.model <- glm(misspecified.mediator.formula, data = data, family=binomial(link='probit')) 
      y.model <- glm(misspecified.outcome.formula, data = data, family=binomial(link='probit')) 
    }
    
    # Estimation of effects:
    est <- sensmediation(med.model=m.model, out.model=y.model, Rho=misspecified.outcome.mediator.corr, progress=FALSE, exp.name = "Z", med.name = "M")
    
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
  to.plot = matrix(nrow = length(result.summary.NDE), ncol = 13) #preallocate vector to plot
  colnames(to.plot) = c("interaction.coefficient", "nde.true", "nde.est", "nde.bias", "nde.emp.SE", "nde.model.SE", "nde.coverage",
                    "nie.true", "nie.est", "nie.bias", "nie.emp.SE", "nie.model.SE", "nie.coverage")
  to.plot[, 1] = corr.coef
  for (i in 1:length(result.summary.NDE)) {
    to.plot[i, 2] = result.summary.NDE[[i]]$true
    to.plot[i, 3] = result.summary.NDE[[i]]$summ[2 ,2]
    to.plot[i, 4] = to.plot[i, 3] - to.plot[i, 2]
    to.plot[i, 5] = result.summary.NDE[[i]]$summ[7 ,2]
    to.plot[i, 6] = result.summary.NDE[[i]]$summ[10 ,2]
    to.plot[i, 7] = result.summary.NDE[[i]]$summ[12 ,2]
    
    to.plot[i, 8] = result.summary.NIE[[i]]$true
    to.plot[i, 9] = result.summary.NIE[[i]]$summ[2 ,2]
    to.plot[i, 10] = to.plot[i, 9] - to.plot[i, 8]
    to.plot[i, 11] = result.summary.NIE[[i]]$summ[7 ,2]
    to.plot[i, 12] = result.summary.NIE[[i]]$summ[10 ,2]
    to.plot[i, 13] = result.summary.NIE[[i]]$summ[12 ,2]
  }
  return(to.plot)
}

create.data.frame.for.plotting.confounding = function(interaction.coef, result.summaries.NDE, result.summaries.NIE, rhos) {
  res.matrix = matrix(NA, nrow = 0, ncol = 14)
  for (i in 1:length(result.summaries.NDE)) {
    mat = cbind(i.coef = interaction.coef[i], create.data.frame.for.plotting(result.summaries.NDE[[i]], result.summaries.NIE[[i]], rhos))
    res.matrix = rbind(res.matrix, mat)
  }
  return(res.matrix)
}








