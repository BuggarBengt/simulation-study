library(sensmediation)

interaction.test.out.p.value = function(data, exp.name = "Z", med.name = "M", out.name = "Y", cov.names = NULL, out.model = "gaussian") {
  out.formula = paste(out.name, "~", paste(c(exp.name, med.name, cov.names, paste(exp.name, "*", med.name, sep = "")), collapse = "+"), sep = "")
  
  if (out.model == "probit") 
    out.model <- glm(out.formula, data = data, family = binomial(link = "probit")) 
  else 
    out.model <- glm(out.formula, data = data) 
  p.value = tryCatch(
    expr = {
      coefficients(summary(out.model))[paste(exp.name, ":", med.name, sep = ""), 4]
    },
    error = function(e){ # return higest possible p-value of no interaction term was found
      return(1)
    }
  )
  return(p.value) # return p-value for interaction coefficient
}


interaction.test.multi.def = function(data, exp.name = "Z", med.name = "M", out.name = "Y", cov.names = NULL, med.model = "gaussian", out.model = "gaussian") {
  med.formula = paste(med.name, "~", paste(c(exp.name, cov.names), collapse = "+"), sep = "")
  out.formula = paste(out.name, "~", paste(c(exp.name, med.name, cov.names, paste(exp.name, "*", med.name, sep = "")), collapse = "+"), sep = "")
  
  if (med.model == "probit") 
    med.model <- glm(med.formula, data = data, family = binomial(link = "probit")) 
  else 
    med.model <- glm(med.formula, data = data) 
  
  if (out.model == "probit") 
    out.model <- glm(out.formula, data = data, family = binomial(link = "probit")) 
  else 
    out.model <- glm(out.formula, data = data) 
  
  est1 <- sensmediation(med.model=med.model, out.model=out.model, exp.name = "Z", med.name = "M")
  est2 <- sensmediation(med.model=med.model, out.model=out.model, exp.name = "Z", med.name = "M", alt.decomposition = T)
  
  diff.nde = est1$NDE-est2$NDE # differences, should be close to zero if interaction is 0
  diff.nie = est1$NIE-est2$NIE
  
  # We need SE of est1$NDE-est2$NDE
  # Derivaties for NDE-NDE/NIE-NIE. Since deriv. of A-B = A'-B' we get:
  part.derivs.NDE = est1$part.deriv$`0`$Lambda - est2$part.deriv$`0`$Lambda
  part.derivs.NIE = est1$part.deriv$`0`$Gamma - est2$part.deriv$`0`$Gamma
  
  # Get standard error by delta method. %*% for matrix multipl.
  NDE.SE = sqrt(part.derivs.NDE %*% est1$sigma.thetabeta$`0` %*% part.derivs.NDE)
  NIE.SE = sqrt(part.derivs.NIE %*% est1$sigma.thetabeta$`0` %*% part.derivs.NIE)
  
  p.NDE = pnorm(abs(diff.nde), mean = 0, sd = NDE.SE, lower.tail = F)*2 # two-tailed test -> *2
  p.NIE = pnorm(abs(diff.nie), mean = 0, sd = NIE.SE, lower.tail = F)*2
  
  result = matrix(c(diff.nde, diff.nie, NDE.SE, NIE.SE, p.NDE, p.NIE), nrow = 2, ncol = 3, dimnames = list(c("NDE", "NIE"), c("est.diff", "SE", "p-value")))
  
  return(result)
}

interaction.test.est.comp = function(data, exp.name = "Z", med.name = "M", out.name = "Y", cov.names = NULL, med.model = "gaussian", out.model = "gaussian") {
  med.formula = paste(med.name, "~", paste(c(exp.name, cov.names), collapse = "+"), sep = "")
  out.formula.no.int = paste(out.name, "~", paste(c(exp.name, med.name, cov.names), collapse = "+"), sep = "")
  out.formula.int = paste(out.name, "~", paste(c(exp.name, med.name, cov.names, paste(exp.name, "*", med.name, sep = "")), collapse = "+"), sep = "")
  
  
  if (med.model == "probit") 
    med.model <- glm(med.formula, data = data, family = binomial(link = "probit")) 
  else 
    med.model <- glm(med.formula, data = data) 
  
  if (out.model == "probit") {
    out.model.no.int <- glm(out.formula.no.int, data = data, family = binomial(link = "probit")) 
    out.model.int <- glm(out.formula.int, data = data, family = binomial(link = "probit")) 
  }
  else {
    out.model.no.int <- glm(out.formula.no.int, data = data) 
    out.model.int <- glm(out.formula.int, data = data) 
  }
  
  est.no.int <- sensmediation(med.model=med.model, out.model=out.model.no.int, exp.name = "Z", med.name = "M")
  est.int <- sensmediation(med.model=med.model, out.model=out.model.int, exp.name = "Z", med.name = "M")
  
  diff.nde = est.no.int$NDE-est.int$NDE # differences, should be close to zero if interaction is 0
  diff.nie = est.no.int$NIE-est.int$NIE
  
  # We need SE of est1$NDE-est2$NDE
  # Derivaties for NDE-NDE/NIE-NIE. Since deriv. of A-B = A'-B' we get:
  part.derivs.NDE = est.no.int$part.deriv$`0`$Lambda - est.int$part.deriv$`0`$Lambda # Funkar inte vi måste skilja på parametrarna här
  part.derivs.NIE = est.no.int$part.deriv$`0`$Gamma - est.int$part.deriv$`0`$Gamma
  
  # Funkar inte är, vi har 2 sigmathetabeta
  # NDE.SE = sqrt(part.derivs.NDE %*% est1$sigma.thetabeta$`0` %*% part.derivs.NDE) 
  # NIE.SE = sqrt(part.derivs.NIE %*% est1$sigma.thetabeta$`0` %*% part.derivs.NIE)
  
  # p.NDE = pnorm(abs(diff.nde), mean = 0, sd = NDE.SE, lower.tail = F)*2 # two-tailed test -> *2
  # p.NIE = pnorm(abs(diff.nie), mean = 0, sd = NIE.SE, lower.tail = F)*2
  
  result = c(nde = diff.nde, nie = diff.nie)
  
  return(result)
}
