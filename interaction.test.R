library(sensmediation)

interaction.test.out.p.value = function(data, exp.name = "Z", med.name = "M", out.name = "Y", out.model = "gaussian") {
  out.formula = paste(out.name, "~", paste(c(exp.name, med.name, cov.names, paste(exp.name, "*", med.name, sep = "")), collapse = "+"), sep = "")
  
  if (out.model == "probit") 
    out.model <- glm(out.formula, data = data, family = binomial(link = "probit")) 
  else 
    out.model <- glm(out.formula, data = data) 
  
  return(list(est1, est2))
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
  
  return(list(est1, est2))
}
