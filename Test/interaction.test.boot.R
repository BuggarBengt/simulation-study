library(sensmediation)
library(boot)

effects.cc = function (betas, thetas, x.med, x.out, alt.decomposition, exp.value, control.value) 
{
  t.de <- ifelse(alt.decomposition == TRUE, exp.value, control.value)
  t.ie <- ifelse(alt.decomposition == TRUE, control.value, 
                 exp.value)
  diff <- exp.value - control.value
  b0 <- betas$beta0
  b1 <- betas$beta1
  b2 <- betas$beta2
  b3 <- betas$beta3
  th1 <- thetas$theta1
  th2 <- thetas$theta2
  th3 <- thetas$theta3
  th5 <- thetas$theta5
  th6 <- thetas$theta6
  th7 <- thetas$theta7
  med.ie <- b1 * diff + x.med %*% b3 * diff
  med.de <- b0 + b1 * t.de + x.med %*% (b2 + t.de * b3)
  out.ie <- th2 + th3 * t.ie + x.out %*% (th6 + t.ie * 
                                            th7)
  out.de <- th3 * diff + x.out %*% th7 * diff
  NIE <- mean(out.ie * med.ie)
  NDE <- mean(th1 * diff + x.out %*% th5 * diff + out.de * med.de)
  return(c(NIE = NIE, NDE = NDE))
}

effects.bb = function (betas, thetas, x.med, x.out, alt.decomposition, exp.value, control.value) {
  t.de <- ifelse(alt.decomposition == TRUE, exp.value, control.value)
  t.ie <- ifelse(alt.decomposition == TRUE, control.value, exp.value)
    b0 <- betas$beta0
    b1 <- betas$beta1
    b2 <- betas$beta2
    b3 <- betas$beta3
    th0 <- thetas$theta0
    th1 <- thetas$theta1
    th2 <- thetas$theta2
    th3 <- thetas$theta3
    th4 <- thetas$theta4
    th5 <- thetas$theta5
    th6 <- thetas$theta6
    th7 <- thetas$theta7
    probs.med.ie <- stats::pnorm(b0 + b1 * exp.value + x.med %*% 
                    (b2 + b3 * exp.value)) - stats::pnorm(b0 + b1 * 
                    control.value + x.med %*% (b2 + b3 * control.value))
    probs.med.de <- stats::pnorm(b0 + b1 * t.de + x.med %*% 
                    (b2 + b3 * t.de))
    probs.out.ie <- stats::pnorm(th0 + th2 + (th1 + th3) * 
                    t.ie + x.out %*% (th4 + th5 * t.ie + th6 + th7 * t.ie)) - 
                    stats::pnorm(th0 + th1 * t.ie + x.out %*% 
                    (th4 + th5 * t.ie))
    probs.out.de1 <- stats::pnorm(th0 + th1 * exp.value + 
                     x.out %*% (th4 + exp.value * th5)) - stats::pnorm(th0 + 
                     th1 * control.value + x.out %*% (th4 + control.value * th5))
    probs.out.de2 <- stats::pnorm(th0 + th2 + exp.value * 
                     (th1 + th3) + x.out %*% (th4 + exp.value * th5 + 
                     th6 + exp.value * th7)) - stats::pnorm(th0 + th2 + 
                     control.value * (th1 + th3) + x.out %*% (th4 + control.value * 
                                                                                                                                                   th5 + th6 + control.value * th7))
    NIE <- mean(probs.out.ie * probs.med.ie)
    NDE <- mean(probs.out.de1 * (1 - probs.med.de) + 
                       probs.out.de2 * probs.med.de)
  return(c(NIE = NIE, NDE = NDE))
}


boot.multi.def = function(data, indices, med.formula, out.formula, med.model.type = "gaussian", out.model.type = "gaussian") {
  dt<-data[indices, ]

  if (med.model.type == "probit") 
    med.model <- glm(med.formula, data = dt, family = binomial(link = "probit")) 
  else 
    med.model <- glm(med.formula, data = dt) 
  
  if (out.model.type == "probit") 
    out.model <- glm(out.formula, data = dt, family = binomial(link = "probit")) 
  else 
    out.model <- glm(out.formula, data = dt) 
  
  betas = list()
  betas$beta0 = coef(med.model)[1]
  betas$beta1 = coef(med.model)[2]
  betas$beta2 = matrix(data = coef(med.model)[3], ncol = 1, nrow = 1)
  betas$beta3 = matrix(data = 0, ncol = 1, nrow = 1) # no interactions for Z*X in mediator
  
  thetas = list()
  thetas$theta0 = coef(out.model)[1]
  thetas$theta1 = coef(out.model)[2]
  thetas$theta2 = coef(out.model)[3]
  thetas$theta3 = tail(coef(out.model), n=1) # Z*M is always last
  x.indices = !coef(out.model) %in% c(thetas$theta0, thetas$theta1, thetas$theta2, thetas$theta3) 
  thetas$theta4 = matrix(coef(out.model)[x.indices], ncol = 1)
  thetas$theta5 = matrix(data = 0, ncol = 1, nrow = 1)
  thetas$theta6 = matrix(data = 0, ncol = 1, nrow = 1)
  thetas$theta7 = matrix(data = 0, ncol = 1, nrow = 1)
  
  dt.x = as.matrix(dt[, -c(1:3), drop=F]) # matrix of covariate-values
  eff1 = effects.bb(betas = betas, thetas = thetas, x.med = dt.x, x.out = dt.x, alt.decomposition = F, exp.value = 1, control.value = 0)
  eff2 = effects.bb(betas = betas, thetas = thetas, x.med = dt.x, x.out = dt.x, alt.decomposition = T, exp.value = 1, control.value = 0)
  
  return(c(NIE.diff = eff1[1]-eff2[1], NDE.diff = eff1[2]-eff2[2]))
}

interaction.test.multi.def.boot = function(data, exp.name = "Z", med.name = "M", out.name = "Y", cov.names = NULL, med.model.type = "gaussian", out.model.type = "gaussian", R=1000) {
  med.formula = paste(med.name, "~", paste(c(exp.name, cov.names), collapse = "+"), sep = "")
  out.formula = paste(out.name, "~", paste(c(exp.name, med.name, cov.names, paste(exp.name, "*", med.name, sep = "")), collapse = "+"), sep = "")
  
  res = boot(data=data[, c(exp.name, med.name, out.name, cov.names)],  # from here we know col. order in df is Z, M, Y, X's...
             statistic=boot.multi.def, R=R, med.formula=med.formula, out.formula=out.formula, 
             med.model.type=med.model.type, out.model.type=out.model.type)  
  
  # centered.dist.nie = res$t[, 1] - res$t0[1] # move distr. with the true sample diff
  # centered.dist.nde = res$t[, 2] - res$t0[2] 
  centered.dist.nie = res$t[, 1] - mean(res$t[, 1]) + 0.004277872  # move distr. with the true sample diff
  centered.dist.nde = res$t[, 2] - mean(res$t[, 2]) - 0.004277872
  
  p.NIE = min(2*mean(centered.dist.nie <= res$t0[1]), 2*mean(centered.dist.nie >= res$t0[1])) # get p-value   
  p.NDE = min(2*mean(centered.dist.nde <= res$t0[2]), 2*mean(centered.dist.nde >= res$t0[2]))
  
  diff.nie = res$t0[1]
  diff.nde = res$t0[2]
  
  NDE.SE = sd(res$t[, 2])
  NIE.SE = sd(res$t[, 1])
  
  result = matrix(c(diff.nde, diff.nie, NDE.SE, NIE.SE, p.NDE, p.NIE), nrow = 2, ncol = 3, dimnames = list(c("NDE", "NIE"), c("est.diff", "SE", "p-value")))
  return(result)
}

interaction.test.multi.def.boot.multi = function(data, exp.name = "Z", med.name = "M", out.name = "Y", cov.names = NULL, med.model.type = "gaussian", out.model.type = "gaussian", R=1000) {
  med.formula = paste(med.name, "~", paste(c(exp.name, cov.names), collapse = "+"), sep = "")
  out.formula = paste(out.name, "~", paste(c(exp.name, med.name, cov.names, paste(exp.name, "*", med.name, sep = "")), collapse = "+"), sep = "")

  res = boot(data=data[, c(exp.name, med.name, out.name, cov.names)],  # from here we know col. order in df is Z, M, Y, X's...
             statistic=boot.multi.def, R=R, med.formula=med.formula, out.formula=out.formula,
             med.model.type=med.model.type, out.model.type=out.model.type)

  centered.dist = res$t[, 1] - mean(res$t[, 1]) # move distr. with the true sample diff
  centered.dist.sample.diff = res$t[, 1] - res$t0[1] # move distr. with the true sample diff
  centered.dist.nonzero = res$t[, 1] - mean(res$t[, 1]) + 0.004277872
  centered.dist.sample.diff.nonzero = res$t[, 1] - res$t0[1] + 0.004277872
  
  p.center = min(2*mean(centered.dist <= res$t0[1]), 2*mean(centered.dist >= res$t0[1]))
  p.sample = min(2*mean(centered.dist.sample.diff <= res$t0[1]), 2*mean(centered.dist.sample.diff >= res$t0[1]))
  p.center.nonzero = min(2*mean(centered.dist.nonzero <= res$t0[1]), 2*mean(centered.dist.nonzero >= res$t0[1]))
  p.sample.nonzero = min(2*mean(centered.dist.sample.diff.nonzero <= res$t0[1]), 2*mean(centered.dist.sample.diff.nonzero >= res$t0[1]))
  p.conf = min(2*mean(res$t[, 1] <= 0), 2*mean(res$t[, 1] >= 0)) # get p-value
  p.conf.nonzero = min(2*mean(res$t[, 1] <= 0.004277872), 2*mean(res$t[, 1] >= 0.004277872)) # get p-value
  diff = res$t0[1]

  result = c(p.center = p.center, p.sample=p.sample, p.center.nonzero= p.center.nonzero, 
             p.sample.nonzero=p.sample.nonzero, p.conf=p.conf, p.conf.nonzero=p.conf.nonzero, diff=diff)
  return(result)
}

interaction.test.multi.def.cc = function(data, exp.name = "Z", med.name = "M", out.name = "Y", cov.names = NULL, med.model.type = "gaussian", out.model.type = "gaussian") {
  med.formula = paste(med.name, "~", paste(c(exp.name, cov.names), collapse = "+"), sep = "")
  out.formula = paste(out.name, "~", paste(c(exp.name, med.name, cov.names, paste(exp.name, "*", med.name, sep = "")), collapse = "+"), sep = "")
  med.model <- glm(med.formula, data = data) 
  out.model <- glm(out.formula, data = data) 
  
  betas = list()
  betas$beta0 = coef(med.model)[1]
  betas$beta1 = coef(med.model)[2]
  betas$beta2 = matrix(data = coef(med.model)[3], ncol = 1, nrow = 1)
  betas$beta3 = matrix(data = 0, ncol = 1, nrow = 1) # no interactions for Z*X in mediator
  thetas = list()
  thetas$theta0 = coef(out.model)[1]
  thetas$theta1 = coef(out.model)[2]
  thetas$theta2 = coef(out.model)[3]
  thetas$theta3 = tail(coef(out.model), n=1) # Z*M is always last
  x.indices = !coef(out.model) %in% c(thetas$theta0, thetas$theta1, thetas$theta2, thetas$theta3) 
  thetas$theta4 = matrix(coef(out.model)[x.indices], ncol = 1)
  thetas$theta5 = matrix(data = 0, ncol = 1, nrow = 1)
  thetas$theta6 = matrix(data = 0, ncol = 1, nrow = 1)
  thetas$theta7 = matrix(data = 0, ncol = 1, nrow = 1)
  
  dt.x = as.matrix(data[, -c(1:3), drop=F]) # matrix of covariate-values
  eff1 = effects.cc(betas = betas, thetas = thetas, x.med = dt.x, x.out = dt.x, alt.decomposition = F, exp.value = 1, control.value = 0)
  eff2 = effects.cc(betas = betas, thetas = thetas, x.med = dt.x, x.out = dt.x, alt.decomposition = T, exp.value = 1, control.value = 0)
  diff = eff1[1]-eff2[1]
  t3 = summary(out.model)$coefficients["Z:M", 1]
  b1 = summary(med.model)$coefficients["Z", 1]
  v.t3 = summary(out.model)$coefficients["Z:M", 2]^2
  v.b1 =  summary(med.model)$coefficients["Z", 2]^2
  var.diff = v.t3*v.b1 + b1^2*v.t3 + t3^2*v.b1 # se uppsats
  pv = pnorm(abs(diff), mean = 0, sd = sqrt(var.diff), lower.tail = F)*2
  return(c(diff = diff, SE=sqrt(var.diff), p.value = pv))
}




