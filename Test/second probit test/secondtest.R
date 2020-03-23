library(sensmediation)
library(boot)
library(e1071)

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

boot.diffs = function(data, indices, med.formula, out.formula, out.formula.noint) {
  return(calc.diffs(data[indices, ], med.formula, out.formula, out.formula.noint))
}

calc.diffs = function(dt,  med.formula, out.formula, out.formula.noint) {
  med.model <- glm(med.formula, data = dt, family = binomial(link = "probit")) 
  out.model <- glm(out.formula, data = dt, family = binomial(link = "probit")) 
  out.model.noint <- glm(out.formula.noint, data = dt, family = binomial(link = "probit")) 
  
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
  eff = effects.bb(betas = betas, thetas = thetas, x.med = dt.x, x.out = dt.x, alt.decomposition = F, exp.value = 1, control.value = 0)

  thetas = list()
  thetas$theta0 = coef(out.model.noint)[1]
  thetas$theta1 = coef(out.model.noint)[2]
  thetas$theta2 = coef(out.model.noint)[3]
  thetas$theta3 = 0 # Z*M is always last
  x.indices = !coef(out.model.noint) %in% c(thetas$theta0, thetas$theta1, thetas$theta2, thetas$theta3) 
  thetas$theta4 = matrix(coef(out.model.noint)[x.indices], ncol = 1)
  thetas$theta5 = matrix(data = 0, ncol = 1, nrow = 1)
  thetas$theta6 = matrix(data = 0, ncol = 1, nrow = 1)
  thetas$theta7 = matrix(data = 0, ncol = 1, nrow = 1)
  
  eff.noint = effects.bb(betas = betas, thetas = thetas, x.med = dt.x, x.out = dt.x, alt.decomposition = F, exp.value = 1, control.value = 0)
  
  return(c(NIE.diff = eff[1]-eff.noint[1], NDE.diff = eff[2]-eff.noint[2]))
}

interaction.test.diffs = function(data, exp.name = "Z", med.name = "M", out.name = "Y", cov.names = NULL, med.model.type = "gaussian", out.model.type = "gaussian", R=1000) {
  med.formula = paste(med.name, "~", paste(c(exp.name, cov.names), collapse = "+"), sep = "")
  out.formula = paste(out.name, "~", paste(c(exp.name, med.name, cov.names, paste(exp.name, "*", med.name, sep = "")), collapse = "+"), sep = "")
  out.formula.noint = paste(out.name, "~", paste(c(exp.name, med.name, cov.names), collapse = "+"), sep = "")
  
  res = boot(data=data[, c(exp.name, med.name, out.name, cov.names)],  # from here we know col. order in df is Z, M, Y, X's...
             statistic=boot.diffs, R=R, 
             med.formula=med.formula, out.formula=out.formula, out.formula.noint=out.formula.noint)  
  nie.CI = boot.ci(res, conf = c(0.9, 0.95, 0.99), index=1)
  nde.CI = boot.ci(res, conf = c(0.9, 0.95, 0.99), index=2)
  test.NIE.bca.90 = ifelse(nie.CI$bca[1, 4]<0 & nie.CI$bca[1, 5]>0, 1, 0)
  test.NIE.bca.95 = ifelse(nie.CI$bca[2, 4]<0 & nie.CI$bca[2, 5]>0, 1, 0)
  test.NIE.bca.99 = ifelse(nie.CI$bca[3, 4]<0 & nie.CI$bca[3, 5]>0, 1, 0)
  
  test.NDE.bca.90 = ifelse(nde.CI$bca[1, 4]<0 & nde.CI$bca[1, 5]>0, 1, 0)
  test.NDE.bca.95 = ifelse(nde.CI$bca[2, 4]<0 & nde.CI$bca[2, 5]>0, 1, 0)
  test.NDE.bca.99 = ifelse(nde.CI$bca[3, 4]<0 & nde.CI$bca[3, 5]>0, 1, 0)
  
  t1 = res$t[!is.na(res$t[, 1]), 1] # remove NAs
  t2 = res$t[!is.na(res$t[, 2]), 2] 
  
  p.NIE = min(2*mean(t1 <= 0), 2*mean(t1 >= 0)) # get p-value   
  p.NDE = min(2*mean(t2 <= 0), 2*mean(t2 >= 0))
  # quantile(res$t[!is.na(res$t[, 1]), 1], probs = c(.025, .975), type = 6)
  
  # estimate bias in std. norm deviates
  b.nie=qnorm((sum(t1 < res$t0[1])+sum(t1 == res$t0[1])/2)/R)
  b.nde=qnorm((sum(t2 < res$t0[2])+sum(t2 == res$t0[2])/2)/R)
  
  alpha=0.05 # 95% limits
  z=qnorm(c(alpha/2,1-alpha/2)) # Std. norm. limits
  
  p.nie=pnorm(z+2*b.nie) # bias-correct & convert to proportions
  p.nde=pnorm(z+2*b.nde) # bias-correct & convert to proportions
  
  q.nie = quantile(t1,probs=p.nie) # Bias-corrected percentile lims.
  q.nde = quantile(t2,probs=p.nde) # Bias-corrected percentile lims.
  test.NIE.bc.95 = ifelse(q.nie[1]<0 & q.nie[2]>0, 1, 0)
  test.NDE.bc.95 = ifelse(q.nde[1]<0 & q.nde[2]>0, 1, 0)
  
  diff.nie = res$t0[1]
  diff.nde = res$t0[2]
  
  NIE.SE = sd(t1)
  NDE.SE = sd(t2)
  
  result = matrix(c(diff.nde, diff.nie, NDE.SE, NIE.SE, p.NDE, p.NIE, test.NDE.bca.90, test.NIE.bca.90, test.NDE.bca.95, test.NIE.bca.95, test.NDE.bca.99, test.NIE.bca.99, test.NIE.bc.95, test.NDE.bc.95), nrow = 2, ncol = 7, 
                  dimnames = list(c("NDE", "NIE"), c("est.diff", "SE", "quant.p-value", "bca.90", "bca.95", "bca.99", "bc.95")))
  return(result)
}

# estimate.a.jn(data, func, ...) {
#   n = nrow(data)
#   part.est = vector(mode = "numeric", length = nrow(data))
#   for (i in 1:n) {
#     part.est[i] = func(data[-i, ], ...)
#   }
#   a.sum = sum(mean(part.est) - part.est)
#   a.sum^3/(6*(a.sum^2)^(3/2))
# }




