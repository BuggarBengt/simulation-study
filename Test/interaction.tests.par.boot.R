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

interaction.test.par.boot = function(data, exp.name = "Z", med.name = "M", out.name = "Y", cov.names = NULL, R=1000) {
  data=data[, c(out.name, exp.name, med.name, cov.names)] # from here we know col. order in df is Y, Z, M, X's...
  med.formula = paste(med.name, "~", paste(c(exp.name, cov.names), collapse = "+"), sep = "")
  out.formula = paste(out.name, "~", paste(c(exp.name, med.name, cov.names), collapse = "+"), sep = "")
  med.model <- glm(med.formula, data = data, family = binomial(link = "probit")) 
  # estimate model without interaction, this is the assumed distr. if H0 is true
  out.model <- glm(out.formula, data = data, family = binomial(link = "probit")) 
  
  betas = list() # estimate H0 effect
  betas$beta0 = coef(med.model)[1]
  betas$beta1 = coef(med.model)[2]
  betas$beta2 = matrix(data = coef(med.model)[3], ncol = 1, nrow = 1)
  betas$beta3 = matrix(data = 0, ncol = 1, nrow = 1) 
  thetas = list()
  thetas$theta0 = coef(out.model)[1]
  thetas$theta1 = coef(out.model)[2]
  thetas$theta2 = coef(out.model)[3]
  thetas$theta3 = 0
  x.indices = !coef(out.model) %in% c(thetas$theta0, thetas$theta1, thetas$theta2, thetas$theta3) 
  thetas$theta4 = matrix(coef(out.model)[x.indices], ncol = 1)
  thetas$theta5 = matrix(data = 0, ncol = 1, nrow = 1)
  thetas$theta6 = matrix(data = 0, ncol = 1, nrow = 1)
  thetas$theta7 = matrix(data = 0, ncol = 1, nrow = 1)
  dt.x = as.matrix(data[, -c(1:3), drop=F]) # matrix of covariate-values
  eff1 = effects.bb(betas = betas, thetas = thetas, x.med = dt.x, x.out = dt.x, alt.decomposition = F, exp.value = 1, control.value = 0)
  eff2 = effects.bb(betas = betas, thetas = thetas, x.med = dt.x, x.out = dt.x, alt.decomposition = T, exp.value = 1, control.value = 0)
  est.true.h0 = eff1[1]-eff2[1]
  
  out.formula = paste("newy~", paste(c(exp.name, med.name, cov.names, paste(exp.name, "*", med.name, sep = "")), collapse = "+"), sep = "")
  nie.diff = vector(mode = "numeric", length = R)
  n=nrow(data)
  for (i in 1:R) {
    # estimate new outcomes using no interaction model
    pred = predict(out.model, data, type="response")
    data$newy = rbinom(n, 1, pred+rnorm(n)) # KOLLLA HÄR!!!!
    
    int.out.model <- glm(out.formula, data = data, family = binomial(link = "probit")) 
    
    betas = list()
    betas$beta0 = coef(med.model)[1]
    betas$beta1 = coef(med.model)[2]
    betas$beta2 = matrix(data = coef(med.model)[3], ncol = 1, nrow = 1)
    betas$beta3 = matrix(data = 0, ncol = 1, nrow = 1) # no interactions for Z*X in mediator
    
    thetas = list()
    thetas$theta0 = coef(int.out.model)[1]
    thetas$theta1 = coef(int.out.model)[2]
    thetas$theta2 = coef(int.out.model)[3]
    thetas$theta3 = tail(coef(int.out.model), n=1) # Z*M is always last
    x.indices = !coef(int.out.model) %in% c(thetas$theta0, thetas$theta1, thetas$theta2, thetas$theta3) 
    thetas$theta4 = matrix(coef(int.out.model)[x.indices], ncol = 1)
    thetas$theta5 = matrix(data = 0, ncol = 1, nrow = 1)
    thetas$theta6 = matrix(data = 0, ncol = 1, nrow = 1)
    thetas$theta7 = matrix(data = 0, ncol = 1, nrow = 1)
    
    eff1 = effects.bb(betas = betas, thetas = thetas, x.med = dt.x, x.out = dt.x, alt.decomposition = F, exp.value = 1, control.value = 0)
    eff2 = effects.bb(betas = betas, thetas = thetas, x.med = dt.x, x.out = dt.x, alt.decomposition = T, exp.value = 1, control.value = 0)
    nie.diff[i] = eff1[1]-eff2[1]
  }
  
  p.center = min(2*mean(nie.diff <= 0), 2*mean(nie.diff >= 0))
  p.sample = min(2*mean(nie.diff <= est.true.h0), 2*mean(nie.diff >= est.true.h0))
  return(c(p.center, p.sample, mean(nie.diff), est.true.h0))
}












