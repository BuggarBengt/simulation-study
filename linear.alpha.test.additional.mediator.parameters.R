# Alpha test linear

interaction.test.multi.def2 = function(data, exp.name = "Z", med.name = "M", out.name = "Y", cov.names = NULL, med.model = "gaussian", out.model = "gaussian") {
  med.formula = paste(med.name, "~", paste(c(exp.name, cov.names, paste(exp.name, "*", cov.names[1], sep = "")), collapse = "+"), sep = "")
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


# No interaction reality
exp.coefs =  c(I = -0.4, X = 0.01)
med.coefs = c(I = 3, Z = 2, X = 0.05, ZX = -0.03)
out.coefs = c(I = 5, Z = 1, M = 0.5, ZM = 0, X = 0.05, ZX = 0, MX = 0, ZMX = 0)

iter = 4000
n    = 2000
p.values.p.test   = vector(mode = "numeric", length = iter)
p.values.def.test = vector(mode = "numeric", length = iter)
diff.def.test = vector(mode = "numeric", length = iter)
diff.NDE.test = vector(mode = "numeric", length = iter)
diff.NIE.test = vector(mode = "numeric", length = iter)
set.seed(12)
start_time = Sys.time()
for (i in 1:iter) {
  if (i%%10 == 0) {
    print(i/iter) 
  }
  X   = 104 - rgamma(n = n, shape = 8, scale = 4.5)
  Z.s = exp.coefs[1] + exp.coefs[2]*X + rnorm(n = n, mean = 0, sd = 1)
  Z   = ifelse(Z.s>0, 1, 0)
  M = med.coefs[1] + med.coefs[2]*Z + med.coefs[3]*X + med.coefs[4]*Z*X + rnorm(n = n, mean = 0, sd = 1)
  Y = out.coefs[1] + out.coefs[2]*Z + out.coefs[3]*M + out.coefs[5]*X + rnorm(n = n, mean = 0, sd = 1)
  data = data.frame(Y, M, Z, X, X2, X3)
  
  p.values.p.test[i] = interaction.test.out.p.value(data, exp.name = "Z", med.name = "M", out.name = "Y", cov.names = c("X"), out.model = "gaussian")
  def.test = interaction.test.multi.def2(data, exp.name = "Z", med.name = "M", out.name = "Y", cov.names = c("X") ,med.model = "gaussian", out.model = "gaussian")
  diff.def.test[i] = def.test[1, 1]
  p.values.def.test[i] = def.test[1, 3]
}
end_time = Sys.time()
end_time - start_time
hist(p.values.p.test)
hist(p.values.def.test)
plot(density(diff.def.test))
plot(density(diff.NDE.test))
plot(density(diff.NIE.test))
shapiro.test(diff.def.test[1:5000])
shapiro.test(diff.NDE.test[1:5000])
shapiro.test(diff.NIE.test[1:5000])

temp.p.def = p.values.def.test
temp.diff.def = diff.def.test
shapiro.test(temp.diff.def[1:5000])

load(file = "Data/Data-test-tests/additional.mediator.linear.p.values.n.2000.i.4000")
load(file = "Data/Data-test-tests/additional.mediator.linear.diff.n.2000.i.4000")

save(p.values.def.test, file = "Data/Data-test-tests/additional.mediator.linear.p.values.n.2000.i.4000")
save(diff.def.test, file = "Data/Data-test-tests/additional.mediator.linear.diff.n.2000.i.4000")
