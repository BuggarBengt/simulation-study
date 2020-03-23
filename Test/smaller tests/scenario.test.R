# using simulated data when H0 is true
n=500
iter = 10000
diffs = vector(mode="numeric", length = iter)
nies = vector(mode="numeric", length = iter)
ndes = vector(mode="numeric", length = iter)
exp.coefs = c(I = -4, X = 0.04)
med.coefs = c(I = -0.5, Z = 0.5, X = 0.01, ZX = 0)
out.coefs = c(I = -2, Z = 1, M = 1, ZM = 0, X = 0.02, ZX = 0, MX = 0, ZMX = 0)
set.seed(123)
for (i in 1:iter) {
  X   = 104 - rgamma(n = n, shape = 8, scale = 4.5)
  Z.s = exp.coefs[1] + exp.coefs[2]*X + rnorm(n = n, mean = 0, sd = 1)
  Z   = ifelse(Z.s>0, 1, 0)
  M.s = med.coefs[1] + med.coefs[2]*Z + med.coefs[3]*X + rnorm(n = n, mean = 0, sd = 1)
  M   = ifelse(M.s>0, 1, 0)
  Y.s = out.coefs[1] + out.coefs[2]*Z + out.coefs[3]*M + out.coefs[4]*Z*M + out.coefs[5]*X + rnorm(n = n, mean = 0, sd = 1)
  Y   = ifelse(Y.s>0, 1, 0)
  data = data.frame(Y, M, Z, X)
  
  med.model <- glm("M~Z+X", data = data, family = binomial(link = "probit")) 
  out.model <- glm("Y~Z+M+X+Z*M", data = data, family = binomial(link = "probit")) 
  
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
  
  dt.x = as.matrix(data[, "X", drop=F]) # matrix of covariate-values
  eff1 = effects.bb(betas = betas, thetas = thetas, x.med = dt.x, x.out = dt.x, alt.decomposition = F, exp.value = 1, control.value = 0)
  eff2 = effects.bb(betas = betas, thetas = thetas, x.med = dt.x, x.out = dt.x, alt.decomposition = T, exp.value = 1, control.value = 0) 
  diffs[i] = eff1[1]-eff2[1]
  nies[i] = eff1[1]
  ndes[i] = eff1[2]

  if (i%%10 == 0) {
    print(i/iter) 
  }
}
hist(diffs, breaks = 40)
hist(nies, breaks = 40)
hist(ndes, breaks = 40)
sd(diffs)
mean(diffs)

# using simulated data when H0 is true
n=500
iter = 10000
diffs2 = vector(mode="numeric", length = iter)
nies2 = vector(mode="numeric", length = iter)
ndes2 = vector(mode="numeric", length = iter)
exp.coefs = c(I = -3.416096, X = 0.036231)
med.coefs = c(I = -1.6507546, Z = 0.2683970, X = 0.0065543, ZX = 0)
out.coefs = c(I = -3.7220626, Z = 0.2763912, M = 1.4729651, ZM = 0, X = 0.0283196, ZX = 0, MX = 0, ZMX = 0)
set.seed(123)
for (i in 1:iter) {
  X   = 104 - rgamma(n = n, shape = 8, scale = 4.5)
  Z.s = exp.coefs[1] + exp.coefs[2]*X + rnorm(n = n, mean = 0, sd = 1)
  Z   = ifelse(Z.s>0, 1, 0)
  M.s = med.coefs[1] + med.coefs[2]*Z + med.coefs[3]*X + rnorm(n = n, mean = 0, sd = 1)
  M   = ifelse(M.s>0, 1, 0)
  Y.s = out.coefs[1] + out.coefs[2]*Z + out.coefs[3]*M + out.coefs[5]*X + rnorm(n = n, mean = 0, sd = 1)
  Y   = ifelse(Y.s>0, 1, 0)
  data = data.frame(Y, M, Z, X)
  
  med.model <- glm("M~Z+X", data = data, family = binomial(link = "probit")) 
  out.model <- glm("Y~Z+M+X+Z*M", data = data, family = binomial(link = "probit")) 
  
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
  
  dt.x = as.matrix(data[, "X", drop=F]) # matrix of covariate-values
  eff1 = effects.bb(betas = betas, thetas = thetas, x.med = dt.x, x.out = dt.x, alt.decomposition = F, exp.value = 1, control.value = 0)
  eff2 = effects.bb(betas = betas, thetas = thetas, x.med = dt.x, x.out = dt.x, alt.decomposition = T, exp.value = 1, control.value = 0) 
  diffs2[i] = eff1[1]-eff2[1]
  nies2[i] = eff1[1]
  ndes2[i] = eff1[2]
  if (i%%10 == 0) {
    print(i/iter) 
  }
}
hist(diffs2, breaks = 40)
hist(ndes2, breaks = 40)
hist(nies2, breaks = 40)
sd(diffs2)
mean(diffs2)




# using simulated data when H0 is true
n=500
iter = 10000
diffs3 = vector(mode="numeric", length = iter)
nies3 = vector(mode="numeric", length = iter)
ndes3 = vector(mode="numeric", length = iter)
exp.coefs = c(I = -4, X = 0.04)
med.coefs = c(I = -2, Z = 2, X = 0.02, ZX = 0)
out.coefs = c(I = -3, Z = 3, M = 3, ZM = 0, X = 0.02, ZX = 0, MX = 0, ZMX = 0)
set.seed(123)
for (i in 1:iter) {
  X   = 104 - rgamma(n = n, shape = 8, scale = 4.5)
  Z.s = exp.coefs[1] + exp.coefs[2]*X + rnorm(n = n, mean = 0, sd = 1)
  Z   = ifelse(Z.s>0, 1, 0)
  M.s = med.coefs[1] + med.coefs[2]*Z + med.coefs[3]*X + rnorm(n = n, mean = 0, sd = 1)
  M   = ifelse(M.s>0, 1, 0)
  Y.s = out.coefs[1] + out.coefs[2]*Z + out.coefs[3]*M + out.coefs[4]*Z*M + out.coefs[5]*X + rnorm(n = n, mean = 0, sd = 1)
  Y   = ifelse(Y.s>0, 1, 0)
  data = data.frame(Y, M, Z, X)
  
  med.model <- glm("M~Z+X", data = data, family = binomial(link = "probit")) 
  out.model <- glm("Y~Z+M+X+Z*M", data = data, family = binomial(link = "probit")) 
  
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
  
  dt.x = as.matrix(data[, "X", drop=F]) # matrix of covariate-values
  eff1 = effects.bb(betas = betas, thetas = thetas, x.med = dt.x, x.out = dt.x, alt.decomposition = F, exp.value = 1, control.value = 0)
  eff2 = effects.bb(betas = betas, thetas = thetas, x.med = dt.x, x.out = dt.x, alt.decomposition = T, exp.value = 1, control.value = 0) 
  diffs3[i] = eff1[1]-eff2[1]
  nies3[i] = eff1[1]
  ndes3[i] = eff1[2]
  if (i%%10 == 0) {
    print(i/iter) 
  }
}
hist(diffs3, breaks = 30)
sd(diffs3)
mean(diffs3)




# using simulated data when H0 is true
n=500
iter = 10000
diffs = vector(mode="numeric", length = iter)
nies = vector(mode="numeric", length = iter)
ndes = vector(mode="numeric", length = iter)
nies.def2 = vector(mode="numeric", length = iter)
ndes.def2 = vector(mode="numeric", length = iter)
nies.noint = vector(mode="numeric", length = iter)
ndes.noint = vector(mode="numeric", length = iter)
nde.diffs = vector(mode="numeric", length = iter)
nie.diffs = vector(mode="numeric", length = iter)
exp.coefs = c(I = -4, X = 0.04)
med.coefs = c(I = -0.5, Z = 0.5, X = 0.01, ZX = 0)
out.coefs = c(I = -2, Z = 1, M = 1, ZM = 0, X = 0.02, ZX = 0, MX = 0, ZMX = 0)
set.seed(123)
for (i in 1:iter) {
  X   = 104 - rgamma(n = n, shape = 8, scale = 4.5)
  Z.s = exp.coefs[1] + exp.coefs[2]*X + rnorm(n = n, mean = 0, sd = 1)
  Z   = ifelse(Z.s>0, 1, 0)
  M.s = med.coefs[1] + med.coefs[2]*Z + med.coefs[3]*X + rnorm(n = n, mean = 0, sd = 1)
  M   = ifelse(M.s>0, 1, 0)
  Y.s = out.coefs[1] + out.coefs[2]*Z + out.coefs[3]*M + out.coefs[4]*Z*M + out.coefs[5]*X + rnorm(n = n, mean = 0, sd = 1)
  Y   = ifelse(Y.s>0, 1, 0)
  data = data.frame(Y, M, Z, X)
  
  med.model <- glm("M~Z+X", data = data, family = binomial(link = "probit")) 
  out.model <- glm("Y~Z+M+X+Z*M", data = data, family = binomial(link = "probit")) 
  
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
  
  dt.x = as.matrix(data[, "X", drop=F]) # matrix of covariate-values
  eff1 = effects.bb(betas = betas, thetas = thetas, x.med = dt.x, x.out = dt.x, alt.decomposition = F, exp.value = 1, control.value = 0)
  eff2 = effects.bb(betas = betas, thetas = thetas, x.med = dt.x, x.out = dt.x, alt.decomposition = T, exp.value = 1, control.value = 0) 
  diffs[i] = eff1[1]-eff2[1]
  nies[i] = eff1[1]
  ndes[i] = eff1[2]
  nies.def2[i] = eff2[1]
  ndes.def2[i] = eff2[2]
  
  out.model <- glm("Y~Z+M+X", data = data, family = binomial(link = "probit")) # no interaction for nie-nide.noint
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
  
  dt.x = as.matrix(data[, "X", drop=F]) # matrix of covariate-values
  eff1 = effects.bb(betas = betas, thetas = thetas, x.med = dt.x, x.out = dt.x, alt.decomposition = F, exp.value = 1, control.value = 0)
  nies.noint[i] = eff1[1]
  ndes.noint[i] = eff1[2]
  nie.diffs[i] = nies[i] - eff1[1] 
  nde.diffs[i] = ndes[i] - eff1[2]
  
  if (i%%10 == 0) {
    print(i/iter) 
  }
}
hist(diffs, breaks = 50)
hist(nie.diffs, breaks = 30)
hist(nde.diffs, breaks = 30)
shapiro.test(nde.diffs[1:3000])
mean(nie.diffs)
mean(nde.diffs)
sd(nde.diffs)
sd(nie.diffs)
mean(diffs)


hist(nies, breaks = 30)
hist(ndes, breaks = 30)
plot(ndes.noint, ndes)
plot(nies.noint, nies)
plot(nies, ndes)
plot(nies.def2, nies)
plot(ndes.def2, ndes)
















library(ggplot2)
ggplot(data, aes(x=X, y=Z)) + 
  geom_point() + 
  stat_smooth(method="glm", method.args=list(family=binomial(link="logit"))) +
  theme_bw()





















