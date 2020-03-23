# Compute true probit diff - using true coefs
exp.coefs = c(I = -3.416096, X = 0.036231)
med.coefs = c(I = -1.6507546, Z = 0.2683970, X = 0.0065543, ZX = 0)
out.coefs = c(I = -3.7220626, Z = 0.2763912, M = 1.4729651, ZM = 0, X = 0.0283196, ZX = 0, MX = 0, ZMX = 0)
X   = 104 - rgamma(n = 1000000, shape = 8, scale = 4.5)

betas = list()
betas$beta0 = med.coefs[1]
betas$beta1 = med.coefs[2]
betas$beta2 = matrix(data = med.coefs[3], ncol = 1, nrow = 1)
betas$beta3 = matrix(data = 0, ncol = 1, nrow = 1) # no interactions for Z*X in mediator

thetas = list()
thetas$theta0 = out.coefs[1]
thetas$theta1 = out.coefs[2]
thetas$theta2 = out.coefs[3]
thetas$theta3 = out.coefs[4]
thetas$theta4 = matrix(out.coefs[5], ncol = 1)
thetas$theta5 = matrix(data = 0, ncol = 1, nrow = 1)
thetas$theta6 = matrix(data = 0, ncol = 1, nrow = 1)
thetas$theta7 = matrix(data = 0, ncol = 1, nrow = 1)

dt.x = as.matrix(X) # matrix of covariate-values
eff1 = effects.bb(betas = betas, thetas = thetas, x.med = dt.x, x.out = dt.x, alt.decomposition = F, exp.value = 1, control.value = 0)
eff2 = effects.bb(betas = betas, thetas = thetas, x.med = dt.x, x.out = dt.x, alt.decomposition = T, exp.value = 1, control.value = 0)
true.diff.nie = eff1[1]-eff2[1]
true.diff.nde = eff1[2]-eff2[2]
true.diff.nie
true.diff.nde

# using simulated data when H0 is true
n=1000
iter = 10000
diffs = vector(mode="numeric", length = iter)
nies = vector(mode="numeric", length = iter)
ndes = vector(mode="numeric", length = iter)
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
  out.model <- glm("Y~Z+M+X", data = data, family = binomial(link = "probit")) 
  
  betas = list()
  betas$beta0 = coef(med.model)[1]
  betas$beta1 = coef(med.model)[2]
  betas$beta2 = matrix(data = coef(med.model)[3], ncol = 1, nrow = 1)
  betas$beta3 = matrix(data = 0, ncol = 1, nrow = 1) # no interactions for Z*X in mediator
  
  thetas = list()
  thetas$theta0 = coef(out.model)[1]
  thetas$theta1 = coef(out.model)[2]
  thetas$theta2 = coef(out.model)[3]
  thetas$theta3 = 0 # Z*M is always last
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
hist(diffs, breaks = 30)
sd(diffs)
mean(diffs)

# using bootstrap distr. H0 true
n=1000
iter = 10
R=1000
exp.coefs = c(I = -3.416096, X = 0.036231)
med.coefs = c(I = -1.6507546, Z = 0.2683970, X = 0.0065543, ZX = 0)
out.coefs = c(I = -3.7220626, Z = 0.2763912, M = 1.4729651, ZM = 0, X = 0.0283196, ZX = 0, MX = 0, ZMX = 0)
results =list()
set.seed(123)
for (i in 1:iter) {
  X   = 104 - rgamma(n = n, shape = 8, scale = 4.5)
  Z.s = exp.coefs[1] + exp.coefs[2]*X + rnorm(n = n, mean = 0, sd = 1)
  Z   = ifelse(Z.s>0, 1, 0)
  M.s = med.coefs[1] + med.coefs[2]*Z + med.coefs[3]*X + rnorm(n = n, mean = 0, sd = 1)
  M   = ifelse(M.s>0, 1, 0)
  Y.s = out.coefs[1] + out.coefs[2]*Z + out.coefs[3]*M + out.coefs[4]*M*Z + out.coefs[5]*X + rnorm(n = n, mean = 0, sd = 1)
  Y   = ifelse(Y.s>0, 1, 0)
  data = data.frame(Z, M, Y, X)
  
  med.formula = "M~Z+X"
  out.formula = "Y~Z+M+X+Z*M"
  
  results[[i]] = boot(data=data,
             statistic=boot.multi.def, R=R, med.formula=med.formula, out.formula=out.formula)
  
  print(i/iter) 
}
hist(results[[1]]$t[,1], breaks = 30)
hist(results[[2]]$t[,1], breaks = 30)
hist(results[[3]]$t[,1], breaks = 30)
hist(results[[4]]$t[,1], breaks = 30)
hist(results[[5]]$t[,1], breaks = 30)
sd(results[[1]]$t[,1])
sd(results[[2]]$t[,1])
sd(results[[3]]$t[,1])
sd(results[[4]]$t[,1])
sd(results[[5]]$t[,1])
mean(results[[1]]$t[,1])
mean(results[[2]]$t[,1])
mean(results[[3]]$t[,1])
mean(results[[4]]$t[,1])
mean(results[[10]]$t[,1])




# using simulated data when H0 is false
n=1000
iter = 10000
diffs2 = vector(mode="numeric", length = iter)
exp.coefs = c(I = -3.416096, X = 0.036231)
med.coefs = c(I = -1.6507546, Z = 0.2683970, X = 0.0065543, ZX = 0)
out.coefs = c(I = -3.7220626, Z = 0.2763912, M = 1.4729651, ZM = -0.2583784, X = 0.0283196, ZX = 0, MX = 0, ZMX = 0)

for (i in 1:iter) {
  X   = 104 - rgamma(n = n, shape = 8, scale = 4.5)
  Z.s = exp.coefs[1] + exp.coefs[2]*X + rnorm(n = n, mean = 0, sd = 1)
  Z   = ifelse(Z.s>0, 1, 0)
  M.s = med.coefs[1] + med.coefs[2]*Z + med.coefs[3]*X + rnorm(n = n, mean = 0, sd = 1)
  M   = ifelse(M.s>0, 1, 0)
  Y.s = out.coefs[1] + out.coefs[2]*Z + out.coefs[3]*M + out.coefs[4]*M*Z + out.coefs[5]*X + rnorm(n = n, mean = 0, sd = 1)
  Y   = ifelse(Y.s>0, 1, 0)
  data = data.frame(Y, M, Z, X)
  
  med.model <- glm("M~Z+X", data = data, family = binomial(link = "probit")) 
  out.model <- glm("Y~Z+M+Z*M+X", data = data, family = binomial(link = "probit")) 
  
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
  
  if (i%%10 == 0) {
    print(i/iter) 
  }
}
hist(diffs2, breaks = 30)
mean(diffs2)

t1 = diffs - mean(diffs)
t2 = diffs2 - mean(diffs2)
hist(t1, breaks = 30)
hist(t2, breaks = 30)


















